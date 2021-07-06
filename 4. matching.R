#4. matching movers and non-movers
source("2. data cleaning and maniputation.R") #importing data and packages

#### identifying our movers and non-movers ####

movers_to_USA_id_list <- movers_dataset_final %>% distinct(cluster_id)
never_movers_id_list <- stayers_dataset %>% distinct(cluster_id)

#### here I make a dataframe that contains basic information about all of our movers and stayers ####

researcher_basic_info <- final_complete_dataset %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>% 
  mutate(origin_country = first(pub_country), #new variable: country of origin
         end_of_career_year = last(career_year)) %>% #new variable: career length in years
  left_join(read_csv("data/country_list.csv", col_names =T), by = c("origin_country" = "country")) %>% 
  select(cluster_id, discipline,specialty, full_name, first_name, first_year, last_year, end_of_career_year,gender, gender_accuracy, origin_country,origin_institution, origin_region = region, final_article_at_origin_year) %>% 
  distinct(cluster_id, .keep_all = T) %>% #only one row per cluster_id
  mutate(moves_to_usa = if_else(cluster_id %in% movers_to_USA_id_list$cluster_id, TRUE, FALSE), #new variable: does this researcher move to the USA?
         stays_at_origin_country = if_else(cluster_id %in% never_movers_id_list$cluster_id, TRUE, FALSE)) %>%  #new variable: does this reseearcher stay in origin country?
  filter(stays_at_origin_country == TRUE | moves_to_usa == TRUE) %>% #this is when this dataframe only becomes our movers and potential matches (nb: controls can still have careers <24 months)
  left_join(movers_dataset_final %>% 
              select(cluster_id, years_between_starting_and_moving, USA_institution, difference_in_qs_overall_score,difference_in_qs_reputation_score,difference_in_qs_overall_ranking_quantile, origin_qs_overall_score_mean, origin_qs_reputation_score_mean, origin_qs_overall_rank_quartiles, USAinstitute_qs_overall_score_mean, USAinstitute_qs_reputation_score_mean,USAinstitute_qs_overall_rank_quartiles,origin_pp_top10_mean_quantile,USAinstitute_pp_top10_mean_quantile,difference_in_pptop10,difference_in_pptop10_quantile) %>% 
              distinct(cluster_id, .keep_all = T)) %>% #new variable: in this join I add a column which represents the month in which the movers move
  mutate(origin_or_usa = if_else(stays_at_origin_country == TRUE, "origin", "usa")) %>% #new variable: here I make a single variable stating the "condition" of the researcher
  select(-stays_at_origin_country, -moves_to_usa, -first_name) %>%  #neatening up df
  ungroup()

######################################## 
###### PREPERATION FOR MATCHING ########
######################################## 

#step 1. making a dataframe of the movers
movers <- researcher_basic_info %>%
  filter(origin_or_usa == "usa") %>% 
  select(gender, discipline,specialty, origin_country, origin_institution, everything()) %>% 
  rename_with(~paste0(., "_movers"), .cols = cluster_id:origin_or_usa) #renaming the columns to make joining DFs with "movers" easier later

# step 2. adding mover performance before move
year_before_move <- movers %>% mutate(career_year = years_between_starting_and_moving_movers-1) %>%  select(cluster_id_movers, career_year) #calculating which month counts as the "month before move" (actually it's half a month = 0.5)

cumulative_researcher_performance_years_quantiles <- cumulative_researcher_performance_years %>% 
  left_join(researcher_basic_info %>% select(cluster_id, discipline, specialty), by = "cluster_id") %>% 
  select(discipline, specialty, cluster_id_movers = cluster_id, career_year, p_full_cumsum, int_collab_cumsum, n_coauthors_cummedian, njs_full_cummean) %>% 
  group_by(specialty, career_year) %>% 
  mutate(p_full_quantile = ntile(p_full_cumsum, 5),
         int_collab_quantile = ntile(int_collab_cumsum, 5),
         n_coauthors_quantile = ntile(n_coauthors_cummedian, 5),
         njs_full_quantile = ntile(njs_full_cummean, 5)) %>% 
  ungroup() %>%  #calculating quantiles for performance in all cluster_ids.
  select(-ends_with(c("cumsum","cummean", "cummedian")))

movers_performance_before_move <- left_join(year_before_move, cumulative_researcher_performance_years_quantiles, by =c("cluster_id_movers","career_year")) %>% rename(year_before_move_movers = career_year, p_full_quantile_movers = p_full_quantile, int_collab_quantile_movers = int_collab_quantile, n_coauthors_quantile_movers = n_coauthors_quantile, njs_full_quantile_movers = njs_full_quantile) %>% select(-discipline,-specialty) #here I get performance decile just for the month before the move for all "movers
movers2 <- movers %>% left_join(movers_performance_before_move, by = "cluster_id_movers") %>% select(-first_year_movers,-full_name_movers,-end_of_career_year_movers, -final_article_at_origin_year_movers) # here i add back in important demographic info (like origin institution etc).

# step 3. making a dataframe with stayers performance. 
stayers <- researcher_basic_info %>% # take the basic df
  select(gender, discipline,specialty, origin_country, origin_institution, everything()) %>% 
  filter(origin_or_usa == "origin") %>% #keep only our "stayers"
  rename_with(~paste0(., "_stayers"), .cols = cluster_id:origin_or_usa) %>% #renaming the columns to make joining DFs with "movers" easier later
  select(-end_of_career_year_stayers,-years_between_starting_and_moving_stayers, -full_name_stayers) %>% 
  left_join(cumulative_researcher_performance_years_quantiles %>% select(everything(),cluster_id_stayers = cluster_id_movers, -discipline,-specialty), by = "cluster_id_stayers") #adding the per-month performance profiles for our stayers.


######################################## so now we have two DFs - 1) movers with performance decile 0.5 months before move & 2) stayers with performance deciles for all months
################ MATCHING ############## now we want to match our "movers" with researchers that are similar in the following ways:
######################################## gender, origin_country, origin_institution, discipline, both at origin institution at least as long as the time between movers starting and becoming affilated with USA, same performance decile at the time just before the "mover" moved

matched <- movers2 %>% 
  inner_join(stayers, by = c("gender", "origin_country", "origin_institution", "discipline")) %>% #joins "movers" and "stayers" DF by if they have same gender, origin and discipline/specialty(depending on which i choose)
  filter(final_article_at_origin_year_stayers >= year_before_move_movers, #removes any matches if "stayers" weren't at origin when "mover" had last article before moving
         year_before_move_movers == career_year, #keeping only rows where the month of stayers performance is equal to the time when "movers" left minus 0.5 months. This removes any "stayer" matches if they weren't active at this timepoint (i.e. quit science)
         last_year_stayers-first_year_stayers >= years_between_starting_and_moving_movers+2, #the career length of the "stayers" most be equal to at least 2 years after their match moved to the USA
         #p_full_quantile_movers == p_full_quantile,
         #int_collab_quantile_movers == int_collab_quantile,
         #n_coauthors_quantile_movers == n_coauthors_quantile,
         #njs_full_quantile_movers == njs_full_quantile
         ) %>%  #"stayer" matches must be in the same performance decile as "movers" at the time when "movers" left minus 0.5 months %>% 
  group_by(cluster_id_movers) %>% 
  mutate(number_of_matches = n()) %>% 
  ungroup()

unique_matched <- matched %>% 
  arrange(number_of_matches) %>% 
  distinct(cluster_id_stayers, .keep_all = T ) %>% 
  distinct(cluster_id_movers, .keep_all=T) %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year = years_between_starting_and_moving_movers) %>% 
  mutate(pair_id = row_number())

cluster_id_and_pair_id <- 
  unique_matched %>% 
  pivot_longer(!c(pair_id,moving_year), names_to = "condition", values_to = "cluster_id") %>% 
  mutate(condition = str_replace(condition, "cluster_id_", ""))


###### matching version two ##### 
##: not including performance metrics in matching





###############################################################################
########### getting basic info and performance for matched sample #############
###############################################################################

