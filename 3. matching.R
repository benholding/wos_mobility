#4. matching movers and non-movers
source("2. data cleaning and maniputation.R") #importing data and packages

#########################################################################
#########################################################################
################## MATCHING "MOVERS" TO "STAYERS" #######################
#########################################################################
#########################################################################

############################################################
# MATCHING STEP ONE: identifying our movers and non-movers #
############################################################

movers_to_USA_id_list <- movers_dataset_final %>% distinct(cluster_id)
never_movers_id_list <- stayers_dataset_3 %>% distinct(cluster_id)

##########################################################################
# MATCHING STEP TWO: GET BASIC INFORMATION ABOUT OUR MOVERS AND STAYERS  #
##########################################################################

researcher_basic_info <- final_complete_dataset %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>% 
  mutate(end_of_career_year = last(career_year)) %>% #new variable: career length in years
  ungroup() %>% 
  left_join(european_country_list, by = c("origin_country" = "country")) %>% #to include country "region"
  distinct(cluster_id, .keep_all = T) %>% #only one row per cluster_id
  mutate(moves_to_usa = if_else(cluster_id %in% movers_to_USA_id_list$cluster_id, TRUE, FALSE), #new variable: does this researcher move to the USA?
         stays_at_origin_country = if_else(cluster_id %in% never_movers_id_list$cluster_id, TRUE, FALSE)) %>%  #new variable: does this researcher stay in origin country?
  filter(stays_at_origin_country == TRUE | moves_to_usa == TRUE) %>% #this is when this dataframe only becomes our movers and potential matches (nb: controls can still have careers <24 months)
  left_join(movers_dataset_final %>% 
              select(cluster_id, years_between_starting_and_moving, USA_institution, origin_qs_overall_score_mean, origin_qs_overall_rank_quartiles, USA_qs_overall_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean, origin_pp_top10_mean_quantile,USA_pp_top10_mean, USA_pp_top10_mean_quantile,gelman_difference_in_qs_overall_score,gelman_difference_in_pptop10, difference_in_pptop10_quantile,difference_in_qs_overall_ranking_quantile, origin_type, USA_type) %>% 
              distinct(cluster_id, .keep_all = T)) %>% #new variables: in this join I add information about the move (career year, new institution name, rankings, differences in rankings)
  mutate(origin_or_usa = if_else(stays_at_origin_country == TRUE, "origin", "usa")) %>%  #new variable: here I make a single variable stating the "condition" of the researcher
  select(cluster_id,origin_or_usa, discipline,specialty, full_name,gender,origin_institution, origin_country, origin_region = region,final_article_at_origininstitution_year, first_year, last_year, end_of_career_year, years_between_starting_and_moving, USA_institution, origin_qs_overall_score_mean, origin_qs_overall_rank_quartiles, USA_qs_overall_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean, origin_pp_top10_mean_quantile,USA_pp_top10_mean,USA_pp_top10_mean_quantile,gelman_difference_in_qs_overall_score,gelman_difference_in_pptop10, difference_in_pptop10_quantile,difference_in_qs_overall_ranking_quantile,origin_leiden_ranked, origin_type, USA_type) #neatening up df

##############################################################################################
# MATCHING STEP THREE: MAKING QUANTILES OF CUMULTATIVE PERFORMANCE INFO FOR ELIGIBLE SAMPLE  #
##############################################################################################

cumulative_researcher_performance_years_quantiles <- cumulative_researcher_performance_years %>% 
  left_join(researcher_basic_info %>% select(cluster_id, discipline, specialty), by = "cluster_id") %>% 
  select(discipline, specialty, cluster_id, career_year, p_full_cumsum, int_collab_cumsum, n_coauthors_cummedian, njs_full_cummean) %>% 
  group_by(specialty, career_year) %>% 
  mutate(p_full_quantile = ntile(p_full_cumsum, 5),
         int_collab_quantile = ntile(int_collab_cumsum, 5),
         n_coauthors_quantile = ntile(n_coauthors_cummedian, 5),
         njs_full_quantile = ntile(njs_full_cummean, 5)) %>% 
  ungroup() %>%  #calculating quantiles for performance in all cluster_ids.
  select(-ends_with(c("cumsum","cummean", "cummedian")))

##################################################################################################################
# MATCHING STEP FOUR: MAKING A DATASET EACH FOR MOVERS AND STAYERS CONTAINING THEIR PERFORMANCE PRIOR TO MOVING  #
##################################################################################################################

#making a dataframe of the movers
movers_basic_info <- researcher_basic_info %>%
  filter(origin_or_usa == "usa") %>% 
  select(gender, discipline,specialty, origin_country, origin_institution, everything()) %>% 
  rename_with(~paste0(., "_movers"), .cols = cluster_id:difference_in_qs_overall_ranking_quantile) %>%  #renaming the columns to make joining DFs with "movers" easier later
  mutate(year_before_move_movers = years_between_starting_and_moving_movers-1) #calculating which year counts as the "year before move"

movers_cumulative_performance_before_move <- movers_basic_info %>% 
  select(cluster_id_movers, year_before_move_movers) %>% 
  left_join(cumulative_researcher_performance_years_quantiles, by =c("cluster_id_movers" = "cluster_id","year_before_move_movers" = "career_year")) %>% 
  select(-discipline,-specialty) %>% 
  rename_with(~paste0(., "_movers"), .cols = p_full_quantile:njs_full_quantile)


for_matching_movers <- movers_basic_info %>% 
  select(cluster_id_movers, gender, discipline,specialty_movers = specialty,origin_country, origin_institution,years_between_starting_and_moving_movers) %>% 
           left_join(movers_cumulative_performance_before_move, by = "cluster_id_movers")

# making a dataframe of the stayers
for_matching_stayers <- researcher_basic_info %>% # take the basic df
  select(gender, discipline, origin_country, origin_institution, cluster_id,specialty, first_year, last_year, end_of_career_year,final_article_at_origininstitution_year,origin_or_usa) %>% 
  filter(origin_or_usa == "origin") %>% #keep only our "stayers"
  rename_with(~paste0(., "_stayers"), .cols = cluster_id:origin_or_usa) %>% #renaming the columns to make joining DFs with "movers" easier later
  left_join(cumulative_researcher_performance_years_quantiles %>% select(-discipline,-specialty), by = c("cluster_id_stayers"="cluster_id")) #adding the per-month performance profiles for our stayers.

############################################
# MATCHING STEP FIVE: The actual matching  #
############################################

# so now we have two DFs - 1) movers with performance quantiles & 2) stayers with performance deciles for all months
# now we want to match our "movers" with researchers that are similar in the following ways:
#gender, origin_country, origin_institution, discipline, both at origin institution at least as long as the time between movers starting and becoming affilated with USA

matched <- for_matching_movers %>% 
  left_join(for_matching_stayers, by = c("gender", "origin_country", "origin_institution", "discipline"))  #joins "movers" and "stayers" DF by if they have same gender, origin and discipline/specialty(depending on which i choose)

length(unique(matched$cluster_id_movers))  

matched2 <- matched %>% 
  filter(career_year == year_before_move_movers, #keeping only rows where the year of stayers performance is equal to the time when "movers" left minus 1 year. This removes any "stayer" matches if they weren't active at this timepoint (i.e. quit science)
         final_article_at_origininstitution_year_stayers >= year_before_move_movers, #removes any matches if "stayers" weren't at origin when "mover" had last article before moving
         last_year_stayers-first_year_stayers >= years_between_starting_and_moving_movers+3, #the career length of the "stayers" most be equal to at least 2 years after their match moved to the USA. In this bit of code, I calculate the career length of the stayers, and then make sure it's as long as the "movers'" + 2 years (the code says +3 because the final year in the data refers to the first year of having no publications, so then would allow for a publication gap of at least 2 years)
         #p_full_quantile_movers == p_full_quantile,
         #int_collab_quantile_movers == int_collab_quantile,
         #n_coauthors_quantile_movers == n_coauthors_quantile,
         #njs_full_quantile_movers == njs_full_quantile #"stayer" matches must be in the same performance decile as "movers" at the time when "movers" left minus 1 year
         ) %>%  
  group_by(cluster_id_movers) %>% 
  mutate(number_of_matches = n()) %>% 
  ungroup()

length(unique(matched2$cluster_id_movers))  #2424

unique_matched <- matched2 %>% 
  mutate(do_specialties_match = if_else(specialty_movers == specialty_stayers, 1, 0)) %>% 
  arrange(number_of_matches, desc(do_specialties_match), desc(end_of_career_year_stayers)) %>% #I match on specialties when possible. I also prefer stayers individuals in science for longer
  distinct(cluster_id_stayers, .keep_all = T) %>% 
  group_by(cluster_id_movers) %>% 
  mutate(number_of_uniquematches = n()) %>% 
  ungroup() %>% 
  distinct(cluster_id_movers, .keep_all=T) %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year = years_between_starting_and_moving_movers, discipline, do_specialties_match, specialty_movers,specialty_stayers) %>% 
  mutate(pair_id = row_number())

length(unique(unique_matched$cluster_id_movers))  #2250
length(unique(matched2$cluster_id_movers))  -length(unique(unique_matched$cluster_id_movers)) #how many duplicate matches = 174


cluster_id_and_pair_id <- 
  unique_matched %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year, pair_id) %>% 
  pivot_longer(!c(pair_id,moving_year), names_to = "condition", values_to = "cluster_id") %>% 
  mutate(condition = str_replace(condition, "cluster_id_", ""))
