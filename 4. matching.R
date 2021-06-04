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
         end_of_career_months = last(career_length_months_at_this_pub)) %>% #new variable: career length in months
  select(cluster_id, discipline,specialty, full_name, first_name, first_year, last_year, end_of_career_months,gender, gender_accuracy, origin_country,origin_institution, final_article_at_origin_career_month) %>% 
  distinct(cluster_id, .keep_all = T) %>% #only one row per cluster_id
  mutate(moves_to_usa = if_else(cluster_id %in% movers_to_USA_id_list$cluster_id, TRUE, FALSE), #new variable: does this researcher move to the USA?
         stays_at_origin_country = if_else(cluster_id %in% never_movers_id_list$cluster_id, TRUE, FALSE)) %>%  #new variable: does this reseearcher stay in origin country?
  filter(stays_at_origin_country == TRUE | moves_to_usa == TRUE) %>% #this is when this dataframe only becomes our movers and potential matches (nb: controls can still have careers <24 months)
  left_join(movers_dataset_final %>% 
              select(cluster_id, months_between_starting_and_moving) %>% 
              distinct(cluster_id, .keep_all = T)) %>% #new variable: in this join I add a column which represents the month in which the movers move
  mutate(origin_or_usa = if_else(stays_at_origin_country == TRUE, "origin", "usa")) %>% #new variable: here I make a single variable stating the "condition" of the researcher
  select(-stays_at_origin_country, -moves_to_usa, -gender_accuracy, -first_name, -last_year) #neatening up df

######################################## 
###### PREPERATION FOR MATCHING ########
######################################## 

#step 1. making a dataframe of the movers
movers <- researcher_basic_info %>%
  filter(origin_or_usa == "usa") %>% 
  select(gender, discipline,specialty, origin_country, origin_institution, everything()) %>% 
  rename_with(~paste0(., "_movers"), .cols = cluster_id:origin_or_usa) #renaming the columns to make joining DFs with "movers" easier later

# step 2. adding mover performance before move
month_before_move <- movers %>% mutate(month = months_between_starting_and_moving_movers-0.5) %>%  select(cluster_id_movers, month) #calculating which month counts as the "month before move" (actually it's half a month = 0.5)

cumulative_researcher_performance_months_quantiles <- cumulative_researcher_performance_months %>% 
  left_join(researcher_basic_info %>% select(cluster_id, discipline, specialty), by = "cluster_id") %>% 
  select(discipline, specialty, cluster_id_movers = cluster_id, month, cum_p_full, cum_int_collab, cum_n_coauthors, njs_full_mean) %>% 
  group_by(specialty, month) %>% 
  mutate(cum_p_full_quantile = ntile(cum_p_full, 5),
         cum_int_collab_quantile = ntile(cum_int_collab, 5),
         cum_n_coauthors_quantile = ntile(cum_n_coauthors, 5),
         njs_full_mean_quantile = ntile(njs_full_mean, 5)) %>% 
  ungroup() #calculating quantiles for performance in all cluster_ids. NOTE: the variable used for the performance assessment is likely to change depending on how we measure performance

movers_performance_before_move <- left_join(month_before_move, cumulative_researcher_performance_months_quantiles, by =c("cluster_id_movers","month")) %>% rename(month_before_move_movers = month, cum_p_full_quantile_movers = cum_p_full_quantile, cum_int_collab_quantile_movers = cum_int_collab_quantile, cum_n_coauthors_quantile_movers = cum_n_coauthors_quantile, njs_full_mean_quantile_movers = njs_full_mean_quantile) %>% select(-discipline,-specialty) #here I get performance decile just for the month before the move for all "movers
movers2 <- movers %>% left_join(movers_performance_before_move, by = "cluster_id_movers") %>% select(-first_year_movers,-full_name_movers,-end_of_career_months_movers, -final_article_at_origin_career_month_movers) # here i add back in important demographic info (like origin institution etc).

# step 3. making a dataframe with stayers performance. 
stayers <- researcher_basic_info %>% # take the basic df
  select(gender, discipline,specialty, origin_country, origin_institution, everything()) %>% 
  filter(origin_or_usa == "origin") %>% #keep only our "stayers"
  rename_with(~paste0(., "_stayers"), .cols = cluster_id:origin_or_usa) %>% #renaming the columns to make joining DFs with "movers" easier later
  select(-end_of_career_months_stayers, -months_between_starting_and_moving_stayers, -full_name_stayers) %>% 
  left_join(cumulative_researcher_performance_months_quantiles %>% select(everything(),cluster_id_stayers = cluster_id_movers, -discipline,-specialty), by = "cluster_id_stayers") #adding the per-month performance profiles for our stayers.


######################################## so now we have two DFs - 1) movers with performance decile 0.5 months before move & 2) stayers with performance deciles for all months
################ MATCHING ############## now we want to match our "movers" with researchers that are similar in the following ways:
######################################## gender, origin_country, origin_institution, discipline, both at origin institution at least as long as the time between movers starting and becoming affilated with USA, same performance decile at the time just before the "mover" moved

matched <- movers2 %>% 
  inner_join(stayers, by = c("gender", "origin_country", "origin_institution", "discipline")) %>% #joins "movers" and "stayers" DF by if they have same gender, origin and discipline/specialty(depending on which i choose)
  filter(final_article_at_origin_career_month_stayers >= month_before_move_movers, #removes any matches if "stayers" weren't at origin when "mover" had last article before moving
         month_before_move_movers == month, #keeping only rows where the month of stayers performance is equal to the time when "movers" left minus 0.5 months. This removes any "stayer" matches if they weren't active at this timepoint (i.e. quit science)
         cum_p_full_quantile_movers == cum_p_full_quantile,
         cum_int_collab_quantile_movers == cum_int_collab_quantile,
         cum_n_coauthors_quantile_movers == cum_n_coauthors_quantile,
         njs_full_mean_quantile_movers == njs_full_mean_quantile
         ) %>%  #"stayer" matches must be in the same performance decile as "movers" at the time when "movers" left minus 0.5 months %>% 
  group_by(cluster_id_movers) %>% 
  mutate(number_of_matches = n()) %>% 
  ungroup()

unique_matched <- matched %>% 
  arrange(number_of_matches) %>% 
  distinct(cluster_id_stayers, .keep_all = T ) %>% 
  distinct(cluster_id_movers, .keep_all=T) %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_month = months_between_starting_and_moving_movers) %>% 
  mutate(pair_id = row_number())

cluster_id_and_pair_id <- 
  unique_matched %>% 
  pivot_longer(!c(pair_id,moving_month), names_to = "condition", values_to = "cluster_id") %>% 
  mutate(condition = str_replace(condition, "cluster_id_", ""))


###### matching version two ##### 
##: not including performance metrics in matching

matched_version2 <- movers2 %>% 
  inner_join(stayers, by = c("gender", "origin_country", "origin_institution", "discipline")) %>% #joins "movers" and "stayers" DF by if they have same gender, origin and discipline/specialty(depending on which i choose)
  filter(final_article_at_origin_career_month_stayers >= month_before_move_movers #removes any matches if "stayers" weren't at origin when "mover" had last article before moving
  ) %>%  #"stayer" matches must be in the same performance decile as "movers" at the time when "movers" left minus 0.5 months %>% 
  group_by(cluster_id_movers) %>% 
  mutate(number_of_matches = n()) %>% 
  ungroup()

unique_matched_version2 <- matched_version2 %>% 
  arrange(number_of_matches) %>% 
  distinct(cluster_id_stayers, .keep_all = T ) %>% 
  distinct(cluster_id_movers, .keep_all=T) %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_month = months_between_starting_and_moving_movers) %>% 
  mutate(pair_id = row_number())

cluster_id_and_pair_id_version2 <- 
  unique_matched_version2 %>% 
  pivot_longer(!c(pair_id,moving_month), names_to = "condition", values_to = "cluster_id") %>% 
  mutate(condition = str_replace(condition, "cluster_id_", ""))



###############################################################################
########### getting basic info and performance for matched sample #############
###############################################################################

