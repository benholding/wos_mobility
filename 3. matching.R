#4. matching movers and non-movers
library(dplyr)
load(file = "for_matching.RData")

#########################################################################
#########################################################################
################## MATCHING "MOVERS" TO "STAYERS" #######################
#########################################################################
#########################################################################

############################################################
# MATCHING STEP ONE: identifying our movers and non-movers #
############################################################

movers_to_USA_id_list <- movers_dataset_final %>% distinct(cluster_id)
never_movers_id_list <- stayers_dataset_4 %>% distinct(cluster_id)

##########################################################################
# MATCHING STEP TWO: GET BASIC INFORMATION ABOUT OUR MOVERS AND STAYERS  #
##########################################################################

#researcher_basic_info contains one line per researcher (either "mover" or potential match) with descriptive information for everyone (name, discipline, gender, institution) and some specific information just for the matches (e.g. differences in ranking)

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
  left_join(movers_dataset_final %>% #here i add the "extra information i had only calculated for the movers. Basically - what institution they moved to, when they moved, ranking, and differences in ranking)
              select(cluster_id, years_between_starting_and_moving, USA_institution, origin_qs_overall_score_mean, origin_qs_overall_rank_quartiles, USA_qs_overall_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean, origin_pp_top10_mean_quantile,USA_pp_top10_mean, USA_pp_top10_mean_quantile,gelman_difference_in_qs_overall_score,gelman_difference_in_pptop10, difference_in_pptop10_quantile,difference_in_qs_overall_ranking_quantile, origin_type, USA_type) %>% 
              distinct(cluster_id, .keep_all = T)) %>%
  mutate(origin_or_usa = if_else(stays_at_origin_country == TRUE, "origin", "usa")) %>%  #new variable: here I make a single variable stating the "condition" of the researcher. i later make a variable actually called condition (confusing i know)
  select(cluster_id,origin_or_usa, discipline,specialty, full_name,gender,origin_institution, origin_country, origin_region = region,final_article_at_origininstitution_year, first_year, last_year, end_of_career_year, years_between_starting_and_moving, USA_institution, origin_qs_overall_score_mean, origin_qs_overall_rank_quartiles, USA_qs_overall_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean, origin_pp_top10_mean_quantile,USA_pp_top10_mean,USA_pp_top10_mean_quantile,gelman_difference_in_qs_overall_score,gelman_difference_in_pptop10, difference_in_pptop10_quantile,difference_in_qs_overall_ranking_quantile,origin_leiden_ranked, origin_type, USA_type) #neatening up df

##############################################################################################
# MATCHING STEP THREE: MAKING QUANTILES OF CUMULTATIVE PERFORMANCE INFO FOR ELIGIBLE SAMPLE  #
##############################################################################################

# here I create a dataset containing the quantile (1 to 5, within specialty) of cumulative performance (p_full, int_collab, n_coauthors, njs_full_quantile) for each eligible researcher given the career year
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

#Step 4a. making a dataframe of the movers for matching ( i needed to do it a special way, so that i could do a left_join for the actual matching)
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

#Step 4b. making a dataframe of the stayers for matching (again, i had to do it in a special way so i could do the left_join in the next step)
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
# 1. gender, 
# 2. origin_country, 
# 3.origin_institution 
# 4. discipline, both at origin institution at least as long as the time between movers starting and becoming affilated with USA

#step 5a - matching movers and stayers based off of the 4 main matching criteria
matches_basic <- for_matching_movers %>% 
  left_join(for_matching_stayers, by = c("gender", "origin_country", "origin_institution", "discipline"))  #joins "movers" and "stayers" DF by if they have same gender, origin and discipline/specialty(depending on which i choose)

length(unique(matches_basic$cluster_id_movers))  #2368

#step 5b - making sure that the control group were at the origin before their match moved, and making sure the controls were active in science for at least 2 years after their match moved.
matches_eligible <- matches_basic %>% 
  filter(career_year == year_before_move_movers, #keeping only rows where the year of stayers performance is equal to the time when "movers" left minus 1 year. This removes any "stayer" matches if they weren't active at this timepoint (i.e. quit science)
         final_article_at_origininstitution_year_stayers >= year_before_move_movers, #removes any matches if "stayers" weren't at origin when "mover" had last article before moving
         last_year_stayers-first_year_stayers >= years_between_starting_and_moving_movers+3) %>% #the career length of the "stayers" most be equal to at least 2 years after their match moved to the USA. In this bit of code, I calculate the career length of the stayers, and then make sure it's as long as the "movers'" + 2 years (the code says +3 because the final year in the data refers to the first year of having no publications, so then would allow for a publication gap of at least 2 years)
  group_by(cluster_id_movers) %>% 
  mutate(number_of_matches = n()) %>% 
  ungroup()

length(unique(matches_eligible$cluster_id_movers))  #2160

#step 5c - keeping only “unique“ matches. By this i mean that each control participant can only be matched with one mover.
# matches with the same specialty, and if they have the same specialty then longer careers, are preferred matches.
matches_unique <- matches_eligible %>% 
  mutate(do_specialties_match = if_else(specialty_movers == specialty_stayers, 1, 0)) %>% 
  arrange(number_of_matches, desc(do_specialties_match), desc(end_of_career_year_stayers)) %>% #I match on specialties when possible. I also prefer stayers individuals in science for longer
  distinct(cluster_id_stayers, .keep_all = T) %>% 
  group_by(cluster_id_movers) %>% 
  mutate(number_of_uniquematches = n()) %>% 
  ungroup() %>% 
  distinct(cluster_id_movers, .keep_all=T) %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year = years_between_starting_and_moving_movers, discipline, do_specialties_match, specialty_movers,specialty_stayers) %>% 
  mutate(pair_id = row_number())

length(unique(matches_unique$cluster_id_movers))  #1989
length(unique(matches_eligible$cluster_id_movers))  -length(unique(matches_unique$cluster_id_movers)) #how many duplicate matches = 171

#step 5d - I put the unique matches into long format, that can be used the left_join to publication information.
cluster_id_and_pair_id <- 
  matches_unique %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year, pair_id) %>% 
  pivot_longer(!c(pair_id,moving_year), names_to = "condition", values_to = "cluster_id") %>%
  mutate(condition = str_replace(condition, "cluster_id_", "")) #new variable: condition

##########################################################################################################
########## MATCHING STEP SIX - MAKING A DATASET OF PERFORMANCE FOR THE MATCHED RESEARCHERS ###############
##########################################################################################################

#setting up data for diff-in-diff
matched_dataset <- cluster_id_and_pair_id %>% 
  left_join(researcher_performance_years, by = "cluster_id") %>% 
  left_join(researcher_basic_info %>% select(cluster_id, origin_institution, origin_country,origin_region,USA_institution, discipline, specialty,end_of_career_year, gender, origin_qs_overall_score_mean,origin_qs_overall_rank_quartiles,USA_qs_overall_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean, origin_pp_top10_mean_quantile, USA_pp_top10_mean, USA_pp_top10_mean_quantile, gelman_difference_in_qs_overall_score,difference_in_qs_overall_ranking_quantile, gelman_difference_in_pptop10, difference_in_pptop10_quantile,origin_leiden_ranked, origin_type, USA_type), by = "cluster_id") %>% 
  select(cluster_id, pair_id, moving_year, condition, discipline, specialty, career_year,end_of_career_year, everything()) %>% 
  mutate(years_from_obtaining_usa_affilation = career_year-moving_year,
         post_move = if_else(years_from_obtaining_usa_affilation >= 0, 1, 0),
         career_over = if_else(career_year > end_of_career_year, 1, 0), #end_of_career_year is the last year the resarcher had any publications. Therefore, if the career year is over that, we say that their career is over.
         condition_numeric = if_else(condition == "movers", 1, 0),
         woman = if_else(gender == "F", 1, 0),
         moving_year_plus1 = moving_year+1, #to add interpretability I added one here. this means that the year that someone starts their career is year 1 instead of year 0
         career_year_plus_1 = career_year+1,
         moving_year_plus1 = if_else(condition == "stayers", 0, moving_year_plus1),
         difference_in_pptop10_quantile_with_zeros = if_else(condition_numeric == 0, 0, difference_in_pptop10_quantile)) %>% 
  select(-cs_full_yearsum,-cs_frac_yearsum, -p_top_prop1_full_yearsum,-p_top_prop1_frac_yearsum,-p_industry_yearsum,-p_int_collab_yearsum,-p_short_dist_collab_yearsum,-p_long_dist_collab_yearsum,-cs_full_mean,-cs_frac_mean,-js_full_median,-js_full_mean,-js_frac_mean,-js_frac_median,-njs_full_median,-njs_frac_median,-n_coauthors_mean,-n_coauthors_median,-js_full_cummean,-js_frac_cummean,-njs_full_cummean,-njs_frac_cummean,-cs_mean_full_cummean,-n_coauthors_cummean,-n_coauthors_cummedian,-p_full_cumsum,-p_frac_cumsum,-cs_frac_cumsum,-ncs_frac_cumsum,-p_top_prop10_full_cumsum,-p_top_prop10_frac_cumsum,-int_collab_cumsum) #removing lots of variables we never use in the analysis

################################################################################################################################################################################################
##### *OPTIONAL* MATCHING STEP SEVEN - MAKING A DATASET FOR THE ROBUSTNESS CHECK WHERE INDIVIDUALS ARE ALSO MATCHED ON PRIOR CUMUALTIVE PERFORMANCE UP TO THE POINT JUST BEFORE THE MOVE #######
################################################################################################################################################################################################

matched_eligible_robustnesscheck <- robustnesscheck <- matches_basic %>% 
  filter(career_year == year_before_move_movers, #keeping only rows where the year of stayers performance is equal to the time when "movers" left minus 1 year. This removes any "stayer" matches if they weren't active at this timepoint (i.e. quit science)
         final_article_at_origininstitution_year_stayers >= year_before_move_movers, #removes any matches if "stayers" weren't at origin when "mover" had last article before moving
         last_year_stayers-first_year_stayers >= years_between_starting_and_moving_movers+3, #the career length of the "stayers" most be equal to at least 2 years after their match moved to the USA. In this bit of code, I calculate the career length of the stayers, and then make sure it's as long as the "movers'" + 2 years (the code says +3 because the final year in the data refers to the first year of having no publications, so then would allow for a publication gap of at least 2 years)
         p_full_quantile_movers == p_full_quantile,
         njs_full_quantile_movers == njs_full_quantile #"stayer" matches must be in the same performance decile as "movers" at the time when "movers" left minus 1 year
  ) %>%  
  group_by(cluster_id_movers) %>% 
  mutate(number_of_matches = n()) %>% 
  ungroup()

unique_matched_robustnesscheck <- matched_eligible_robustnesscheck %>% 
  mutate(do_specialties_match = if_else(specialty_movers == specialty_stayers, 1, 0)) %>% 
  arrange(number_of_matches, desc(do_specialties_match), desc(end_of_career_year_stayers)) %>% #I match on specialties when possible. I also prefer stayers individuals in science for longer
  distinct(cluster_id_stayers, .keep_all = T) %>% 
  group_by(cluster_id_movers) %>% 
  mutate(number_of_uniquematches = n()) %>% 
  ungroup() %>% 
  distinct(cluster_id_movers, .keep_all=T) %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year = years_between_starting_and_moving_movers, discipline, do_specialties_match, specialty_movers,specialty_stayers) %>% 
  mutate(pair_id = row_number())

cluster_id_and_pair_id_robustnesscheck <- 
  unique_matched_robustnesscheck %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year, pair_id) %>% 
  pivot_longer(!c(pair_id,moving_year), names_to = "condition", values_to = "cluster_id") %>% 
  mutate(condition = str_replace(condition, "cluster_id_", ""))

matched_dataset_robustnesscheck  <- cluster_id_and_pair_id_robustnesscheck %>% 
  left_join(researcher_performance_years, by = "cluster_id") %>% 
  left_join(researcher_basic_info %>% select(cluster_id, origin_institution, origin_country,origin_region,USA_institution, discipline, specialty,end_of_career_year, gender, origin_qs_overall_score_mean,origin_qs_overall_rank_quartiles,USA_qs_overall_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean, origin_pp_top10_mean_quantile, USA_pp_top10_mean, USA_pp_top10_mean_quantile, gelman_difference_in_qs_overall_score,difference_in_qs_overall_ranking_quantile, gelman_difference_in_pptop10, difference_in_pptop10_quantile,origin_leiden_ranked, origin_type, USA_type), by = "cluster_id") %>% 
  select(cluster_id, pair_id, moving_year, condition, discipline, specialty, career_year,end_of_career_year, everything()) %>% 
  mutate(years_from_obtaining_usa_affilation = career_year-moving_year,
         post_move = if_else(years_from_obtaining_usa_affilation >= 0, 1, 0),
         career_over = if_else(career_year >= end_of_career_year, 1, 0),
         condition_numeric = if_else(condition == "movers", 1, 0),
         woman = if_else(gender == "F", 1, 0),
         current_qs_ranking = if_else(post_move == 1 & condition_numeric == 1, USA_qs_overall_rank_quartiles, origin_qs_overall_rank_quartiles),
         current_leiden_ranking = if_else(post_move == 1 & condition_numeric == 1, USA_pp_top10_mean_quantile, origin_pp_top10_mean_quantile))%>% 
  mutate(moving_year_plus1 = moving_year+1, #to add interpretability I added one here. this means that the year that someone starts their career is year 1 instead of year 0
         career_year_plus_1 = career_year+1,
         moving_year_plus1 = if_else(condition == "stayers", 0, moving_year_plus1),
         difference_in_pptop10_quantile_with_zeros = if_else(condition_numeric == 0, 0, difference_in_pptop10_quantile)) %>% 
  select(-cs_full_yearsum,-cs_frac_yearsum, -p_top_prop1_full_yearsum,-p_top_prop1_frac_yearsum,-p_industry_yearsum,-p_int_collab_yearsum,-p_short_dist_collab_yearsum,-p_long_dist_collab_yearsum,-cs_full_mean,-cs_frac_mean,-js_full_median,-js_full_mean,-js_frac_mean,-js_frac_median,-njs_full_median,-njs_frac_median,-n_coauthors_mean,-n_coauthors_median,-js_full_cummean,-js_frac_cummean,-njs_full_cummean,-njs_frac_cummean,-cs_mean_full_cummean,-n_coauthors_cummean,-n_coauthors_cummedian,-p_full_cumsum,-p_frac_cumsum,-cs_frac_cumsum,-ncs_frac_cumsum,-p_top_prop10_full_cumsum,-p_top_prop10_frac_cumsum,-int_collab_cumsum) #removing lots of variables we never use in the analysis


save(matched_dataset,matched_dataset_robustnesscheck, file = "matched_dataset.RData")
