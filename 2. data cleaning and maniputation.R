load(file = "wos_data.RData") #loading data (imported using script 1.)
pacman::p_load(tidyverse, cumstats)

#########################################################################
#########################################################################
########## GETTING A DATASET OF ALL ELIGIBLE AUTHORS IN EUROPE ########## 
#########################################################################
#########################################################################

# making a dataset with publications only for authors that fulfill criteria:
# 1. only from one country
# 2. only European institutions
# 3. only from certain disciplines (those that are well covered in wos, and don't have abnormally high coauthorship rates)

####################################################################################
# STEP ONE: Find authors who are from a single country at order_of_publishing == 1 #
####################################################################################

length(unique(publication_list_all$cluster_id)) #at this point we have 525,892 researchers
percentage_gender_inferable <- 1-mean(is.na(researcher_info$gender))

step1 <- publication_list_all %>% #take the main dataset
  filter(order_of_publishing == 1) %>% #include only rows where it represents the earliest article for that author.
  group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because some have multiple affilations) i need to check that all affilations are from the same country
  mutate(number_of_distinct_countries = n_distinct(pub_country)) %>% #checking if the rows have the same country affilation
  filter(number_of_distinct_countries == 1, #ensuring that only cluster_ids with only 1 distinct country affilation are included
         !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
  select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "publication_list_all" dataframe, which should result in us being left with only the rows in "publication_list_all" that match with the cluster_ids we identified as being eligible 
  distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
  left_join(publication_list_all, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 1
  ungroup()

length(unique(step1$cluster_id)) # we now have 501,492 researchers
length(unique(publication_list_all$cluster_id))-length(unique(step1$cluster_id)) # change = 24400


###################################################
# STEP TWO: Keep only European origin researchers #
###################################################

european_countries <- european_country_list %>% pull(country) #this provides a list of countries we include as European

step2 <- step1 %>%
  filter(order_of_publishing == 1, #at the first paper(s)...
         pub_country %in% european_countries) %>% #...only include the row if the country of the affilation was in our "European" country list.
  select(cluster_id) %>%
  distinct(cluster_id) %>% #keeping only distinct cluster_ids (since for some cluster_ids we would have 2 rows if they had multiple affiliations in country)
  left_join(publication_list_all, by = "cluster_id")

length(unique(step2$cluster_id)) #at this point we have 191,694 researchers
length(unique(step1$cluster_id))-length(unique(step2$cluster_id)) #change = 309798

#####################################################
# STEP THREE: Keeping only well-covered disciplines #
#####################################################

# in order to decide on what disciplines should be included, I looked at the coverage
wos_covered_disciplines <- publication_info %>%
  distinct(ut, .keep_all = T) %>%
  group_by(discipline) %>%
  summarise(proportion_of_refs_covered = mean(n_refs_1980_covered/n_refs, na.rm =T)) %>%
  filter(proportion_of_refs_covered >= .6,#Biology, Biomedical Research, Chemistry, Clinical Medicine, Earth and Space, Engineering & tech, Health, Maths, Physics, & Psychology are all >60% covered
         !is.na(discipline)) 

step3 <- step2 %>% #taking the dataset made in step 2 above
  left_join(publication_info, by = c("ut")) %>% #joining it with the publication_info dataset, in order to get discipline information for each publication
  group_by(cluster_id) %>% 
  count(discipline) %>% # i then count how many articles have each disciples per author
  slice_max(n) %>% #then i make a dataframe showing the discipline per author with the highest number of articles
  filter(discipline %in% wos_covered_disciplines$discipline) %>% #i keep only authors where their most popular discipline is >=60% covered
  select(cluster_id, discipline) %>% #selecting the remaining authors (and also their discipline)
  left_join(step2, by = "cluster_id") %>%  #joining our selection of authors from only our chosen disciplines, with the information present in the previous dataframe.
  ungroup()

length(unique(step3$cluster_id)) #at this point we have 181,751 researchers
length(unique(step2$cluster_id))-length(unique(step3$cluster_id)) #change = 9943
################################################################################
# STEP FOUR: removing any subdisciplines that have too high coauthorship rates #
################################################################################
#here i look at mean numbers of co-authors per specialty
average_number_of_coauthors_per_specialty <- publication_info %>% 
  distinct(ut, .keep_all = T) %>%
  group_by(specialty) %>% 
  summarise(mean_n_authors = mean(n_authors, na.rm=T),
            median_n_authors = median(n_authors, na.rm=T)) %>% 
  arrange(desc(mean_n_authors)) #Nuclear & Particle Physics has 139 mean authors compared to next nearest 18.8, so anyone working primarily in this specialty will be removed

#here I add the each researchers main specialty to the dataset, and then remove researchers who focus on the specialty "Nuclear & Particle Physics"
step4 <- step3 %>% #take the step 3 dataset (i.e. only wos covered disciplines)
  select(-discipline) %>% #...remove this column since it will be duplicate when we...
  left_join(publication_info, by = c("ut")) %>% #...join the author information to the publication metadata
  group_by(cluster_id) %>% 
  add_count(specialty, name = "n_specialty_articles") %>% #per cluster id, this provides a count of number of articles with this specialty...
  add_count(discipline, name = "n_discipline_articles") %>% #and also discipline
  distinct(cluster_id, discipline, specialty, .keep_all = T) %>% #keeping one row per cluster_id
  select(cluster_id, discipline, n_discipline_articles,specialty, n_specialty_articles) %>% 
  filter(!is.na(discipline),
         !is.na(specialty)) %>% #keeping only individuals with a main discipline and specialty
  arrange(cluster_id, desc(n_discipline_articles), desc(n_specialty_articles)) %>% #this arranges the dataset so that an individuals top discipline is at the top, which is further ordered by specialty 
  slice(1) %>% #taking an individuals top specialty....
  select(cluster_id, specialty) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  filter(specialty != "Nuclear & Particle Physics") %>% #...and excluding  cluster ids with a specialty of nuclear physics.
  left_join(step3, by = "cluster_id") #then joining the whole dataset back got all individuals that weren't excluded.

length(unique(step4$cluster_id)) #at this point we have 178,621 researchers
length(unique(step3$cluster_id))-length(unique(step4$cluster_id)) #change = 3130
###################################################################
# STEP FIVE: MAKING THE FINAL DATASET OF ALL ELIGIBLE RESEARCHERS #
###################################################################

# Here I chose a institute which represents the "origin" of the researcher. 
step5 <- 
  step4 %>% 
  filter(order_of_publishing == 1) %>% #take the data of everyone at order_of_publishing = 1...
  select(cluster_id, pub_org_name) %>% 
  left_join(publication_list_all, by = c("cluster_id", "pub_org_name")) %>% #... get their publications
  group_by(cluster_id, pub_org_name) %>% 
  mutate(number_of_publications_with_this_affilation = n()) %>% #for each affilation measure how many times researchers published with this affilation during career.
  distinct(cluster_id, pub_org_name, number_of_publications_with_this_affilation, .keep_all = T) %>% 
  select(cluster_id, pub_org_name, number_of_publications_with_this_affilation, lr_univ_id,pub_country) %>% 
  group_by(cluster_id) %>% 
  arrange(cluster_id, desc(number_of_publications_with_this_affilation),lr_univ_id) %>% #important ordering to ensure the comment below is correct.
  mutate(origin_institution = first(pub_org_name), #this makes a variable of the origin institution. If there were multiple institutions at order_of_publishing == 1, then this takes the institution where the researcher had the most publications in his/her career. if it is a tie, then the leiden ranked university is chosen. If it is still a tie then it is selected alphabetically.
         origin_country = first(pub_country), #new variable: what is the origin country of the researcher
         origin_leiden_ranked = first(if_else(is.na(lr_univ_id), 0, 1))) %>% #new variable: is the origin institute Leiden ranked?
  select(cluster_id, origin_institution, origin_country,origin_leiden_ranked) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  left_join(step4, by ="cluster_id") #creates a dataset with information about the authors, including origin info, + each UT (but with no further meta data)
  
final_complete_dataset <- step5 %>% #this becomes the dataset that contains descriptive information about all of our potential matches
  filter(origin_institution == pub_org_name) %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>%
  mutate(final_article_at_origininstitution_year = last(career_year)) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  select(cluster_id, final_article_at_origininstitution_year) %>% 
  left_join(step5, by = "cluster_id") %>% 
  left_join(publication_info %>% select(ut, n_authors, n_countries), by = "ut") %>% #adding number of authors on paper, and number of countries
  mutate(n_coauthors = n_authors - 1) 

length(unique(final_complete_dataset$cluster_id)) #at this point we have 178,621 researchers
length(unique(publication_list_all$cluster_id))-length(unique(final_complete_dataset$cluster_id)) #total exclusion to "enrollment" = 347271

save(final_complete_dataset, file = "eligible_researchers.RData")
#########################################################################
#########################################################################
########## GETTING A DATASET OF ALL ELIGIBLE MOVERS TO THE USA ##########
#########################################################################
#########################################################################

# making a dataset with publications only for authors that fulfill criteria:
# 1. Moving to the USA was the researchers first international move
# 2. Must move after at least two years at origin institution
# 3. Must be still affiliated to origin at the publication immediately before the move to the USA
# 4. Only getting a new affilation in the USA (no other country) allowed
# 5. Must move on or before 2016
# 6. Must have at least two publications at the origin institution before moving
# 7. Must have gender information

##########################################################################
# STEP SIX: Find people who moved to the USA and it was their first move #
##########################################################################

final_complete_dataset %>% filter(pub_country == "United States") %>% distinct(cluster_id) %>% tally() #9140 people at least once got USA affilation
length(unique(final_complete_dataset$cluster_id))-9140 # how many non movers

step6 <- 
  final_complete_dataset %>% #taking the main dataset
  arrange(cluster_id, order_of_publishing) %>% #Then I arrange the dataset, so that the order_of_publishing 1 is always first for each cluster_id 
  group_by(cluster_id) %>% #then within each cluster ID I am going to check when people moved to a first new country
  mutate(new_country_compared_to_lag1 = if_else(pub_country != lag(pub_country), TRUE, FALSE), #so here I compare a persons pub_country at order_of_publishing x to pub_country at time x-1. Is it changes it puts a TRUE in the column
         new_country_compared_to_lag1_is_duplicate = duplicated(new_country_compared_to_lag1), #since we only want to see the first move, I made another column that puts a TRUE if a TRUE has already been seen for that subject in the new_country_compared_to_lag1 variable
         is_first_new_country = if_else(new_country_compared_to_lag1 == TRUE & new_country_compared_to_lag1_is_duplicate == FALSE, TRUE, FALSE), #then using the two previous variables, I make a final variable that shows the publication where a first move was made
         origin_country = first(pub_country)) %>%   #this is nothing to do with the above 3 lines. I just wanted to add a variable that states what the origin country is for each cluster_ID
  filter(is_first_new_country == TRUE & pub_country == "United States") %>% #this makes the dataset represent only data for the time when the individual moved to the usa.
  mutate(USA_institution = pub_org_name,
         USA_lr_univ_id = lr_univ_id) %>% #adding a column stating what the destination is for the researcher
  ungroup()

length(unique(step6$cluster_id)) #7229
9140-length(unique(step6$cluster_id))

###############################################################
# STEP SEVEN: find the people that moved only after two years #
###############################################################

step7 <- step6 %>% #taking the dataset of researchers who moved to the USA as their first ever abroad affilation
  group_by(cluster_id) %>% #and within each cluster id...
  filter(career_year >= 2) %>% #keep participants who moved at least 2 years (i.e. past year 0 and year 1) into their career
  select(cluster_id, USA_institution, move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, years_between_starting_and_moving = career_year, USA_lr_univ_id) %>% #i'm selecting and renaming certain columns here to neaten things up
  left_join(final_complete_dataset, by = "cluster_id") #then i reattach all of the publications for each of our researchers which moved at or after month 24 (2 years)

length(unique(step7$cluster_id)) #6426
length(unique(step6$cluster_id))-length(unique(step7$cluster_id)) #change = 803

################################################################################################################################
# STEP EIGHT: Include only people that were still affiliated with their origin institution immediately before moving to the USA #
################################################################################################################################

step8 <- step7 %>% # so for all of our eligible movers...
  filter(order_of_publishing == move_to_USA_publication_order-1, #... we check that at the publication before the publication showing they had moved ot the USA...
         pub_org_name == origin_institution) %>% ###they were still affiliated with their origin institution. The helps us know that they haven't moved loads before moving to the USA
  distinct(cluster_id) %>% #for those who pass this criteria I collect their cluster_ids....
  left_join(step7, by ="cluster_id") #... and then get the information from the movers_24_months_min dataset for those cluster_ids.

length(unique(step8$cluster_id)) #5539
length(unique(step7$cluster_id)) - length(unique(step8$cluster_id)) # change = 887
##############################################################################
# STEP NINE: Exclude anyone that moved to any other country at the same time #
##############################################################################

step9 <- 
  step8 %>% 
  filter(order_of_publishing == move_to_USA_publication_order) %>% 
  group_by(cluster_id) %>% 
  mutate(is_not_acceptable_country = if_else(pub_country != "United States", if_else(pub_country != origin_country, 1,0), 0),
         count_of_ineligable_countries = sum(is_not_acceptable_country)) %>%
  filter(count_of_ineligable_countries == 0) %>%  #at the point of becoming affilated with US, the only other affilations must be with origin country
  ungroup() %>% 
  select(names(step8))

length(unique(step9$cluster_id)) #5380
length(unique(step8$cluster_id)) -length(unique(step9$cluster_id)) #change = 159
###############################################################################
# STEP TEN: Exclude researchers who moved to close to the end of our dataset #
###############################################################################

#### because otherwise we don't have enough time to measure citation performance. We go with before or in 2016 (then at the least we can see performance in 2016,2017,2018,2019)

step10 <- step9 %>% 
  filter(move_to_USA_year <= 2016)

length(unique(step10$cluster_id)) #4768
length(unique(step9$cluster_id)) -length(unique(step10$cluster_id)) #change = 612
######################################################################################
# STEP ELEVEN: Exclude researchers were at the USA destination for less than 2 years #
######################################################################################

step11 <- step10 %>%  
  distinct(cluster_id, .keep_all = T) %>%
  select(cluster_id, USA_institution, move_to_USA_publication_order,move_to_USA_year, USA_lr_univ_id, years_between_starting_and_moving) %>%
  left_join(final_complete_dataset, by = "cluster_id") %>% 
  distinct(cluster_id, ut, pub_org_name, .keep_all = T) %>%
  arrange(cluster_id, desc(order_of_publishing)) %>% # First I arrange the publications of each researcher in reverse
  filter(USA_institution == pub_org_name) %>% # taking only rows where the mover if at their destination
  group_by(cluster_id) %>% 
  slice(1) %>% #take the first row (i.e. the last article at the destination)
  mutate(final_publication_at_destination_year = pub_year, #new variable: what year was the last published article
         years_at_destination = pub_year-move_to_USA_year) %>% #new variable: how many years was spent at destination institution
  filter(years_at_destination >=2) %>% #MOST IMPORTANT: here i keep only researchers who have publications that were published at the destination institution at least 2 years after their first publication at the destination
  select(cluster_id,USA_institution, USA_lr_univ_id, move_to_USA_publication_order,move_to_USA_year, final_article_at_destination_publication_order = order_of_publishing,final_article_at_destination_year = pub_year, years_at_destination, years_between_starting_and_moving ) %>% # neatening up the dataset a bit
  left_join(final_complete_dataset, by = "cluster_id") %>% #for our researchers that were at the destination for at least 2 years, i reattach their full publication profile
  arrange(cluster_id, order_of_publishing) %>%  #finally i rearrange the publications back into the normal order
  ungroup()

length(unique(step11$cluster_id)) #3102
length(unique(step10$cluster_id)) -length(unique(step11$cluster_id)) #change = 1666

###############################################################################
# STEP TWELVE: Exclude researchers who had only one publication at the origin #
###############################################################################

step12 <- step11 %>% 
  filter(move_to_USA_publication_order >= 3) 

length(unique(step12$cluster_id)) #2722
length(unique(step11$cluster_id)) -length(unique(step12$cluster_id)) #change = 380

######################################################################
# STEP THIRTEEN: Exclude researchers with missing gender information #
######################################################################

step13 <- step12 %>% 
  filter(!is.na(gender)) 

length(unique(step13$cluster_id)) #2598
length(unique(step12$cluster_id)) -length(unique(step13$cluster_id)) #change = 124
###################################################################################################################
# STEP FORTEEN: Calculate QS/Leiden ranking difference from origin to destination and make final movers dataset #
###################################################################################################################

mean_qs_ranking <- qs_ranking %>% 
  mutate(overall_rank_numeric = gsub("\\+|=","",overall_rank), 
         overall_rank_numeric = gsub("-.*","",overall_rank_numeric),
         overall_rank_numeric = as.numeric(overall_rank_numeric)) %>% 
  group_by(grid_id) %>% 
  summarise(is_in_qs_ranking = TRUE,
            qs_overall_rank_mean = mean(overall_rank_numeric, na.rm=T),
            qs_overall_score_mean = mean(overall_score, na.rm=T),
            qs_reputation_score_mean = mean(reputation_score, na.rm=T)) %>% 
  mutate(qs_overall_rank_quartiles = ntile(qs_overall_rank_mean, 20))

mean_leiden_ranking <- leiden_ranking %>% select(wos_name, Period, PP_top10,lr_univ_id) %>% 
  group_by(wos_name) %>% 
  summarise(pp_top10_mean = mean(PP_top10, na.rm=T),
            lr_univ_id = first(lr_univ_id)) %>% 
  mutate(pp_top10_mean_quantile = 21-ntile(pp_top10_mean, 20))

leiden_pp10_our_institutes <- 
  step13 %>% 
  distinct(lr_univ_id) %>% 
  filter(!is.na(lr_univ_id)) %>% 
  left_join(mean_leiden_ranking, by = "lr_univ_id") %>% 
  select(wos_name, pp_top10_mean, pp_top10_mean_quantile)

origin_institution_rankings_and_type  <- step13 %>% distinct(origin_institution) %>% 
  left_join(institute_grid_ids, by = c("origin_institution" = "wos_institute_name")) %>%  #add QS ranking for year they moved to USA
  left_join(mean_qs_ranking, by = c("grid_id" = "grid_id")) %>% 
  left_join(leiden_pp10_our_institutes, by = c("origin_institution"= "wos_name")) %>% 
  rename_with(.fn = ~ paste0("origin_", .x)) %>% 
  select(origin_institution = origin_origin_institution, everything())

USA_institution_rankings_and_type <- step13 %>% distinct(USA_institution) %>% 
  left_join(institute_grid_ids, by = c("USA_institution" = "wos_institute_name")) %>%  #add QS ranking for year they moved to USA
  left_join(mean_qs_ranking, by = c("grid_id" = "grid_id")) %>% 
  left_join(leiden_pp10_our_institutes, by = c("USA_institution"= "wos_name")) %>% 
  rename_with(.fn = ~ paste0("USA_", .x)) %>% 
  select(USA_institution = USA_USA_institution, everything())

movers_dataset_final <- step13 %>% 
  left_join(origin_institution_rankings_and_type, by = "origin_institution") %>% 
  left_join(USA_institution_rankings_and_type, by ="USA_institution") %>% 
  mutate(#difference_in_qs_overall_score = origin_qs_overall_score_mean-USA_qs_overall_score_mean,
         #difference_in_qs_reputation_score = origin_qs_reputation_score_mean-USA_qs_reputation_score_mean,
         difference_in_qs_overall_ranking_quantile = origin_qs_overall_rank_quartiles-USA_qs_overall_rank_quartiles, #a higher score represents moving upwards in ranking in USA
         #difference_in_pptop10 = origin_pp_top10_mean-USA_pp_top10_mean,
         difference_in_pptop10_quantile = origin_pp_top10_mean_quantile-USA_pp_top10_mean_quantile, #a higher score represents moving upwards in ranking in USA
         gelman_difference_in_qs_overall_score = effectsize::standardize(USA_qs_overall_score_mean-origin_qs_overall_score_mean, two_sd = T),
         gelman_difference_in_pptop10 = effectsize::standardize(USA_pp_top10_mean-origin_pp_top10_mean, two_sd=T)) #a higher score represents moving upwards
  
#how many eligible movers to USA do have?
length(unique(movers_dataset_final$cluster_id)) #2598
9140-length(unique(movers_dataset_final$cluster_id)) #excluded USA movers = 6542
movers_dataset_final %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
movers_dataset_final %>% distinct(cluster_id, .keep_all = T) %>% pull(discipline) %>% table()
table(movers_dataset_final %>% distinct(cluster_id, .keep_all = T) %>% pull(origin_country))  

#########################################################################################################
#########################################################################################################
########## GETTING A DATASET OF ALL RESEARCHERS WHO STAYED IN THEIR HOME COUNTRIES ("STAYERS") ##########
#########################################################################################################
#########################################################################################################

# making a dataset with publications only for authors that fulfill criteria:
# 1. Must have never moved to another country

############################################################################################
# STEP FIFTEEN: get cluster_ids of researchers who are potential comparisons to the movers #
############################################################################################

researchers_who_moved_anywhere <- 
  final_complete_dataset %>% #taking the main dataset
  group_by(cluster_id) %>% #then within each cluster ID I am going to check when people moved to a first new country
  arrange(cluster_id, order_of_publishing) %>% 
  mutate(origin_country = first(pub_country)) %>% 
  filter(!is.na(pub_country),
         pub_country != origin_country) %>% 
  distinct(cluster_id)

stayers_dataset <- final_complete_dataset %>% 
  anti_join(researchers_who_moved_anywhere, by = "cluster_id") #these cluster_ids never moved to a new country. We find matches for the movers in these cluster_ids

length(unique(stayers_dataset$cluster_id)) #145313
(length(unique(final_complete_dataset$cluster_id))-9140)-length(unique(stayers_dataset$cluster_id)) # how many non-movers, after removing non USA, were excluded because they moved to another countries


stayers_dataset_2 <- stayers_dataset %>% 
  filter(!is.na(gender))

length(unique(stayers_dataset_2$cluster_id)) #135739
length(unique(stayers_dataset$cluster_id)) -length(unique(stayers_dataset_2$cluster_id)) #9574 matches lost

stayers_dataset_3 <- stayers_dataset_2 %>% 
  group_by(cluster_id) %>% 
  mutate(total_number_of_articles = n_distinct(ut)) %>% 
  filter(total_number_of_articles >= 4)

length(unique(stayers_dataset_3$cluster_id)) #84092
length(unique(stayers_dataset_2$cluster_id)) -length(unique(stayers_dataset_3$cluster_id)) #11881 matches lost


#########################################################################################################
#########################################################################################################
######################## MEASURING RESEARCHER PERFORMANCE BY YEAR #######################################
#########################################################################################################
#########################################################################################################

researcher_performance <- final_complete_dataset %>% #this computes performance averaged within each career year. the bibliometric data is 3-year citation rate information.
  select(cluster_id, ut, order_of_publishing, career_year,n_coauthors) %>% 
  distinct(cluster_id, ut, .keep_all = T) %>% 
  left_join(citation_3year_info, by ="ut") %>% 
  mutate(njs_over_2 = if_else(njs_full >2, 1, 0),
         njs_over2_frac = if_else(njs_full > 2, p_frac, 0)) %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id, career_year) %>% 
  summarise(p_full_yearsum = sum(p_full),
            p_frac_yearsum = sum(p_frac),
            cs_full_yearsum = sum(cs_full),
            cs_frac_yearsum = sum(cs_frac),
            ncs_full_yearsum = sum(ncs_full),
            ncs_frac_yearsum = sum(ncs_frac),
            p_top_prop1_full_yearsum = sum(p_top_prop1_full),
            p_top_prop1_frac_yearsum = sum(p_top_prop1_frac),
            p_top_prop10_full_yearsum = round(sum(p_top_prop10_full),0),
            p_top_prop10_frac_yearsum = sum(p_top_prop10_frac),
            p_industry_yearsum = sum(p_industry),
            p_int_collab_yearsum = sum(p_int_collab),
            p_short_dist_collab_yearsum = sum(p_short_dist_collab),
            p_long_dist_collab_yearsum = sum(p_long_dist_collab),
            njs_full_over2_yearsum = sum(njs_over_2),
            njs_full_over2_frac_yearsum = sum(njs_over2_frac),
            cs_full_mean = mean(cs_full, na.rm=T),
            cs_frac_mean = mean(cs_frac, na.rm=T),
            ncs_full_mean = mean(ncs_full, na.rm=T),
            ncs_frac_mean = mean(ncs_frac, na.rm=T),
            js_full_median = median(cs_full, na.rm=T),
            js_full_mean = mean(js_full, na.rm=T),
            js_full_median = median(js_full, na.rm=T),
            js_frac_mean = mean(js_frac, na.rm=T),
            js_frac_median = median(js_frac, na.rm=T),
            njs_full_mean = mean(njs_full, na.rm=T),
            njs_full_median = median(njs_full, na.rm=T),
            njs_frac_mean = mean(njs_frac, na.rm=T),
            njs_frac_median = median(njs_frac, na.rm=T),
            n_coauthors_mean = mean(n_coauthors, na.rm=T),
            n_coauthors_median = median(n_coauthors, na.rm=T),) %>% 
  mutate( #this is required for the matching and for the "cumulative_researcher_performance_years" dataframe
    js_full_cummean = cummean(js_full_mean),
    js_frac_cummean = cummean(js_frac_mean),
    njs_full_cummean = cummean(njs_full_mean),
    njs_frac_cummean = cummean(njs_frac_mean),
    cs_mean_full_cummean = cummean(cs_full_mean),
    n_coauthors_cummean = cummean(n_coauthors_median), 
    n_coauthors_cummedian = cummedian(n_coauthors_median)) %>%
  ungroup()

researcher_years_prepstep <- researcher_performance %>% #i want to fill out the dataset, so even if say a researcher didn't publish anything in their 3rd year that we still have a row for that individuals third year (filled with NA). This is especially useful if we want to measuring cumulative performance 
  select(cluster_id, career_year, p_full_yearsum) %>% 
  spread(cluster_id, p_full_yearsum, fill = 0) %>% 
  gather(cluster_id, p_full_yearsum, -career_year) %>% 
  mutate(cluster_id = as.double(cluster_id)) %>% 
  select(cluster_id, career_year) 
  
researcher_performance_years <- #this is the final dataset that I use in the analysis
  researcher_years_prepstep %>% 
  left_join(researcher_performance, by = c("cluster_id", "career_year")) %>% 
  mutate(across(ends_with("yearsum"), replace_na, 0)) %>% #this replaces NAs in the yearly sum columns to 0, so therefore we can easily sum the column to count total performance
  fill(contains("cumm")) %>% #this fills up any NAs with the previous not NA value. Good for cumulative values.
  arrange(cluster_id, career_year) %>% 
  group_by(cluster_id) %>% 
  mutate(p_full_cumsum = cumsum(p_full_yearsum),
         p_frac_cumsum = cumsum(p_frac_yearsum),
         cs_frac_cumsum = cumsum(cs_frac_yearsum),
         ncs_frac_cumsum = cumsum(ncs_frac_yearsum),
         p_top_prop10_full_cumsum = cumsum(p_top_prop10_full_yearsum),
         p_top_prop10_frac_cumsum = cumsum(p_top_prop10_frac_yearsum),
         int_collab_cumsum = cumsum(p_int_collab_yearsum)) %>% 
  ungroup()


# this is smaller version of the researcher_performance_years dataset used for matching only #
cumulative_researcher_performance_years <- 
  researcher_performance_years  %>% 
  select(cluster_id, career_year, ends_with(c("cumsum","cummean", "cummedian")))

### a final note ###
# The main datasets we get out of this script that are used later on are:
# 1. final_complete_dataset - contains all descriptive information about potential researchers to be included in the analysis (both movers and stayers) + UTs
# 2. movers_dataset_final - contains all descriptive information about movers + UTs + ranking information [most important info is probably just the cluster_ids]
# 3. stayers_dataset_3 - contains all descriptive information about eligable "stayers" + UTs [most important info is probably just the cluster_ids]
# 4. researcher_performance_years - for all researchers in final_complete_dataset, this contains per-career-year performance information
# 5. cumulative_researcher_performance_years - for all researchers in final_complete_dataset, this contains CUMULATIVE performance information per career year.
###################