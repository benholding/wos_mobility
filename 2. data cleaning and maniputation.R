source("1. importing data.R") #importing data and packages
#identifying movers

###########################################################
### GETTING A DATASET OF ALL ELIGIBLE AUTHORS IN EUROPE ###
###########################################################

length(unique(publication_list_all$cluster_id)) #at this point we have 405,534 researchers

# making a dataset with publications only for authors that fulfill criteria:
# 1. only allowed  affilations from a single city at the earliest publication timepoint
# 2. only European institutions (my way of doing it results in excluding any authors where their first publications was a non-leiden ranked institution)
# 3. only biomedical/health researchers

# one_institution_authors <- publication_list_all %>% #take the main dataset
#   ungroup() %>% 
#   filter(order_of_publishing == 1) %>% #include only rows where it represents the earliest time for that author
#           #some authors published more than one article at their earliest timepoint. Here I specify that we only take authors that published 1 or max 2 articles at the earliest timepoint
#   group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because they published 2 articles at the same time) i need to check that both articles include the same affilation
#   mutate(number_of_distinct_affilations = n_distinct(pub_org)) %>% #checking if the rows have the same affilation
#   filter(number_of_distinct_affilations == 1, #ensuring that only cluster_ids with only 1 distinct affilation are included
#          !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
#   select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "publication_list_all" dataframe, which should result in us being left with only the rows in "publication_list_all" that match with the cluster_ids we identified as being eligible 
#   distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
#   left_join(publication_list_all, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 1
#   ungroup()
# 
# length(unique(one_institution_authors$cluster_id)) #at this point we have 294,282 researchers

# one_city_authors <- publication_list_all %>% #take the main dataset
#   ungroup() %>% 
#   filter(order_of_publishing == 1) %>% #include only rows where it represents the earliest time for that author.
#   group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because they published 2 articles at the same time) i need to check that both articles include the same city
#   mutate(number_of_distinct_cities = n_distinct(pub_city)) %>% #checking if the rows have the same city affilation
#   filter(number_of_distinct_cities == 1, #ensuring that only cluster_ids with only 1 distinct city affilation are included
#          !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
#   select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "publication_list_all" dataframe, which should result in us being left with only the rows in "publication_list_all" that match with the cluster_ids we identified as being eligible 
#   distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
#   left_join(publication_list_all, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 1
#   ungroup()
# 
# length(unique(one_city_authors$cluster_id)) # we now have 337,623 researchers

one_country_authors <- publication_list_all %>% #take the main dataset
  filter(order_of_publishing == 1) %>% #include only rows where it represents the earliest article for that author.
  group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because some have multiple affilations) i need to check that all affilations are from the same country
  mutate(number_of_distinct_countries = n_distinct(pub_country)) %>% #checking if the rows have the same country affilation
  filter(number_of_distinct_countries == 1, #ensuring that only cluster_ids with only 1 distinct country affilation are included
         !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
  select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "publication_list_all" dataframe, which should result in us being left with only the rows in "publication_list_all" that match with the cluster_ids we identified as being eligible 
  distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
  left_join(publication_list_all, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 1
  ungroup()

length(unique(one_country_authors$cluster_id)) # we now have 386,539 researchers

## here i'm keeping only european authors. These two next steps SHOULD of led to the same value, but since I've discovered that I can use UT as a time marker, it doesn't quite now (in code 0. I used min year/month to get data from WoS)
#this is the code for including authors from any european institution at start:

only_european_authors_all <- one_country_authors %>% #taking the just made dataset
  filter((pub_country == "Austria" | pub_country == "Belgium" | pub_country == "Croatia" | pub_country == "Cyprus" | pub_country == "Czech Republic" | pub_country == "Denmark" | pub_country == "Estonia" | pub_country == "Finland" | pub_country == "France" | pub_country == "Germany" | pub_country == "Greece" | pub_country == "Hungary" | pub_country == "Iceland" | pub_country == "Ireland" | pub_country == "Italy" | pub_country == "Lithuania" | pub_country == "Luxembourg" | pub_country == "Netherlands" | pub_country == "Norway" | pub_country == "Poland" | pub_country == "Portugal" | pub_country == "Romania" | pub_country == "Serbia" | pub_country == "Slovakia" | pub_country == "Slovenia" | pub_country == "Spain" | pub_country == "Sweden" | pub_country == "Switzerland" | pub_country == "United Kingdom") & order_of_publishing == 1) %>% #keeping only authors where at their earliest article they are affiliated to a "European" institution.
  select(cluster_id) %>% #identifying which authors are left
  distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had multiple affiliations)
  left_join(publication_list_all, by = "cluster_id") #now we have a dataset that fulfills criteria 2

length(unique(only_european_authors_all$cluster_id)) #at this point we have 129,913 researchers

#this is the code for including only leiden ranked european authors at start:
only_european_authors_leidenonly <- one_country_authors %>% #taking the just made dataset
  filter(lr_region == "eu" & order_of_publishing == 1) %>% #keeping only authors where at their earliest article they are affiliated to a "European" (according to Leiden ranking region variable) institution. This also removes anyone not affilated with a leiden ranked university at earliest timepoint
  select(cluster_id) %>% #identifying which authors are left
  distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had multiple affiliations)
  left_join(publication_list_all, by = "cluster_id") #now we have a dataset that fulfills criteria 2

length(unique(only_european_authors_leidenonly$cluster_id)) #at this point we have 129,597 researchers

#in order to decide on what disciplines should be included, I looked at the coverage
# publication_info %>%
#   distinct(ut, .keep_all = T) %>%
#   group_by(discipline) %>%
#   summarise(proportion_of_refs_covered = mean(n_refs_1980_covered/n_refs, na.rm =T)) %>% 
#   filter(proportion_of_refs_covered >= .6) #Biology, Biomedical Research, Chemistry, Clinical Medicine, Earth and Space, Engineering &  tech, Health, Maths, Physics & Psychology are all >60% covered

only_wos_covered_authors <- only_european_authors_leidenonly %>% #taking the just made dataset
  left_join(publication_info %>% #joining it with the publication_info dataset, in order to get discipline information for each publication
              distinct(ut, .keep_all=T), #since the publication_info dataset has duplicates, i do this to just get a single row per publication
            by = c("ut")) %>% 
  group_by(cluster_id) %>% 
  count(discipline) %>% # i then count how many articles have each disciples per author
  slice_max(n) %>% #then i make a dataframe showing the discipline per author with the highest number of articles
  filter(discipline == "Biology"| discipline == "Biomedical Research" | discipline == "Chemistry"| discipline == "Clinical Medicine" | discipline == "Earth and Space"|  discipline == "Engineering and Technology" | discipline == "Health" | discipline == "Mathematics" |discipline == "Physics" | discipline == "Psychology") %>% #i keep only authors where their most popular discipline is >=60% covered
  select(cluster_id, discipline) %>% #selecting the remaining authors (and also their discipline)
  distinct(cluster_id, .keep_all = T) %>% #making sure our selection is only distinct authors
  left_join(only_european_authors_leidenonly, by = "cluster_id") %>%  #joining our selection of authors from only our chosen disciplines, with the information present in the previous dataframe.
  ungroup()

length(unique(only_wos_covered_authors$cluster_id)) #at this point we have 122,933 researchers

# only_full_names <- only_wos_covered_authors %>% #here i do a couple of final changes
#   inner_join(eu_univ_1176_eligible_researchers %>% #to improve name disambiguation accuracy I want to only include authors if we have a first name
#               filter(!is.na(first_name)) %>% #filtering our names where first_name is NA
#               distinct(cluster_id, .keep_all = T) %>% 
#               select(cluster_id, full_name, first_name, gender, gender_estimation_accuracy = accuracy), 
#             by = "cluster_id")
# 
# length(unique(only_full_names$cluster_id)) #at this point we have 117,304 researchers
# 
# 
# final_dataset <- only_full_names %>% 
#   group_by(cluster_id) %>% #then I want to make sure we don't include any freak mistakes in wos name disambiguation. I noticed that some authors had HUGE numbers of publications
#   mutate(n_pubs = n_distinct(ut)) %>% #measuring the number of unique UTs per author
#   filter(n_pubs <= 200) %>% #only keeping in authors if they have less than or equal to 200 unique UTs
#   left_join(publication_info %>% distinct(ut, .keep_all=T), by = c("ut", "pub_year")) %>%  #adding more information about each publication
#   ungroup()
# 
# length(unique(final_dataset$cluster_id)) #at this point we have 117,236 researchers

final_complete_dataset <- only_wos_covered_authors

###########################################################
### GETTING A DATASET OF ALL ELIGIBLE MOVERS TO THE USA ###
###########################################################

#how many people move to USA
final_complete_dataset %>% filter(pub_country == "United States") %>% distinct(cluster_id) %>% summarise(n_movers_to_USA=n()) #7452
final_complete_dataset %>% filter(lr_country_name == "United States") %>% distinct(cluster_id) %>% summarise(n_movers_to_USA=n()) #6529

#now I need to find the people who moved to the USA and it was their first move
movers_to_USA_dataset <- 
  final_complete_dataset %>% #taking the main dataset
  arrange(cluster_id, order_of_publishing) %>% #Then I arrange the dataset, so that the order_of_publishing 1 is always first for each cluster_id 
  group_by(cluster_id) %>% #then within each cluster ID I am going to check when people moved to a first new country
  mutate(new_country_compared_to_lag1 = if_else(pub_country != lag(pub_country), TRUE, FALSE), #so here I compare a persons pub_country at order_of_publishing x to pub_country at time x-1. Is it changes it puts a TRUE in the column
         new_country_compared_to_lag1_is_duplicate = duplicated(new_country_compared_to_lag1), #since we only want to see the first move, I made another column that puts a TRUE if a TRUE has already been seen for that subject in the new_country_compared_to_lag1 variable
         is_first_new_country = if_else(new_country_compared_to_lag1 == TRUE & new_country_compared_to_lag1_is_duplicate == FALSE, TRUE, FALSE), #then using the two previous variables, I make a final variable that shows the publication where a first move was made
         origin_country = first(pub_country)) %>%  #this is nothing to do with the above 3 lines. I just wanted to add a variable that states what the origin country is for each cluster_ID
  filter(is_first_new_country == TRUE & pub_country == "United States") %>% 
  ungroup()

length(unique(movers_to_USA_dataset$cluster_id)) #5752

#Then I need to find the people that moved only after a specific length of time. Below I work out subgroups for those who moved at least 3, 4 or 5 years after they started

movers_3_years_min <- movers_to_USA_dataset %>% 
  group_by(cluster_id) %>% 
  mutate(years_between_starting_and_moving = pub_year-first_year) %>% 
  filter(years_between_starting_and_moving >= 3) %>% 
  select(cluster_id, origin_country,first_year, move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, move_to_USA_month = months_numeric_null_is_min_of_year, years_between_starting_and_moving) %>% 
  left_join(final_complete_dataset, by = "cluster_id")

length(unique(movers_3_years_min$cluster_id)) #4398
movers_3_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
movers_3_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% hist(50)

# movers_4_years_min <- movers_to_USA_dataset %>% 
#   group_by(cluster_id) %>% 
#   mutate(years_between_starting_and_moving = pub_year-first_year) %>% 
#   filter(years_between_starting_and_moving >= 4) %>% 
#   select(cluster_id, origin_country,first_year, move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, move_to_USA_month = months_numeric_null_is_min_of_year, years_between_starting_and_moving) %>% 
#   left_join(final_complete_dataset, by = "cluster_id")
#
# length(unique(movers_4_years_min$cluster_id)) #3345
# movers_4_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
# movers_4_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% hist(50)
# 
# movers_5_years_min <- movers_to_USA_dataset %>% 
#   group_by(cluster_id) %>% 
#   mutate(years_between_starting_and_moving = pub_year-first_year) %>% 
#   filter(years_between_starting_and_moving >= 5) %>% 
#   select(cluster_id, origin_country,first_year, move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, move_to_USA_month = months_numeric_null_is_min_of_year, years_between_starting_and_moving) %>% 
#   left_join(final_complete_dataset, by = "cluster_id")
# 
# length(unique(movers_5_years_min$cluster_id)) #2430
# movers_5_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
# movers_5_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% hist(50)

#next we require our movers to be at the destination for a set number of years. Below I have code for 3, 4 or 5 years

only_usa_destination <- 
  movers_3_years_min %>% 
  filter(order_of_publishing == move_to_USA_publication_order) %>% 
  group_by(cluster_id) %>% 
  mutate(is_not_acceptable_country = if_else(pub_country != "United States", if_else(pub_country != origin_country, 1,0), 0),
         count_of_ineligable_countries = sum(is_not_acceptable_country)) %>%
  filter(count_of_ineligable_countries == 0) %>%  #at the point of becoming affilated with US, the only other affilations must be with origin country
  ungroup()

length(unique(only_usa_destination$cluster_id)) #4230

only_usa_destination_leiden_ranked <- 
  only_usa_destination %>% 
  filter(lr_country_name == "United States") #there must be at least one affilation at this point with a leiden ranked institution

length(unique(only_usa_destination_leiden_ranked$cluster_id)) #3557

only_usa_destination_leiden_ranked_single <- only_usa_destination_leiden_ranked %>% 
  group_by(cluster_id) %>% 
  mutate(n_distinct_leidenranked_USinstitues = n_distinct(lr_univ_id)) %>% 
  filter(n_distinct_leidenranked_USinstitues == 1) #here we filter so that eligible authors must be only affilated to 1 leiden ranked institution in the USA (otherwise how do we know how good the institution is?)

length(unique(only_usa_destination_leiden_ranked_single$cluster_id)) #3506

### we also need to remove people who moved to close to the end of our dataset. Because otherwise we don't have enough time to measure citation performance. We go with 5 years (2015,2016,2017,2018,2019)

movers2015_or_earlier <- only_usa_destination_leiden_ranked_single %>% 
  filter(move_to_USA_year <= 2015)

length(unique(movers2015_or_earlier$cluster_id)) #2607


### then we only include people that were at the destination for at least 3 years

movers_dataset_3yearsmin <- movers2015_or_earlier %>%   
  distinct(cluster_id, .keep_all = T) %>%
  select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month,USA_institution = lr_univ_name, USA_lr_univ_id = lr_univ_id) %>% 
  left_join(final_complete_dataset, by = "cluster_id") %>%
  distinct(cluster_id, ut, lr_univ_id, .keep_all = T) %>% 
  mutate(is_destination_affilation = if_else(USA_lr_univ_id == lr_univ_id, 1, 0),
         how_many_publications_at_destination_affilation = sum(is_destination_affilation, na.rm=T)) %>%
  arrange(cluster_id, desc(order_of_publishing)) %>% 
  filter(lr_univ_id == USA_lr_univ_id) %>% 
  slice(1) %>% 
  mutate(final_publication_at_destination_year = pub_year,
         final_publication_at_destination_month = months_numeric_null_is_min_of_year,
         years_at_destination = pub_year-move_to_USA_year) %>% 
  filter(years_at_destination >=3) %>% 
  select(cluster_id, origin_country,USA_institution, USA_lr_univ_id, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month, final_article_at_destination_publication_order = order_of_publishing,final_article_at_destination_year = pub_year,final_article_at_destination_month = months_numeric_null_is_min_of_year, years_at_destination) %>% 
  left_join(final_complete_dataset, by = "cluster_id") %>% 
  arrange(cluster_id, order_of_publishing)

#how many eligible movers to USA do have?
length(unique(movers_dataset_3yearsmin$cluster_id)) #1315
movers_dataset_3yearsmin %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
movers_dataset_3yearsmin %>% distinct(cluster_id, .keep_all = T) %>% pull(discipline) %>% table()
table(movers_dataset_3yearsmin %>% distinct(cluster_id, .keep_all = T) %>% pull(origin_country))

# movers_dataset_4yearsmin <- only_usa_destination_leiden_ranked_single %>%   
#   distinct(cluster_id, .keep_all = T) %>%
#   select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month,USA_institution = lr_univ_name, USA_lr_univ_id = lr_univ_id) %>% 
#   left_join(final_complete_dataset, by = "cluster_id") %>%
#   distinct(cluster_id, ut, lr_univ_id, .keep_all = T) %>% 
#   mutate(is_destination_affilation = if_else(USA_lr_univ_id == lr_univ_id, 1, 0),
#          how_many_publications_at_destination_affilation = sum(is_destination_affilation, na.rm=T)) %>%
#   arrange(cluster_id, desc(order_of_publishing)) %>% 
#   filter(lr_univ_id == USA_lr_univ_id) %>% 
#   slice(1) %>% 
#   mutate(final_publication_at_destination_year = pub_year,
#          final_publication_at_destination_month = months_numeric_null_is_min_of_year,
#          years_at_destination = pub_year-move_to_USA_year) %>% 
#   filter(years_at_destination >=4) %>% 
#   select(cluster_id, origin_country,USA_institution, USA_lr_univ_id, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month, final_article_at_destination_publication_order = order_of_publishing,final_article_at_destination_year = pub_year,final_article_at_destination_month = months_numeric_null_is_min_of_year, years_at_destination) %>% 
#   left_join(final_complete_dataset, by = "cluster_id") %>% 
#   arrange(cluster_id, order_of_publishing)
# 
# #how many eligible movers to USA do have?
# length(unique(movers_dataset_4yearsmin$cluster_id)) #911

# movers_dataset_5yearsmin <- only_usa_destination_leiden_ranked_single %>%   
#   distinct(cluster_id, .keep_all = T) %>%
#   select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month,USA_institution = lr_univ_name, USA_lr_univ_id = lr_univ_id) %>% 
#   left_join(final_complete_dataset, by = "cluster_id") %>%
#   distinct(cluster_id, ut, lr_univ_id, .keep_all = T) %>% 
#   mutate(is_destination_affilation = if_else(USA_lr_univ_id == lr_univ_id, 1, 0),
#          how_many_publications_at_destination_affilation = sum(is_destination_affilation, na.rm=T)) %>%
#   arrange(cluster_id, desc(order_of_publishing)) %>% 
#   filter(lr_univ_id == USA_lr_univ_id) %>% 
#   slice(1) %>% 
#   mutate(final_publication_at_destination_year = pub_year,
#          final_publication_at_destination_month = months_numeric_null_is_min_of_year,
#          years_at_destination = pub_year-move_to_USA_year) %>% 
#   filter(years_at_destination >=5) %>% 
#   select(cluster_id, origin_country,USA_institution, USA_lr_univ_id, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month, final_article_at_destination_publication_order = order_of_publishing,final_article_at_destination_year = pub_year,final_article_at_destination_month = months_numeric_null_is_min_of_year, years_at_destination) %>% 
#   left_join(final_complete_dataset, by = "cluster_id") %>% 
#   arrange(cluster_id, order_of_publishing)
# 
# #how many eligible movers to USA do have?
# length(unique(movers_dataset_5yearsmin$cluster_id)) #528

#####################################################################################
######## finding a dataset of people that stayed in their home countries ############
#####################################################################################

researchers_who_moved_anywhere <- 
  final_complete_dataset %>% #taking the main dataset
  group_by(cluster_id) %>% #then within each cluster ID I am going to check when people moved to a first new country
  mutate(origin_country = first(pub_country)) %>%  #this is nothing to do with the above 3 lines. I just wanted to add a variable that states what the origin country is for each cluster_ID
  filter(!is.na(pub_country),
         pub_country != origin_country) %>% 
  distinct(cluster_id)

stayers_dataset <- final_complete_dataset %>% 
  anti_join(researchers_who_moved_anywhere, by = "cluster_id") %>% 
  group_by(cluster_id) %>% #then within each cluster ID I am going to check when people moved to a first new country
  mutate(origin_country = first(pub_country))  #this is nothing to do with the above 3 lines. I just wanted to add a variable that states what the origin country is for each cluster_ID
  
