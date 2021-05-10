source("1. importing data.R") #importing data and packages-.
#identifying movers

#STEP ONE#
# making a dataset with publications only for authors that fulfill criteria:
# 1. only allowed  affilations from a single city at the earliest publication timepoint
# 2. only European institutions (my way of doing it results in excluding any authors where their first publications was a non-leiden ranked institution)
# 3. only biomedical/health researchers

length(unique(publication_list_all$cluster_id)) #at this point we have 405,534 researchers

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

only_european_authors_all <- one_country_authors %>% #taking the just made dataset
  filter((pub_country == "Austria" | pub_country == "Belgium" | pub_country == "Croatia" | pub_country == "Cyprus" | pub_country == "Czech Republic" | pub_country == "Denmark" | pub_country == "Estonia" | pub_country == "Finland" | pub_country == "France" | pub_country == "Germany" | pub_country == "Greece" | pub_country == "Hungary" | pub_country == "Iceland" | pub_country == "Ireland" | pub_country == "Italy" | pub_country == "Lithuania" | pub_country == "Luxembourg" | pub_country == "Netherlands" | pub_country == "Norway" | pub_country == "Poland" | pub_country == "Portugal" | pub_country == "Romania" | pub_country == "Serbia" | pub_country == "Slovakia" | pub_country == "Slovenia" | pub_country == "Spain" | pub_country == "Sweden" | pub_country == "Switzerland" | pub_country == "United Kingdom") & order_of_publishing == 1) %>% #keeping only authors where at their earliest article they are affiliated to a "European" institution.
  select(cluster_id) %>% #identifying which authors are left
  distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had multiple affiliations)
  left_join(publication_list_all, by = "cluster_id") #now we have a dataset that fulfills criteria 2

length(unique(only_european_authors_all$cluster_id)) #at this point we have 129,913 researchers

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

final_dataset <- only_wos_covered_authors
  
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

