source("1. importing data.R") #importing data and packages-.
#identifying movers

#STEP ONE#
# making a dataset with publications only for authors that fulfill criteria:
# 1. only allowed  affilations from a single city at the earliest publication timepoint
# 2. only European institutions (my way of doing it results in excluding any authors where their first publications was a non-leiden ranked institution)
# 3. only biomedical/health researchers

length(unique(author_info_per_publication$cluster_id)) #at this point we have 405,534 researchers

# one_institution_authors <- author_info_per_publication %>% #take the main dataset
#   ungroup() %>% 
#   filter(is_earliest_timepoint == 1 & number_of_publications_at_this_timeopoint < 5) %>% #include only rows where it represents the earliest time for that author
#           #some authors published more than one article at their earliest timepoint. Here I specify that we only take authors that published 1 or max 2 articles at the earliest timepoint
#   group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because they published 2 articles at the same time) i need to check that both articles include the same affilation
#   mutate(number_of_distinct_affilations = n_distinct(pub_org)) %>% #checking if the rows have the same affilation
#   filter(number_of_distinct_affilations == 1, #ensuring that only cluster_ids with only 1 distinct affilation are included
#          !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
#   select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "author_info_per_publication" dataframe, which should result in us being left with only the rows in "author_info_per_publication" that match with the cluster_ids we identified as being eligible 
#   distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
#   left_join(author_info_per_publication, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 1
#   ungroup()
# 
# length(unique(one_institution_authors$cluster_id)) #at this point we have 294,282 researchers

one_city_authors <- author_info_per_publication %>% #take the main dataset
  ungroup() %>% 
  filter(is_earliest_timepoint == 1 & number_of_publications_at_this_timeopoint < 5) %>% #include only rows where it represents the earliest time for that author. Also that authors only had maximum 4 articles at this time point
  group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because they published 2 articles at the same time) i need to check that both articles include the same city
  mutate(number_of_distinct_cities = n_distinct(pub_city)) %>% #checking if the rows have the same city affilation
  filter(number_of_distinct_cities == 1, #ensuring that only cluster_ids with only 1 distinct city affilation are included
         !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
  select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "author_info_per_publication" dataframe, which should result in us being left with only the rows in "author_info_per_publication" that match with the cluster_ids we identified as being eligible 
  distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
  left_join(author_info_per_publication, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 1
  ungroup()

length(unique(one_city_authors$cluster_id)) # we now have 337,623 researchers

only_european_authors <- one_city_authors %>% #taking the just made dataset
  filter(lr_region == "eu" & is_earliest_timepoint == 1) %>% #keeping only authors where at their earliest timepoint they are affiliated to a "European" (according to Leiden ranking region variable) institution. This also removes anyone not affilated with a leiden ranked university at earliest timepoint
  select(cluster_id) %>% #identifying which authors are left
  distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
  left_join(author_info_per_publication, by = "cluster_id") #now we have a dataset that fulfills criteria 2

length(unique(only_european_authors$cluster_id)) #at this point we have 116,839 researchers

# #in order to decide on what disciplines should be included, I looked at the coverage
# publication_info %>%
#   distinct(ut, .keep_all = T) %>%
#   group_by(discipline) %>%
#   summarise(proportion_of_refs_covered = mean(n_refs_1980_covered/n_refs, na.rm =T)) #Biology, Biomedical Research, Chemistry, Clinical Medicine, Earth and Space, Engineering &  tech, Health, Maths, Physics & Psychology are all >60% covered

only_wos_covered_authors <- only_european_authors %>% #taking the just made dataset
  left_join(publication_info %>% #joining it with the publication_info dataset, in order to get discipline information for each publication
              distinct(ut, .keep_all=T), #since the publication_info dataset has duplicates, i do this to just get a single row per publication
            by = c("ut")) %>% 
  group_by(cluster_id) %>% 
  count(discipline) %>% # i then count how many articles have each disciples per author
  slice_max(n) %>% #then i make a dataframe showing the discipline per author with the highest number of articles
  filter(discipline == "Biology"| discipline == "Biomedical Research" | discipline == "Chemistry"| discipline == "Clinical Medicine" | discipline == "Earth and Space"| discipline == "Biology"|  discipline == "Engineering and Technology" | discipline == "Health" | discipline == "Mathematics" |discipline == "Physics" | discipline == "Psychology") %>% #i keep only authors where their most popular discipline is >=60% covered
  select(cluster_id, discipline) %>% #selecting the remaining authors (and also their discipline)
  distinct(cluster_id, .keep_all = T) %>% #making sure our selection is only distinct authors
  left_join(only_european_authors, by = "cluster_id") %>%  #joining our selection of authors from only our chosen disciplines, with the information present in the previous dataframe.
  ungroup()

length(unique(only_wos_covered_authors$cluster_id)) #at this point we have 110,694 researchers
  
final_dataset <- only_wos_covered_authors %>% #here i do a couple of final changes
  inner_join(eu_univ_1176_eligible_researchers %>% #to improve name disambiguation accuracy I want to only include authors if we have a first name
              filter(!is.na(first_name)) %>% #filtering our names where first_name is NA
              distinct(cluster_id, .keep_all = T) %>% 
              select(cluster_id, full_name, first_name, gender, gender_estimation_accuracy = accuracy), 
            by = "cluster_id") %>% 
  group_by(cluster_id) %>% #then I want to make sure we don't include any freak mistakes in wos name disambiguation. I noticed that some authors had HUGE numbers of publications
  mutate(n_pubs = n_distinct(ut)) %>% #measuring the number of unique UTs per author
  filter(n_pubs <= 200) %>% #only keeping in authors if they have less than or equal to 200 unique UTs
  left_join(publication_info %>% distinct(ut, .keep_all=T), by = c("ut", "pub_year")) #adding more information about each publication
