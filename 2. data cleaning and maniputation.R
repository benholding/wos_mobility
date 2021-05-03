source("1. importing data.R") #importing data and packages-.
#identifying movers

#STEP ONE#
# making a dataset with publications only for authors that fulfill criteria:
# 1. only allowed one affiliation at the earliest publication timepoint
# 2. only European institutions (my way of doing it results in excluding any authors where their first publications was a non-leiden ranked institution)
# 3. only biomedical/health researchers

one_institution_authors <- author_info_per_publication %>% #take the main dataset
  filter(is_earliest_timepoint == 1 & author_affilations_count_per_ut == 1, #include only rows where it represents the earliest time for that author AND there must be only 1 affilation
         number_of_publications_at_this_timeopoint < 3) %>% #some authors published more than one article at their earliest timepoint. Here I specify that we only take authors that published 1 or max 2 articles at the earliest timepoint
  group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because they published 2 articles at the same time) i need to check that both articles include the same affilation
  mutate(number_of_distinct_affilations = n_distinct(pub_org)) %>% #checking if the rows have the same affilation
  filter(number_of_distinct_affilations == 1, #ensuring that only cluster_ids with 1 distinct affilation are included
         !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
  select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "author_info_per_publication" dataframe, which should result in us being left with only the rows in "author_info_per_publication" that match with the cluster_ids we identified as being eligible 
  distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
  left_join(author_info_per_publication, by = "cluster_id") #now we have a dataset that fulfills criteria 1

only_european_authors <- one_institution_authors %>% #taking the just made dataset
  filter(lr_region == "eu" & is_earliest_timepoint == 1) %>% #keeping only authors where at their earliest timepoint they are affiliated to a "European" (according to Leiden ranking region variable) institution
  select(cluster_id) %>% #identifying which authors are left
  distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
  left_join(author_info_per_publication, by = "cluster_id") #now we have a dataset that fulfills criteria 2

only_biomedical_authors <- only_european_authors %>% #taking the just made dataset
  left_join(publication_info %>% #joining it with the publication_info dataset, in order to get discipline information for each publication
              distinct(ut, .keep_all=T), #since the publication_info dataset has duplicates, i do this to just get a single row per publication
            by = c("ut")) %>% 
  group_by(cluster_id) %>% 
  count(discipline) %>% # i then count how many articles have each disciples per author
  slice_max(n) %>% #then i make a dataframe showing the discipline per author with the highest number of articles
  filter(discipline == "Biomedical Research"| discipline == "Clinical Medicine"| discipline == "Health") %>% #i keep only authors where their most popular discipline is either Biomedical Research, Clinical Medicine, or Health
  select(cluster_id) %>% #selecting the remainding authors
  distinct(cluster_id) %>% #making sure our selection is only distinct authors
  left_join(only_european_authors, by = "cluster_id") #joining our selection of biomedical authors, with the information present in the previous dataframe.
  
final_dataset <- only_biomedical_authors %>% #here i do a couple of final changes
  inner_join(eu_univ_1176_eligible_researchers %>% #to improve name disambiguation accuracy I want to only include authors if we have a first name
              filter(!is.na(first_name)) %>% #filtering our names where first_name is NA
              distinct(cluster_id, .keep_all = T) %>% 
              select(cluster_id, full_name, first_name, gender, gender_estimation_accuracy = accuracy), 
            by = "cluster_id") %>% 
  group_by(cluster_id) %>% #then I want to make sure we don't include any freak mistakes in wos name disambiguation. I noticed that some authors had HUGE numbers of publications
  mutate(n_pubs = n_distinct(ut)) %>% #measuring the number of unique UTs per author
  filter(n_pubs <= 150) %>% #only keeping in authors if they have less than or equal to 150 unique UTs
  left_join(publication_info %>% distinct(ut, .keep_all=T), by = c("ut", "pub_year")) #adding more information about each publication