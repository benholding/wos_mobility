source("1. importing data.R") #importing data and packages
#identifying movers

pacman::p_load(cumstats)

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

length(unique(one_country_authors$cluster_id)) # we now have 386,543 researchers

## here i'm keeping only european authors. These two next steps SHOULD of led to the same value, but since I've discovered that I can use UT as a time marker, it doesn't quite now (in code 0. I used min year/month to get data from WoS)

          #this is the code for including authors from any european institution at start:
          # only_european_authors_all <- one_country_authors %>% #taking the just made dataset
          #   filter((pub_country == "Austria" | pub_country == "Belgium" | pub_country == "Croatia" | pub_country == "Cyprus" | pub_country == "Czech Republic" | pub_country == "Denmark" | pub_country == "Estonia" | pub_country == "Finland" | pub_country == "France" | pub_country == "Germany" | pub_country == "Greece" | pub_country == "Hungary" | pub_country == "Iceland" | pub_country == "Ireland" | pub_country == "Italy" | pub_country == "Lithuania" | pub_country == "Luxembourg" | pub_country == "Netherlands" | pub_country == "Norway" | pub_country == "Poland" | pub_country == "Portugal" | pub_country == "Romania" | pub_country == "Serbia" | pub_country == "Slovakia" | pub_country == "Slovenia" | pub_country == "Spain" | pub_country == "Sweden" | pub_country == "Switzerland" | pub_country == "United Kingdom") & order_of_publishing == 1) %>% #keeping only authors where at their earliest article they are affiliated to a "European" institution.
          #   select(cluster_id) %>% #identifying which authors are left
          #   distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had multiple affiliations)
          #   left_join(publication_list_all, by = "cluster_id") #now we have a dataset that fulfills criteria 2
          # length(unique(only_european_authors_all$cluster_id)) #at this point we have 129,913 researchers

#this is the code for including only (the commented code provides only leiden ranked) european authors at start:
european_countries <- read_csv("country_list.csv", col_names =F) %>% pull(X1)

          # only_european_authors_leidenonly <- one_country_authors %>% #taking the just made dataset
          #   filter(lr_country_name %in% european_countries,
          #          order_of_publishing == 1) %>% #keeping only authors where at their earliest article they are affiliated to a "European" (according to Leiden ranking region variable) institution. This also removes anyone not affilated with a leiden ranked university at earliest timepoint
          #   select(cluster_id) %>% #identifying which authors are left
          #   distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had multiple affiliations)
          #   left_join(publication_list_all, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 2
          #   group_by(cluster_id)
          # 
          # length(unique(only_european_authors_leidenonly$cluster_id)) #at this point we have 137,231 researchers

only_european_authors <- one_country_authors %>% #taking the just made dataset
  filter(pub_country %in% european_countries,
         order_of_publishing == 1) %>% #keeping only authors where at their earliest article they are affiliated to a "European" (according to Leiden ranking region variable) institution.
  select(cluster_id) %>% #identifying which authors are left
  distinct(cluster_id) %>% #keeping only distinct ones (since for some cluster_ids we would have 2 rows if they had multiple affiliations)
  left_join(publication_list_all, by = "cluster_id")  #now we have a dataset that fulfills criteria 2

length(unique(only_european_authors$cluster_id)) #at this point we have 137,625 researchers

# assessing the origin institution and adding the final month of publishing at the origin institution
assessing_origin_institution <- 
  only_european_authors %>% 
  filter(order_of_publishing == 1) %>% 
  select(cluster_id, pub_org_name) %>% 
  left_join(publication_list_all, by = c("cluster_id", "pub_org_name")) %>% 
  group_by(cluster_id, pub_org_name) %>% 
  mutate(number_of_publications_with_this_affilation = n()) %>% 
  distinct(cluster_id, pub_org_name, number_of_publications_with_this_affilation, .keep_all = T) %>% 
  select(cluster_id, pub_org_name, number_of_publications_with_this_affilation, lr_univ_id) %>% 
  group_by(cluster_id) %>% 
  arrange(cluster_id, desc(number_of_publications_with_this_affilation),lr_univ_id) %>% 
  mutate(origin_institution = first(pub_org_name)) %>% #this makes a variable of the origin institution. If there were multiple institutions at time 0, then this takes the instituion where the researcher had the most publications. if it is a tie, then it is selected alphabetically. NAs are always last to be chosen (but if all first affilations are NA then one will still be included)
  select(cluster_id, origin_institution) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  left_join(only_european_authors, by ="cluster_id") %>% 
  filter(origin_institution == pub_org_name) %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>%
  mutate(final_article_at_origin_career_month = last(career_length_months_at_this_pub),
         final_article_at_origin_order_of_publishing = last(order_of_publishing)) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  select(cluster_id, origin_institution, final_article_at_origin_career_month, final_article_at_origin_order_of_publishing) %>% 
  left_join(only_european_authors, by = "cluster_id") %>% 
  ungroup()

          #in order to decide on what disciplines should be included, I looked at the coverage
          # publication_info %>%
          #   distinct(ut, .keep_all = T) %>%
          #   group_by(discipline) %>%
          #   summarise(proportion_of_refs_covered = mean(n_refs_1980_covered/n_refs, na.rm =T)) %>% 
          #   filter(proportion_of_refs_covered >= .6) #Biology, Biomedical Research, Chemistry, Clinical Medicine, Earth and Space, Engineering &  tech, Health, Maths, Physics & Psychology are all >60% covered

only_wos_covered_authors <- assessing_origin_institution %>% #taking the just made dataset
  left_join(publication_info %>% #joining it with the publication_info dataset, in order to get discipline information for each publication
              distinct(ut, .keep_all=T), #since the publication_info dataset has duplicates, i do this to just get a single row per publication
            by = c("ut")) %>% 
  group_by(cluster_id) %>% 
  count(discipline) %>% # i then count how many articles have each disciples per author
  slice_max(n) %>% #then i make a dataframe showing the discipline per author with the highest number of articles
  filter(discipline == "Biology"| discipline == "Biomedical Research" | discipline == "Chemistry"| discipline == "Clinical Medicine" | discipline == "Earth and Space"|  discipline == "Engineering and Technology" | discipline == "Health" | discipline == "Mathematics" |discipline == "Physics" | discipline == "Psychology") %>% #i keep only authors where their most popular discipline is >=60% covered
  select(cluster_id, discipline) %>% #selecting the remaining authors (and also their discipline)
  distinct(cluster_id, .keep_all = T) %>% #making sure our selection is only distinct authors
  left_join(assessing_origin_institution, by = "cluster_id") %>%  #joining our selection of authors from only our chosen disciplines, with the information present in the previous dataframe.
  ungroup()

length(unique(only_wos_covered_authors$cluster_id)) #at this point we have 130,533 researchers

#i also wanted to include the most popular specialty and meso citation cluster. This is rubbish code but here we go...

specialty <- assessing_origin_institution %>% 
  left_join(publication_info %>% 
              distinct(ut, .keep_all=T), 
            by = c("ut")) %>% 
  group_by(cluster_id) %>% 
  add_count(specialty, name = "n_specialty_articles") %>% 
  add_count(discipline, name = "n_discipline_articles") %>% 
  distinct(cluster_id, discipline, specialty, .keep_all = T) %>% 
  select(cluster_id, discipline, n_discipline_articles,specialty, n_specialty_articles) %>% 
  filter(!is.na(discipline), 
         !is.na(specialty)) %>% 
  arrange(cluster_id, desc(n_discipline_articles), desc(n_specialty_articles)) %>% 
  slice(1) %>% 
  select(cluster_id, specialty) %>% 
  distinct(cluster_id, .keep_all = T)

meso_citation_cluster <- assessing_origin_institution %>% 
  left_join(publication_info %>% 
              distinct(ut, .keep_all=T), 
            by = c("ut")) %>% 
  group_by(cluster_id) %>% 
  count(cluster_id2) %>% 
  slice_max(n) %>% 
  select(cluster_id, meso_citation_cluster=cluster_id2) %>% 
  distinct(cluster_id, .keep_all = T)

final_complete_dataset <- only_wos_covered_authors %>% 
  left_join(specialty, by="cluster_id") %>% 
  left_join(meso_citation_cluster, by = "cluster_id") %>% 
  select(cluster_id, discipline, specialty, meso_citation_cluster, everything()) %>% 
  left_join(publication_info %>% select(ut, n_authors, n_countries), by = "ut") %>% #adding number of authors on paper, and number of countries
  mutate(n_coauthors = n_authors - 1)

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



###########################################################
### GETTING A DATASET OF ALL ELIGIBLE MOVERS TO THE USA ###
###########################################################

#### how many people move to USA ####
final_complete_dataset %>% filter(pub_country == "United States") %>% distinct(cluster_id) %>% summarise(n_movers_to_USA=n()) #7870

#### now I need to find the people who moved to the USA and it was their first move ####

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

length(unique(movers_to_USA_dataset$cluster_id)) #6096

#### Then I need to find the people that moved only after a specific length of time. Below I work out subgroups for those who moved at least 24 months after they started ####

movers_24_months_min <- movers_to_USA_dataset %>% #taking the dataset of researchers who moved to the USA as their first ever abroad affilation
  group_by(cluster_id) %>% #and within each cluster id...
  filter(career_length_months_at_this_pub >= 24) %>% #then we only keep participants who moved at least 24 months into their career
  select(cluster_id, origin_country,move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, months_between_starting_and_moving = career_length_months_at_this_pub) %>% #i'm selecting and renaming certain columns here to neaten things up
  left_join(final_complete_dataset, by = "cluster_id") #then i reattach all of the publications for each of our researchers which moved at or after month 24 (2 years)

length(unique(movers_24_months_min$cluster_id)) #5117

#### i need to check that they still affiliated to the origin institution for just before the move. Therefore we can pretty much say that they have been stationary at this university. OBS: only leiden unis count. ####

movers_with_same_instution <- movers_24_months_min %>% # so for all of our eligible movers...
  filter(order_of_publishing == move_to_USA_publication_order-1, #... we check that at the publication before the publication showing they had moved ot the USA...
         pub_org_name == origin_institution) %>% ###they were still affilated with their origin institution. The helps us know that they haven't moved loads before moving to the USA
  distinct(cluster_id) %>% #for those who pass this criteria I collect their cluster_ids....
  left_join(movers_24_months_min, by ="cluster_id") #... and then get the information from the movers_24_months_min dataset for those cluster_ids.

length(unique(movers_with_same_instution$cluster_id)) #4428

#### checking they only moved to the USA ####

only_usa_destination <- 
  movers_with_same_instution %>% 
  filter(order_of_publishing == move_to_USA_publication_order) %>% 
  group_by(cluster_id) %>% 
  mutate(is_not_acceptable_country = if_else(pub_country != "United States", if_else(pub_country != origin_country, 1,0), 0),
         count_of_ineligable_countries = sum(is_not_acceptable_country)) %>%
  filter(count_of_ineligable_countries == 0) %>%  #at the point of becoming affilated with US, the only other affilations must be with origin country
  ungroup() %>% 
  select(names(movers_24_months_min))

length(unique(only_usa_destination$cluster_id)) #4297

            # #### checking that the new affilation was leiden ranked ####
            # 
            # only_usa_destination_leiden_ranked <- 
            #   only_usa_destination %>% 
            #   filter(lr_country_name == "United States") #there must be at least one affilation at this point with a leiden ranked institution
            # 
            # length(unique(only_usa_destination_leiden_ranked$cluster_id)) #3922
            # 
            # #### checking that there was only 1 leiden ranked new affilation ####
            # 
            # only_usa_destination_leiden_ranked_single <- only_usa_destination_leiden_ranked %>% 
            #   group_by(cluster_id) %>% 
            #   mutate(n_distinct_leidenranked_USinstitues = n_distinct(lr_univ_id)) %>% 
            #   filter(n_distinct_leidenranked_USinstitues == 1) #here we filter so that eligible authors must be only affilated to 1 leiden ranked institution in the USA (otherwise how do we know how good the institution is?)
            # 
            # length(unique(only_usa_destination_leiden_ranked_single$cluster_id)) #3322

#### removing researchers who moved to close to the end of our dataset. ####
#### because otherwise we don't have enough time to measure citation performance. We go with before or in 2016 (then at the least we can see performance in 2016,2017,2018,2019)

movers2016_or_earlier <- only_usa_destination %>% 
  filter(move_to_USA_year <= 2016)

length(unique(movers2016_or_earlier$cluster_id)) #3785

###### assessing destination institution

# assessing the origin institution and adding the final month of publishing at the origin institution
assessing_destination_institution <- 
  movers2016_or_earlier %>% 
  select(cluster_id, pub_org_name, move_to_USA_publication_order) %>% 
  left_join(publication_list_all, by = c("cluster_id", "pub_org_name")) %>% 
  filter(order_of_publishing >= move_to_USA_publication_order) %>% 
  group_by(cluster_id, pub_org_name) %>% 
  mutate(number_of_publications_with_this_affilation = n()) %>% 
  distinct(cluster_id, pub_org_name, number_of_publications_with_this_affilation, .keep_all = T) %>% 
  select(cluster_id, pub_org_name, pub_country, number_of_publications_with_this_affilation, lr_univ_id) %>% 
  filter(pub_country == "United States") %>% 
  group_by(cluster_id) %>% 
  arrange(cluster_id, desc(number_of_publications_with_this_affilation),lr_univ_id) %>% 
  mutate(destination_institution = first(pub_org_name)) %>% #this makes a variable of the origin institution. If there were multiple institutions at time 0, then this takes the instituion where the researcher had the most publications. if it is a tie, then it is selected alphabetically. NAs are always last to be chosen (but if all first affilations are NA then one will still be included)
  select(cluster_id, destination_institution) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  left_join(movers2016_or_earlier, by ="cluster_id")

### then we only include people that were at the destination for at least 24 months

movers_at_destination_24monthsmin <- assessing_destination_institution %>%   #so of our eligible movers we...
  distinct(cluster_id, .keep_all = T) %>% #.... take those cluster_ids and...
  select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year,USA_institution = pub_org_name, USA_lr_univ_id = lr_univ_id, months_between_starting_and_moving) %>%
  left_join(final_complete_dataset, by = "cluster_id") %>% # ... get the full publication profiles of these researchers
  distinct(cluster_id, ut, pub_org_name, .keep_all = T) %>%
  mutate(is_destination_affilation = if_else(USA_institution == pub_org_name, 1, 0),
         how_many_publications_at_destination_affilation = sum(is_destination_affilation, na.rm=T)) %>% #here we make a new variable which is the sum of publications at the destinatino
  arrange(cluster_id, desc(order_of_publishing)) %>% #here i start to do the proper work. First I arrange the publications of each researcher in reverse
  filter(USA_institution == pub_org_name) %>% # taking only rows where the mover if at their destination
  group_by(cluster_id) %>% 
  slice(1) %>% #take the first row (i.e. the last article at the destination)
  mutate(final_publication_at_destination_year = pub_year, #a new variable: what year was the last published article
         final_publication_at_destination_careermonth = career_length_months_at_this_pub, #new variable: how many months since start of career was the last article at destination published
         years_at_destination = pub_year-move_to_USA_year, #new variable: how many years was spent at destination institution
         months_at_destination = final_publication_at_destination_careermonth-months_between_starting_and_moving) %>% #MOST IMPORTANT new variable: how many months were spent at destination
  filter(months_at_destination >=24) %>% #MOST IMPORTANT: here i keep only researchers who have publications that were published at the destination institution at least 24 months after their first publication
  select(cluster_id, origin_country,USA_institution, USA_lr_univ_id, move_to_USA_publication_order,move_to_USA_year,months_at_destination, final_article_at_destination_publication_order = order_of_publishing,final_article_at_destination_year = pub_year, years_at_destination, months_between_starting_and_moving, final_publication_at_destination_careermonth) %>% # neatening up the dataset a bit
  left_join(final_complete_dataset, by = "cluster_id") %>% #for our researchers that were at the destination for at least 24 months, i reattach their full publication profile
  arrange(cluster_id, order_of_publishing) #finally i rearrange the publications back into the normal order

movers_min2originpubs <- movers_at_destination_24monthsmin %>% filter(move_to_USA_publication_order > 2)

movers_dataset_final <- movers_min2originpubs
  
#how many eligible movers to USA do have?
length(unique(movers_dataset_final$cluster_id)) #2001
movers_dataset_final %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
movers_dataset_final %>% distinct(cluster_id, .keep_all = T) %>% pull(discipline) %>% table()
table(movers_dataset_final %>% distinct(cluster_id, .keep_all = T) %>% pull(origin_country))  

          ##### next we require our movers to be at the destination for a set number of years. Below I have code for 3, 4 or 5 years ####
          # movers_3_years_min <- movers_to_USA_dataset %>% 
          #   group_by(cluster_id) %>% 
          #   mutate(years_between_starting_and_moving = pub_year-first_year) %>% 
          #   filter(years_between_starting_and_moving >= 3) %>% 
          #   select(cluster_id, origin_country,first_year, move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, move_to_USA_month = months_numeric_null_is_min_of_year, years_between_starting_and_moving) %>% 
          #   left_join(final_complete_dataset, by = "cluster_id")
          # 
          # length(unique(movers_3_years_min$cluster_id)) #4398
          # movers_3_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
          # movers_3_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% hist(50)
          
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
  arrange(cluster_id, order_of_publishing) %>% 
  mutate(origin_country = first(pub_country)) %>% 
  filter(!is.na(pub_country),
         pub_country != origin_country) %>% 
  distinct(cluster_id)

stayers_dataset <- final_complete_dataset %>% 
  anti_join(researchers_who_moved_anywhere, by = "cluster_id") %>% 
  group_by(cluster_id) %>% #then within each cluster ID I am going to check when people moved to a first new country
  mutate(origin_country = first(pub_country))  #this is nothing to do with the above 3 lines. I just wanted to add a variable that states what the origin country is for each cluster_ID
  
###################################################################################
###################### Getting cumulative researcher performance ##################
###################################################################################


researcher_performance <- final_complete_dataset %>% 
  select(cluster_id, ut, order_of_publishing, career_length_months_at_this_pub,n_coauthors) %>% 
  distinct(cluster_id, ut, .keep_all = T) %>% 
  left_join(citation_3year_info, by ="ut") %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id, career_length_months_at_this_pub) %>% 
  summarise(month = first(career_length_months_at_this_pub),
            p_full_sum = sum(p_full),
            #p_frac = sum(p_frac),
            cs_full_sum = sum(cs_full),
            cs_full_mean = mean(cs_full),
            #cs_frac = sum(cs_frac),
            #ncs_full = sum(ncs_full),
            #ncs_frac = sum(ncs_frac),
            #p_top_prop1_full = sum(p_top_prop1_full),
            #p_top_prop1_frac = sum(p_top_prop1_frac),
            #p_top_prop10_full = sum(p_top_prop10_full),
            #p_top_prop10_frac = sum(p_top_prop10_frac),
            #js_full = mean(js_full),
            #js_frac = mean(js_frac),
            njs_full_mean = mean(njs_full),
            #njs_frac = mean(njs_frac),
            #p_industry = sum(p_industry),
            p_int_collab_sum = sum(p_int_collab),
            #p_short_dist_collab = sum(p_short_dist_collab),
            #p_long_dist_collab = sum(p_long_dist_collab)
            #n_coauthors_mean = mean(n_coauthors),
            n_coauthors_median = median(n_coauthors)) %>% 
  mutate( #this is required for the matching and for the "cumulative_researcher_performance_months" dataframe
    #js_full = cummean(js_full)
    #js_frac = cummean(js_frac),
    njs_full_cummean = cummean(njs_full_mean), #this is the only cummulative mean we are interested in. The others are cumulative sums
    #njs_frac_cummean = cummean(njs_frac),
    #cs_mean_full_cummean = cummean(cs_mean_full),
    n_coauthors_cummedian = cummedian(n_coauthors_median) #no wait, we also want the cumulative median 
    ) %>%
  ungroup() %>%
  select(-career_length_months_at_this_pub)

researcher_months_prepstep <- researcher_performance %>% 
  select(cluster_id, month, p_full) %>% 
  spread(cluster_id, p_full, fill = 0) %>% 
  gather(cluster_id, p_full, -month) %>% 
  mutate(cluster_id = as.double(cluster_id)) %>% 
  select(cluster_id, month) 
  
researcher_performance_months <- 
  researcher_months_prepstep %>% 
  left_join(researcher_performance, by = c("cluster_id", "month")) %>% 
  group_by(cluster_id) %>% 
  mutate(p_full_sum = replace_na(p_full_sum, 0),
         cs_full_sum = replace_na(cs_full_sum, 0),
         cs_full_mean = cs_full_mean, #for these mean ones, we want the NAs as they make it easier to calculate the means within the 1 year bins later
         njs_full_mean = njs_full_mean,
         p_int_collab_sum = replace_na(p_int_collab_sum, 0),
         n_coauthors_median = n_coauthors_median) %>% 
  fill(njs_full_cummean, n_coauthors_cummedian)

cumulative_researcher_performance_months <- 
  researcher_performance_months %>% 
  mutate(cum_p_full = cumsum(p_full_sum),
         #cum_p_frac = cumsum(p_frac),
         #cum_cs_frac = cumsum(cs_frac),
         #cum_ncs_frac = cumsum(ncs_frac),
         #cum_p_top_prop10_frac = cumsum(p_top_prop10_frac),
         cum_int_collab = cumsum(p_int_collab_sum),
         cum_n_coauthors = n_coauthors_cummedian,
         njs_full_mean = njs_full_cummean) %>%  #i don't need to do anything here, as this should already be the average NJS so far
  select(cluster_id, month, cum_p_full, cum_int_collab,cum_n_coauthors, njs_full_mean)

          # individual_researcher_performance_years <- final_complete_dataset %>% 
          #   select(cluster_id, ut, discipline, pub_year,order_of_publishing, career_length_months_at_this_pub) %>% 
          #   distinct(cluster_id, ut, .keep_all = T) %>% 
          #   left_join(citation_3year_info, by ="ut") %>% 
          #   arrange(cluster_id, order_of_publishing) %>% 
          #   group_by(cluster_id) %>% 
          #   mutate(scientific_year = (pub_year-first(pub_year))+1,
          #          n_articles_published= n_distinct(ut)) %>% 
          #   group_by(cluster_id, scientific_year) %>% 
          #   summarise(discipline = first(discipline),
          #             year = first(pub_year),
          #             p_full = sum(p_full),
          #             p_frac = sum(p_frac),
          #             cs_frac = sum(cs_frac),
          #             ncs_frac = sum(ncs_frac),
          #             p_top_prop10_frac = sum(p_top_prop10_frac),
          #             njs_frac = sum(njs_frac)) %>% 
          #   select(cluster_id, scientific_year, p_full) %>% 
          #   spread(cluster_id, p_full, fill = 0) %>% 
          #   gather(cluster_id, p_full, -scientific_year) %>% 
          #   mutate(cluster_id = as.double(cluster_id)) %>% 
          #   select(cluster_id, scientific_year) %>% 
          #   left_join(individual_researcher_performance_years, by = c("cluster_id", "scientific_year")) %>% 
          #   group_by(cluster_id) %>% 
          #   mutate(cum_p_full = cumsum(replace_na(p_full, 0)),
          #          cum_p_frac = cumsum(replace_na(p_frac, 0)),
          #          cum_cs_frac = cumsum(replace_na(cs_frac, 0)),
          #          cum_ncs_frac = cumsum(replace_na(ncs_frac, 0)),
          #          cum_p_top_prop10_frac = cumsum(replace_na(p_top_prop10_frac, 0)),
          #          cum_njs_frac = cumsum(replace_na(njs_frac, 0)),
          #          discipline = first(discipline)) %>% 
          #   select(cluster_id:discipline, cum_p_full:cum_njs_frac)
