source("2. data cleaning and maniputation.R") #importing data and packages

length(unique(final_dataset$ut)) #1,217,055 articles
length(unique(final_dataset$cluster_id)) #122,933 researchers

#How many affilations do people have?
table(final_dataset %>% distinct(cluster_id,ut, .keep_all = T) %>% pull(author_affilations_count_per_ut))
hist(final_dataset %>% distinct(cluster_id,ut, .keep_all = T) %>% pull(author_affilations_count_per_ut), 30)

#Number of countries authors are affilated in papers
table(final_dataset %>% group_by(cluster_id, ut) %>% mutate(n_pubcountries_per_paperauthor = n_distinct(lr_country_name)) %>% pull(n_pubcountries_per_paperauthor)) #Can we trust papers where authors have 3,4 or 5 countries of affilation?

#How many people move around?
final_dataset %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% distinct(cluster_id, .keep_all=T) %>% count(n_distinct_pubcountries_total) #some people were affilated with 6,7,8,9 countries
final_dataset %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% distinct(cluster_id, .keep_all=T) %>% count(n_distinct_pubcountries_total) %>% filter(n_distinct_pubcountries_total>1) %>% summarise(count_overall_movers = sum(n)) #61,377

#how many people move to USA
final_dataset %>% filter(pub_country == "United States") %>% distinct(cluster_id) %>% summarise(n_movers_to_USA=n()) #7452
final_dataset %>% filter(lr_country_name == "United States") %>% distinct(cluster_id) %>% summarise(n_movers_to_USA=n()) #6529


#now I need to find the people who moved to the USA and it was their first move
movers_to_USA_dataset <- 
  final_dataset %>% #taking the main dataset
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
  left_join(final_dataset, by = "cluster_id")

length(unique(movers_3_years_min$cluster_id)) #4398
movers_3_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
movers_3_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% hist(50)

# movers_4_years_min <- movers_to_USA_dataset %>% 
#   group_by(cluster_id) %>% 
#   mutate(years_between_starting_and_moving = pub_year-first_year) %>% 
#   filter(years_between_starting_and_moving >= 4) %>% 
#   select(cluster_id, origin_country,first_year, move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, move_to_USA_month = months_numeric_null_is_min_of_year, years_between_starting_and_moving) %>% 
#   left_join(final_dataset, by = "cluster_id")
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
#   left_join(final_dataset, by = "cluster_id")
# 
# length(unique(movers_5_years_min$cluster_id)) #2430
# movers_5_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
# movers_5_years_min %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% hist(50)

#next we require our movers to be at the destination for a set number of years

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

movers_dataset_3yearsmin <- only_usa_destination_leiden_ranked_single %>%   
  distinct(cluster_id, .keep_all = T) %>%
  select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month,USA_institution = lr_univ_name, USA_lr_univ_id = lr_univ_id) %>% 
  left_join(final_dataset, by = "cluster_id") %>%
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
  left_join(final_dataset, by = "cluster_id") %>% 
  arrange(cluster_id, order_of_publishing)

#how many eligible movers to USA do have?
length(unique(movers_dataset_3yearsmin$cluster_id)) #1478
movers_dataset_3yearsmin %>% distinct(cluster_id, .keep_all = T) %>% pull(move_to_USA_publication_order) %>% table()
movers_dataset_3yearsmin %>% distinct(cluster_id, .keep_all = T) %>% pull(discipline) %>% table()
table(movers_dataset_3yearsmin %>% distinct(cluster_id, .keep_all = T) %>% pull(origin_country))

# movers_dataset_4yearsmin <- only_usa_destination_leiden_ranked_single %>%   
#   distinct(cluster_id, .keep_all = T) %>%
#   select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month,USA_institution = lr_univ_name, USA_lr_univ_id = lr_univ_id) %>% 
#   left_join(final_dataset, by = "cluster_id") %>%
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
#   left_join(final_dataset, by = "cluster_id") %>% 
#   arrange(cluster_id, order_of_publishing)
# 
# #how many eligible movers to USA do have?
# length(unique(movers_dataset_4yearsmin$cluster_id)) #911

# movers_dataset_5yearsmin <- only_usa_destination_leiden_ranked_single %>%   
#   distinct(cluster_id, .keep_all = T) %>%
#   select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month,USA_institution = lr_univ_name, USA_lr_univ_id = lr_univ_id) %>% 
#   left_join(final_dataset, by = "cluster_id") %>%
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
#   left_join(final_dataset, by = "cluster_id") %>% 
#   arrange(cluster_id, order_of_publishing)
# 
# #how many eligible movers to USA do have?
# length(unique(movers_dataset_5yearsmin$cluster_id)) #528

#which US institutions are visited?
table(movers_dataset %>% distinct(cluster_id, .keep_all = T) %>% pull(USA_institution))


#looking at differentiation before move


x <- movers_dataset_3yearsmin %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>% 
  mutate(scientific_year = pub_year-first(pub_year)) %>% 
  filter(order_of_publishing < move_to_USA_publication_order) %>% 
  group_by(cluster_id) %>% 
  mutate(n_articles_published = n_distinct(ut)) %>% 
  group_by(discipline, scientific_year) %>% 
  summarise(article_published_mean = mean(n_articles_published),
            articles_published_sd = sd(n_articles_published))

y <- final_dataset %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>% 
  mutate(scientific_year = pub_year-first(pub_year)) %>% 
  group_by(cluster_id) %>% 
  mutate(n_articles_published = n_distinct(ut)) %>% 
  group_by(discipline, scientific_year) %>% 
  summarise(article_published_mean = mean(n_articles_published),
            articles_published_sd = sd(n_articles_published))


p<- ggplot(x, aes(x=scientific_year, y=article_published_mean, group=discipline, color=discipline)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=article_published_mean-articles_published_sd, ymax=article_published_mean+articles_published_sd), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap(~discipline)

ggplot(x %>% filter(discipline != "Physics"), aes(x=scientific_year, y=article_published_mean, group=discipline, color=discipline)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=article_published_mean-articles_published_sd, ymax=article_published_mean+articles_published_sd), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap(~discipline)



