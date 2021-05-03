source("2. data cleaning and maniputation.R") #importing data and packages

length(unique(final_dataset$ut)) #403,236 articles
length(unique(final_dataset$cluster_id)) #40,135 researchers

#How many affilations do people have?
table(final_dataset %>% distinct(cluster_id,ut, .keep_all = T) %>% pull(author_affilations_count_per_ut))
hist(final_dataset %>% distinct(cluster_id,ut, .keep_all = T) %>% pull(author_affilations_count_per_ut), 30)

#Number of countries authors are affilated in papers
table(final_dataset %>% group_by(cluster_id, ut) %>% mutate(n_pubcountries_per_paperauthor = n_distinct(lr_country_name)) %>% pull(n_pubcountries_per_paperauthor)) #Can we trust papers where authors have 3,4 or 5 countries of affilation?

#How many people move around?
final_dataset %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% distinct(cluster_id, .keep_all=T) %>% count(n_distinct_pubcountries_total) #some people were affilated with 6 countries
final_dataset %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% distinct(cluster_id, .keep_all=T) %>% count(n_distinct_pubcountries_total) %>% filter(n_distinct_pubcountries_total>1) %>% summarise(count_overall_movers = sum(n)) #19632

#now I need to find the people who moved to the USA and it was their first move and they stayed there for a while
movers_dataset <- 
  final_dataset %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>% 
  mutate(new_country_compared_to_lag1 = if_else(pub_country != lag(pub_country), TRUE, FALSE), new_country_compared_to_lag1_is_duplicate = duplicated(new_country_compared_to_lag1), is_first_new_country = if_else(new_country_compared_to_lag1 == TRUE & new_country_compared_to_lag1_is_duplicate == FALSE, TRUE, FALSE), origin_country = min(pub_country, na.rm = T)) %>% 
  filter(is_first_new_country == TRUE & order_of_publishing>5 & pub_country == "United States") %>% 
  select(cluster_id, origin_country, move_to_USA_publication_order = order_of_publishing, move_to_USA_year = pub_year, move_to_USA_month = months_numeric_null_is_min_of_year) %>% 
  left_join(final_dataset, by = "cluster_id") %>% 
  filter(order_of_publishing == move_to_USA_publication_order) %>% 
  group_by(cluster_id) %>% 
  filter(n_distinct(pub_country) < 3) %>% 
  mutate(is_not_acceptable_country = if_else(pub_country != "United States", if_else(pub_country != origin_country, 1,0), 0),
         count_of_ineligable_countries = sum(is_not_acceptable_country)) %>%
  filter(count_of_ineligable_countries == 0,
         lr_country_name == "United States") %>% 
  mutate(n_distinct_leidenranked_USinstitues = n_distinct(lr_univ_id)) %>% 
  filter(n_distinct_leidenranked_USinstitues == 1) %>% 
  distinct(cluster_id, .keep_all = T) %>%
  select(cluster_id, origin_country, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month,USA_institution = lr_univ_name, USA_lr_univ_id = lr_univ_id) %>% 
  left_join(final_dataset, by = "cluster_id") %>%
  distinct(cluster_id, ut, lr_univ_id, .keep_all = T) %>% 
  mutate(is_destination_affilation = if_else(USA_lr_univ_id == lr_univ_id, 1, 0),
         how_many_publications_at_destination_affilation = sum(is_destination_affilation, na.rm=T)) %>%
  filter(how_many_publications_at_destination_affilation >=3) %>%
  arrange(cluster_id, desc(order_of_publishing)) %>% 
  filter(lr_univ_id == USA_lr_univ_id) %>% 
  slice(1) %>% 
  select(cluster_id, origin_country,USA_institution, USA_lr_univ_id, move_to_USA_publication_order,move_to_USA_year, move_to_USA_month, final_article_at_destination_publication_order = order_of_publishing,final_article_at_destination_year = pub_year,final_article_at_destination_month = months_numeric_null_is_min_of_year) %>% 
  left_join(final_dataset, by = "cluster_id") %>% 
  arrange(cluster_id, order_of_publishing)
  
#where are our researchers coming from?
table(movers_dataset %>% distinct(cluster_id, .keep_all = T) %>% pull(origin_country))

#which US institutions are visited?
table(movers_dataset %>% distinct(cluster_id, .keep_all = T) %>% pull(USA_institution))





#STEP FIVE#
#understanding more about the movements of the researchers
countries_moved_to <- author_publications_3countrylimit %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% filter(n_distinct_pubcountries_total>1) %>% arrange(cluster_id, pub_year) %>% distinct(cluster_id, pub_country, .keep_all = T) %>% select(cluster_id, pub_country, pub_year) %>% group_by(cluster_id) %>%  mutate(move_n = row_number()-1, years_since_previousmove = pub_year-lag(pub_year)) %>% ungroup() %>%   count(pub_country)

#just for fun, I made a map with where people moved
library(rworldmap)

mapped_data <- joinCountryData2Map(countries_moved_to, joinCode = "NAME", 
                                   nameJoinColumn = "pub_country")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data, nameColumnToPlot = "n",colourPalette = "diverging",catMethod ="pretty",numCats=200)

