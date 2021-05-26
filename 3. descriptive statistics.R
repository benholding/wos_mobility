source("2. data cleaning and maniputation.R") #importing data and packages

##########################################
#### basic demographics about the whole dataset ####

length(unique(final_complete_dataset$ut)) #1,217,055 articles
length(unique(final_complete_dataset$cluster_id)) #122,933 researchers

#How many affilations do people have?
table(final_complete_dataset %>% distinct(cluster_id,ut, .keep_all = T) %>% pull(author_affilations_count_per_ut))
hist(final_complete_dataset %>% distinct(cluster_id,ut, .keep_all = T) %>% pull(author_affilations_count_per_ut), 30)

#Number of countries authors are affilated in papers
table(final_complete_dataset %>% group_by(cluster_id, ut) %>% mutate(n_pubcountries_per_paperauthor = n_distinct(lr_country_name)) %>% pull(n_pubcountries_per_paperauthor)) #Can we trust papers where authors have 3,4 or 5 countries of affilation?

#How many people move around?
final_complete_dataset %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% distinct(cluster_id, .keep_all=T) %>% count(n_distinct_pubcountries_total) #some people were affilated with 6,7,8,9 countries
final_complete_dataset %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% distinct(cluster_id, .keep_all=T) %>% count(n_distinct_pubcountries_total) %>% filter(n_distinct_pubcountries_total>1) %>% summarise(count_overall_movers = sum(n)) #61,377



