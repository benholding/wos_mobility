#4. matching movers and non-movers
source("3. descriptive statistics.R") #importing data and packages

movers_to_USA_list <- movers_dataset_3yearsmin %>% distinct(cluster_id)
never_movers_list <- stayers_dataset %>% distinct(cluster_id)

researcher_basic_info <- final_complete_dataset %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>% 
  mutate(origin_country = first(pub_country)) %>% 
  select(cluster_id, discipline, full_name, first_name, first_year, last_year, gender, gender_accuracy, origin_country) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  mutate(moves_to_usa = if_else(cluster_id %in% movers_to_USA_list$cluster_id, TRUE, FALSE),
         stays_at_origin = if_else(cluster_id %in% never_movers_list$cluster_id, TRUE, FALSE))

only_eligible_researchers <- researcher_basic_info %>% 
  filter(stays_at_origin == TRUE | moves_to_usa == TRUE)


eligibles_performance <- only_eligible_researchers %>% 
  left_join(individual_researcher_performance, by = c("cluster_id", "discipline")) %>% 
  left_join(movers_dataset_3yearsmin %>% 
              select(cluster_id, move_to_USA_year, final_article_at_destination_year, years_at_destination) %>% 
              distinct(cluster_id, .keep_all = T)) %>% 
  mutate(origin_or_usa = if_else(stays_at_origin == TRUE, "origin", 
                                 if_else(year >= move_to_USA_year, "usa", "origin")))



stayers_dataset
  
  
movers_dataset <- 