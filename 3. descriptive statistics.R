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

############################################################
####looking at performance differentiation for the first 5 years#########
############################################################

individual_researcher_performance <- final_complete_dataset %>% 
  select(cluster_id, ut, discipline, pub_year,order_of_publishing) %>% 
  distinct(cluster_id, ut, .keep_all = T) %>% 
  left_join(citation_3year_info, by ="ut") %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>% 
  mutate(scientific_year = (pub_year-first(pub_year))+1,
         n_articles_published= n_distinct(ut)) %>% 
  group_by(cluster_id, scientific_year) %>% 
  summarise(discipline = first(discipline),
            year = first(pub_year),
            p_full = sum(p_full),
            p_frac = sum(p_frac),
            cs_frac = sum(cs_frac),
            ncs_frac = sum(ncs_frac),
            p_top_prop10_frac = sum(p_top_prop10_frac),
            njs_frac = sum(njs_frac)) %>% 
  mutate(cum_p_full = cumsum(p_full),
         cum_p_frac = cumsum(p_frac),
         cum_cs_frac = cumsum(cs_frac),
         cum_ncs_frac = cumsum(ncs_frac),
         cum_p_top_prop10_frac = cumsum(p_top_prop10_frac),
         cum_njs_frac = cumsum(njs_frac))
  
individual_researcher_performance_first5years <-   
  individual_researcher_performance %>% 
  filter(scientific_year <=5)

average_researcher_performance  <- individual_researcher_performance_first5years %>% 
  group_by(scientific_year, discipline) %>% 
  mutate(median_cum_p_full = median(cum_p_full, na.rm = T),
         top5percent_cum_p_full = quantile(cum_p_full, 0.95, na.rm=T),
         median_cum_p_frac = median(cum_p_frac, na.rm = T),
         top5percent_cum_p_frac = quantile(cum_p_frac, 0.95, na.rm=T),
         median_cum_cs_frac = median(cum_cs_frac, na.rm = T),
         top5percent_cum_cs_frac = quantile(cum_cs_frac, 0.95, na.rm=T),
         median_cum_ncs_frac = median(cum_ncs_frac, na.rm = T),
         top5percent_cum_ncs_frac = quantile(cum_ncs_frac, 0.95, na.rm=T),
         median_cum_p_top_prop10_frac = median(cum_p_top_prop10_frac, na.rm = T),
         top5percent_cum_p_top_prop10_frac = quantile(cum_p_top_prop10_frac, 0.95, na.rm=T),
         median_cum_njs_frac = median(cum_njs_frac, na.rm = T),
         top5percent_cum_njs_frac = quantile(cum_njs_frac, 0.95, na.rm=T)) %>% 
  distinct(scientific_year, discipline, .keep_all = T) %>% 
  select(scientific_year, discipline, median_cum_p_full:top5percent_cum_njs_frac) %>% 
  mutate(cluster_id = "9999999") %>% 
  arrange(discipline, scientific_year)

movers_individual_researcher_performance_first5years <- 
  movers_dataset_3yearsmin %>% 
  distinct(cluster_id) %>% 
  left_join(individual_researcher_performance_first5years, by = "cluster_id") %>% 
  arrange(discipline, scientific_year)

movers_average_researcher_performance <- movers_individual_researcher_performance_first5years %>% 
  group_by(scientific_year, discipline) %>% 
  mutate(movers_median_cum_p_full = median(cum_p_full, na.rm = T),
         movers_top5percent_cum_p_full = quantile(cum_p_full, 0.95, na.rm=T),
         movers_median_cum_p_frac = median(cum_p_frac, na.rm = T),
         movers_top5percent_cum_p_frac = quantile(cum_p_frac, 0.95, na.rm=T),
         movers_median_cum_cs_frac = median(cum_cs_frac, na.rm = T),
         movers_top5percent_cum_cs_frac = quantile(cum_cs_frac, 0.95, na.rm=T),
         movers_median_cum_ncs_frac = median(cum_ncs_frac, na.rm = T),
         movers_top5percent_cum_ncs_frac = quantile(cum_ncs_frac, 0.95, na.rm=T),
         movers_median_cum_p_top_prop10_frac = median(cum_p_top_prop10_frac, na.rm = T),
         movers_top5percent_cum_p_top_prop10_frac = quantile(cum_p_top_prop10_frac, 0.95, na.rm=T),
         movers_median_cum_njs_frac = median(cum_njs_frac, na.rm = T),
         movers_top5percent_cum_njs_frac = quantile(cum_njs_frac, 0.95, na.rm=T)) %>% 
  distinct(scientific_year, discipline, .keep_all = T) %>% 
  select(scientific_year, discipline, movers_median_cum_p_full:movers_top5percent_cum_njs_frac) %>% 
  mutate(cluster_id = "9999999") %>% 
  arrange(discipline, scientific_year)

plot_function_allresearchers = function (column) {
  ggplot(individual_researcher_performance_first5years, aes_string(x="scientific_year", y = column,  group="cluster_id")) +
    geom_line() +
    geom_line(data = average_researcher_performance ,aes_string(y=paste0("median_",column)), size=1, color="green") +
    geom_line(data = average_researcher_performance, aes_string(y=paste0("top5percent_",column)), size=1, color="red") +
    facet_wrap(~discipline)
}

plot_function_movers = function (column) {
  ggplot(movers_individual_researcher_performance_first5years, aes_string(x="scientific_year", y = column,  group="cluster_id")) +
    geom_line() +
    geom_line(data = movers_average_researcher_performance ,aes_string(y=paste0("movers_median_",column)), size=1, color="green") +
    geom_line(data = movers_average_researcher_performance, aes_string(y=paste0("movers_top5percent_",column)), size=1, color="red") +
    facet_wrap(~discipline)
}

performance_plots <- lapply(names(individual_researcher_performance_first5years)[10:15], plot_function_allresearchers)
movers_performance_plots <- lapply(names(movers_individual_researcher_performance_first5years)[10:15], plot_function_movers)

updated_plots <- list()
updated_plots$cum_p_full <- performance_plots[[1]] + ylim(0,30) #cumulative full publication count
updated_plots$cum_p_frac <- performance_plots[[2]] + ylim(0,6) #cumulative fractionalised publication count
updated_plots$cum_cs_frac <- performance_plots[[3]] + ylim(0,55) #cumulative fractionalised publication count
updated_plots$cum_ncs_frac <- performance_plots[[4]] + ylim(0,12) #cumulative fractionalised publication count
updated_plots$cum_p_top_prop10_frac <- performance_plots[[5]] + ylim(0,2) #cumulative fractionalised publication count
updated_plots$cum_njs_frac <- performance_plots[[6]] + ylim(0,12) #cumulative fractionalised publication count

updated_plots$cum_p_full_moversonly <- movers_performance_plots[[1]] + ylim(0,30) #cumulative full publication count
updated_plots$cum_p_frac_moversonly <- movers_performance_plots[[2]] + ylim(0,6) #cumulative fractionalised publication count
updated_plots$cum_cs_frac_moversonly <- movers_performance_plots[[3]] + ylim(0,55) #cumulative fractionalised publication count
updated_plots$cum_ncs_frac_moversonly <- movers_performance_plots[[4]] + ylim(0,12) #cumulative fractionalised publication count
updated_plots$cum_p_top_prop10_frac_moversonly <- movers_performance_plots[[5]] + ylim(0,2) #cumulative fractionalised publication count
updated_plots$cum_njs_frac_moversonly <- movers_performance_plots[[6]] + ylim(0,12) #cumulative fractionalised publication count

lapply(names(updated_plots), 
       function(x)ggsave(filename=paste0("plots/",x,".pdf"), plot=updated_plots[[x]]))


############################################################
####looking at performance differentiation after the move#########
############################################################

postmove_individual_researcher_performance <- movers_dataset_3yearsmin %>% 
  select(cluster_id, ut, discipline, pub_year,order_of_publishing, move_to_USA_publication_order) %>% 
  distinct(cluster_id, ut, .keep_all = T) %>% 
  left_join(citation_3year_info, by ="ut") %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  filter(order_of_publishing >= move_to_USA_publication_order) %>% 
  group_by(cluster_id) %>%
  mutate(post_move_year = pub_year-first(pub_year),
         n_articles_published_post_move= n_distinct(ut)) %>% 
  group_by(cluster_id, post_move_year) %>% 
  summarise(discipline = first(discipline),
            p_full = sum(p_full),
            p_frac = sum(p_frac),
            cs_frac = sum(cs_frac),
            ncs_frac = sum(ncs_frac),
            p_top_prop10_frac = sum(p_top_prop10_frac),
            njs_frac = sum(njs_frac)) %>% 
  mutate(cum_p_full = cumsum(p_full),
         cum_p_frac = cumsum(p_frac),
         cum_cs_frac = cumsum(cs_frac),
         cum_ncs_frac = cumsum(ncs_frac),
         cum_p_top_prop10_frac = cumsum(p_top_prop10_frac),
         cum_njs_frac = cumsum(njs_frac)) 

postmove_individual_researcher_performance_first4years <- 
  postmove_individual_researcher_performance %>% 
  filter(post_move_year <=4)

postmove_average_researcher_performance  <- postmove_individual_researcher_performance_first4years %>% 
  group_by(post_move_year, discipline) %>% 
  mutate(median_cum_p_full = median(cum_p_full, na.rm = T),
         top5percent_cum_p_full = quantile(cum_p_full, 0.95, na.rm=T),
         median_cum_p_frac = median(cum_p_frac, na.rm = T),
         top5percent_cum_p_frac = quantile(cum_p_frac, 0.95, na.rm=T),
         median_cum_cs_frac = median(cum_cs_frac, na.rm = T),
         top5percent_cum_cs_frac = quantile(cum_cs_frac, 0.95, na.rm=T),
         median_cum_ncs_frac = median(cum_ncs_frac, na.rm = T),
         top5percent_cum_ncs_frac = quantile(cum_ncs_frac, 0.95, na.rm=T),
         median_cum_p_top_prop10_frac = median(cum_p_top_prop10_frac, na.rm = T),
         top5percent_cum_p_top_prop10_frac = quantile(cum_p_top_prop10_frac, 0.95, na.rm=T),
         median_cum_njs_frac = median(cum_njs_frac, na.rm = T),
         top5percent_cum_njs_frac = quantile(cum_njs_frac, 0.95, na.rm=T)) %>% 
  distinct(post_move_year, discipline, .keep_all = T) %>% 
  select(post_move_year, discipline, median_cum_p_full:top5percent_cum_njs_frac) %>% 
  mutate(cluster_id = "9999999") %>% 
  arrange(discipline, post_move_year)

plot_function_movemove = function (column) {
  ggplot(postmove_individual_researcher_performance_first4years, aes_string(x="post_move_year", y = column,  group="cluster_id")) +
    geom_line() +
    geom_line(data = postmove_average_researcher_performance ,aes_string(y=paste0("median_",column)), size=1, color="green") +
    geom_line(data = postmove_average_researcher_performance, aes_string(y=paste0("top5percent_",column)), size=1, color="red") +
    ylim(0, max(postmove_average_researcher_performance[paste0("top5percent_",column)])) +
    facet_wrap(~discipline)
}

postmove_performance_plots <- lapply(names(postmove_individual_researcher_performance)[10:15], plot_function_movemove)

postmove_performance_plots
