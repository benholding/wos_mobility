source("4. matching.R") #importing data and packages

pacman::p_load(sjPlot, cowplot)

#setting up data for diff-in-diff
diffindiff_data <- cluster_id_and_pair_id %>% 
  left_join(researcher_performance_months, by = "cluster_id") %>% 
  left_join(researcher_basic_info %>% select(cluster_id, discipline, specialty), by = "cluster_id") %>% 
  select(cluster_id, pair_id, moving_month, condition, discipline, specialty, month, everything()) %>% 
  mutate(months_from_move = month-moving_month,
         post_move = if_else(months_from_move >= 0, 1, -1),
         late_mover = if_else(moving_month >= 64, 1, -1)) %>% #dummy variable if mover moves later than the 3rd quartile of movers
  filter(months_from_move >= -24,
         months_from_move <= 24,
         months_from_move != 0) %>%  #everyone in the movers group publishes at zero. I've removed it from the dataset, but not sure if that is best solution
  group_by(cluster_id) %>% 
  mutate(one_month_bins = as.double(rep(-23:24, each = 2)),
         six_month_bins = as.double(rep(-3:4, each=12)),
         six_month_bins_nozero = if_else(six_month_bins >= 0, six_month_bins+1, six_month_bins),
         one_year_bins = as.double(rep(-1:2, each=24)),
         one_year_bins_nozero = if_else(one_year_bins >= 0, one_year_bins+1, one_year_bins)) %>% 
  ungroup()

# diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% count(discipline) #do this to see how many matches per discipline

version2_diffindiff_data <- cluster_id_and_pair_id_version2 %>% 
  left_join(researcher_performance_months, by = "cluster_id") %>% 
  select(cluster_id, pair_id, moving_month, condition, discipline, specialty, month, everything()) %>% 
  mutate(months_from_move = month-moving_month,
         post_move = if_else(months_from_move >= 0, 1, -1),
         late_mover = if_else(moving_month >= 64, 1, -1)) %>% #dummy variable if mover moves later than the 3rd quartile of movers
  filter(months_from_move >= -24,
         months_from_move <= 24,
         months_from_move != 0) %>%  #everyone in the movers group publishes at zero. I've removed it from the dataset, but not sure if that is best solution
  group_by(cluster_id) %>% 
  mutate(one_month_bins = as.double(rep(-23:24, each = 2)),
         six_month_bins = as.double(rep(-3:4, each=12)),
         six_month_bins_nozero = if_else(six_month_bins >= 0, six_month_bins+1, six_month_bins),
         one_year_bins = as.double(rep(-1:2, each=24)),
         one_year_bins_nozero = if_else(one_year_bins >= 0, one_year_bins+1, one_year_bins)) %>% 
  ungroup()

## assessing parallel trends ##
#full dataset

#matched dataset

#one year bins
summarised_data_for_ggplot <- diffindiff_data %>% 
  group_by(cluster_id, one_year_bins) %>% 
  summarise(condition = first(condition),
            discipline = first(discipline),
            specialty = first(specialty),
            p_full_sum_per_subject = sum(p_full_sum),
            cs_full_sum_per_subject = sum(cs_full_sum),
            cs_full_mean_per_subject= mean(cs_full_mean, na.rm=T),
            njs_full_mean_per_subject = mean(njs_full_mean, na.rm=T),
            p_int_collab_sum_per_subject= sum(p_int_collab_sum),
            n_coauthors_median_per_subject = mean(n_coauthors_median, na.rm=T)) %>% 
  group_by(one_year_bins, condition) %>% 
  summarise(p_full_total = mean(p_full_sum_per_subject, na.rm=T),
            cs_full_total = mean(cs_full_sum_per_subject, na.rm=T),
            cs_full_mean_per_paper = mean(cs_full_mean_per_subject, na.rm=T),
            njs_full_mean_per_paper = mean(njs_full_mean_per_subject, na.rm=T),
            p_int_collab_total = mean(p_int_collab_sum_per_subject, na.rm=T),
            n_coauthors_median_per_paper = mean(n_coauthors_median_per_subject, na.rm=T))

p_full_mean_oneyear <- ggplot(summarised_data_for_ggplot, aes(x = one_year_bins,y = p_full_total, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

cs_full_sum_oneyear <- ggplot(summarised_data_for_ggplot, aes(x = one_year_bins,y = cs_full_total, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

cs_full_mean_oneyear <- ggplot(summarised_data_for_ggplot, aes(x = one_year_bins,y = cs_full_mean_per_paper, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

njs_full_mean_oneyear <- ggplot(summarised_data_for_ggplot, aes(x = one_year_bins,y = njs_full_mean_per_paper, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

p_int_collab_sum_oneyear <- ggplot(summarised_data_for_ggplot, aes(x = one_year_bins,y = p_int_collab_total, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

n_coauthors_median_oneyear <- ggplot(summarised_data_for_ggplot, aes(x = one_year_bins,y = n_coauthors_median_per_paper, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

plot_grid(p_full_mean_oneyear, cs_full_sum_oneyear, cs_full_mean_oneyear, njs_full_mean_oneyear, p_int_collab_sum_oneyear, n_coauthors_median_oneyear)

#same as above but for version 2 matching
summarised_data_for_ggplot_version2 <- version2_diffindiff_data %>% 
  group_by(cluster_id, one_month_bins) %>% 
  summarise(condition = first(condition),
            discipline = first(discipline),
            specialty = first(specialty),
            p_full_sum_per_subject = sum(p_full),
            p_frac_sum_per_subject = sum(p_frac),
            cs_frac_sum_per_subject = sum(cs_frac),
            ncs_frac_sum_per_subject= sum(ncs_frac),
            
            p_top_prop10_frac_sum_per_subject = sum(p_top_prop10_frac),
            njs_frac_mean_per_subject = mean(njs_frac, na.rm=T)) %>% 
  group_by(one_month_bins, condition) %>% 
  summarise(p_full_mean = mean(p_full_sum_per_subject),
            p_frac_mean = mean(p_frac_sum_per_subject),
            cs_frac_mean = mean(cs_frac_sum_per_subject),
            ncs_frac_mean = mean(ncs_frac_sum_per_subject),
            p_top_prop10_frac_mean = mean(p_top_prop10_frac_sum_per_subject),
            njs_frac_mean = mean(njs_frac_mean_per_subject, na.rm=T))

version2_p_full_mean_onemonth <- ggplot(summarised_data_for_ggplot_version2, aes(x = one_month_bins,y = p_full_mean, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

version2_p_frac_mean_onemonth <- ggplot(summarised_data_for_ggplot_version2, aes(x = one_month_bins,y = p_frac_mean, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

version2_cs_frac_mean_onemonth <- ggplot(summarised_data_for_ggplot_version2, aes(x = one_month_bins,y = cs_frac_mean, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

version2_ncs_frac_mean_onemonth <- ggplot(summarised_data_for_ggplot_version2, aes(x = one_month_bins,y = ncs_frac_mean, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

version2_p_top_prop10_frac_mean_onemonth <- ggplot(summarised_data_for_ggplot_version2, aes(x = one_month_bins,y = p_top_prop10_frac_mean, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

version2_njs_frac_mean_onemonth <- ggplot(summarised_data_for_ggplot_version2, aes(x = one_month_bins,y = njs_frac_mean, group = condition, colour = condition)) +
  geom_line()+
  geom_vline(xintercept=c(0), linetype="dotted")

plot_grid(version2_p_full_mean_onemonth, version2_p_frac_mean_onemonth, version2_cs_frac_mean_onemonth, version2_ncs_frac_mean_onemonth, version2_p_top_prop10_frac_mean_onemonth, version2_njs_frac_mean_onemonth)



## running difference in difference 
test_p_full <- lm(p_full ~condition*post_move + month , data = diffindiff_data_6monthbins); summary(test_p_full); plot_model(test_p_full, type = "int")
test_p_frac <- lm(p_frac ~condition*post_move + month, data = diffindiff_data); summary(test_p_frac); plot_model(test_p_frac, type = "int")
test_cs_frac <- lm(cs_frac ~condition*post_move + month, data = diffindiff_data); summary(test_cs_frac); plot_model(test_cs_frac, type = "int")
test_ncs_frac <- lm(ncs_frac ~condition*post_move + month, data = diffindiff_data); summary(test_ncs_frac); plot_model(test_ncs_frac, type = "int")
test_p_top_prop10_frac <- lm(p_top_prop10_frac ~condition*post_move + month, data = diffindiff_data); summary(test_p_top_prop10_frac); plot_model(test_p_top_prop10_frac, type = "int")
test_njs_frac <- lm(njs_frac ~condition*post_move + month, data = diffindiff_data); summary(test_njs_frac); plot_model(test_njs_frac, type = "int")
