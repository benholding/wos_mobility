load("matched_dataset.RData") #for those downloading the code, you should use load("data_to_be_shared.RData") instead 
pacman::p_load(cowplot, dplyr, vioplot, ggplot2)
##################################################
############# DESCRIPTIVE STATISTICS #############
##################################################

dim(matched_dataset) #47736 years of data
dim(matched_dataset %>% filter(career_over == F)) #41065 years of actual career
n_distinct(matched_dataset$cluster_id) #3978 resarchers
n_distinct(matched_dataset$pair_id) #1989 pairs

# Descriptive info about the final matched dataset

matched_dataset %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>%  count(discipline) %>% mutate(individual_n = n*2, for_table = paste0(individual_n, " (", n,")")) %>% select(discipline, for_table) %>%  write.csv("tables/S1_discipline.csv")  #How many matches per discipline
matched_dataset %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(origin_country) %>% mutate(individual_n = n*2, for_table = paste0(individual_n, " (", n,")")) %>% select(origin_country, for_table) %>%  write.csv("tables/S1_countries.csv")  #How many matches per country
matched_dataset %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(origin_region) %>% print(n=100) #How many matches per region
matched_dataset %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(gender) #How many matches per gender

matched_dataset %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% 
  count(origin_type) %>% 
  left_join(read.csv("raw_data/missing_orgin_types.csv") %>% count(origin_type) %>% select(origin_type, to_add = n)) %>% 
  mutate(across(c(n, to_add), .fns = ~replace_na(.,0))) %>% 
  mutate(final_count = n+to_add) %>% 
  filter(!is.na(origin_type)) %>% 
  select(origin_type, final_count) %>% 
  mutate(individual_n = final_count*2, for_table = paste0(individual_n, " (", final_count,")")) %>% select(origin_type, for_table) %>%  write.csv("tables/S1_origin_type.csv")#how many types of origin institution per match. 

matched_dataset %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(USA_type) %>% #how many types of USA institution per match.
  mutate(NA_to_add = c(1, 0, 1, 1, 1, 2, 4,0)) %>%  # i manually went through the NA - 1 = education, 1 =archive, 1 = facility, 1= gov, 2 = health, 4 = nonprofit
  filter(!is.na(USA_type)) %>% 
  mutate(final_sum = n+NA_to_add) %>% 
  select(USA_type, final_sum) %>%  
  mutate(individual_n = final_sum*2, for_table = paste0(individual_n, " (", final_sum,")")) %>% 
  select(USA_type, for_table) %>% 
  write.csv("tables/S1_usa_type.csv")

matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "p_full_yearsum", mean = mean(p_full_yearsum), median = median(p_full_yearsum), sd = sd(p_full_yearsum), min = min(p_full_yearsum),max = max(p_full_yearsum)) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "p_frac_yearsum", mean = mean(p_frac_yearsum, na.rm = T), median = median(p_frac_yearsum, na.rm = T), sd = sd(p_frac_yearsum, na.rm = T), min = min(p_frac_yearsum, na.rm = T),max = max(p_frac_yearsum, na.rm = T))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "ncs_full_mean", mean = mean(ncs_full_mean, na.rm = T), median = median(ncs_full_mean, na.rm = T), sd = sd(ncs_full_mean, na.rm = T), min = min(ncs_full_mean, na.rm = T),max = max(ncs_full_mean, na.rm = T))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "ncs_frac_mean", mean = mean(ncs_frac_mean, na.rm = T), median = median(ncs_frac_mean, na.rm = T), sd = sd(ncs_frac_mean, na.rm = T), min = min(ncs_frac_mean, na.rm = T),max = max(ncs_frac_mean, na.rm = T))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "njs_full_mean", mean = mean(njs_full_mean, na.rm = T), median = median(njs_full_mean, na.rm = T), sd = sd(njs_full_mean, na.rm = T), min = min(njs_full_mean, na.rm = T),max = max(njs_full_mean, na.rm = T))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "njs_frac_mean", mean = mean(njs_frac_mean, na.rm = T), median = median(njs_frac_mean, na.rm = T), sd = sd(njs_frac_mean, na.rm = T), min = min(njs_frac_mean, na.rm = T),max = max(njs_frac_mean, na.rm = T))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "njs_full_over2_yearsum", mean = mean(njs_full_over2_yearsum), median = median(njs_full_over2_yearsum), sd = sd(njs_full_over2_yearsum), min = min(njs_full_over2_yearsum),max = max(njs_full_over2_yearsum))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "njs_full_over2_frac_yearsum", mean = mean(njs_full_over2_frac_yearsum), median = median(njs_full_over2_frac_yearsum), sd = sd(njs_full_over2_frac_yearsum), min = min(njs_full_over2_frac_yearsum),max = max(njs_full_over2_frac_yearsum))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "p_top_prop10_full_yearsum", mean = mean(p_top_prop10_full_yearsum), median = median(p_top_prop10_full_yearsum), sd = sd(p_top_prop10_full_yearsum), min = min(p_top_prop10_full_yearsum),max = max(p_top_prop10_full_yearsum))) %>% 
  rbind(matched_dataset %>% filter(years_from_obtaining_usa_affilation >= -5, years_from_obtaining_usa_affilation <= 2) %>% summarise(measure = "p_top_prop10_frac_yearsum", mean = mean(p_top_prop10_frac_yearsum), median = median(p_top_prop10_frac_yearsum), sd = sd(p_top_prop10_frac_yearsum), min = min(p_top_prop10_frac_yearsum),max = max(p_top_prop10_frac_yearsum))) %>% 
  write.csv("tables/S1_outcome_descriptives.csv")

#############################################
######## MAKING RAW DATA PLOTS ##############
#############################################

controls_data_forplots <- matched_dataset %>%  
  filter(years_from_obtaining_usa_affilation <= 2 & years_from_obtaining_usa_affilation >= -5,
         condition_numeric == 0) 

movers_data_forplots <- matched_dataset %>%  
  filter(years_from_obtaining_usa_affilation <= 2 & years_from_obtaining_usa_affilation >= -5,
         condition_numeric == 1) 

raw_data_plots_means_per_year <- matched_dataset %>% 
  filter(years_from_obtaining_usa_affilation <= 2 & years_from_obtaining_usa_affilation >= -5) %>%
  group_by(condition, years_from_obtaining_usa_affilation) %>%
  summarise(mean_p_full = mean(p_full_yearsum),
            mean_ncs_full_mean = mean(ncs_full_mean, na.rm=T),
            mean_njs_full_mean = mean(njs_full_mean, na.rm=T),
            mean_njs_full_over2_yearsum = mean(njs_full_over2_yearsum),
            mean_p_top_prop10_full_yearsum =mean(p_top_prop10_full_yearsum),
            median_p_full = median(p_full_yearsum),
            median_ncs_full_mean = median(ncs_full_mean, na.rm=T),
            median_njs_full_mean = median(njs_full_mean, na.rm=T),
            median_njs_full_over2_yearsum = median(njs_full_over2_yearsum),
            median_p_top_prop10_full_yearsum =median(p_top_prop10_full_yearsum),
            sd_p_full = sd(p_full_yearsum),
            sd_ncs_full_mean = sd(ncs_full_mean, na.rm=T),
            sd_mean_njs_full_mean = sd(njs_full_mean, na.rm = T),
            sd_mean_njs_full_over2_yearsum = sd(njs_full_over2_yearsum),
            sd_mean_p_top_prop10_full_yearsum = sd(p_top_prop10_full_yearsum),
            se_p_full = sd_p_full/sqrt(n()),
            se_ncs_full_mean = sd_ncs_full_mean/sqrt(n()),
            se_mean_njs_full_mean = sd_mean_njs_full_mean/sqrt(n()),
            se_njs_full_over2_yearsum = sd_mean_njs_full_over2_yearsum/sqrt(n()),
            se_mean_p_top_prop10_full_yearsum = sd_mean_p_top_prop10_full_yearsum/sqrt(n())
  )

#Raw data plots of total publications

aa <- ggplot(raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_p_full, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  geom_line(data = raw_data_plots_means_per_year, aes(years_from_obtaining_usa_affilation,median_p_full), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_p_full-se_p_full, ymax = mean_p_full+se_p_full), width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) number of publications") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1)) +
  scale_y_continuous(limits = c(1, 3.5))

par(cex.axis=1.2, cex.lab = 1.2,mar = c(2, 4, 0.1, 0.1))
vioplot(log(p_full_yearsum+1) ~ years_from_obtaining_usa_affilation, data = controls_data_forplots, h = 0.1, col = "lightblue", plotCentre = "line", 
        side = "left", ylim = c(0, 3), areaEqual = F,rectCol=NA,lineCol = NA, xlab  =NA, ylab = "log(sum of pubs per individual)+1)")
vioplot(log(p_full_yearsum+1) ~ years_from_obtaining_usa_affilation, data = movers_data_forplots, h = 0.1, col = "palevioletred", plotCentre = "line", 
        side = "right", ylim = c(0, 3), add = T, areaEqual = F,rectCol=NA, lineCol =NA)
#legend("topleft", fill = c("lightblue","palevioletred"), legend = c("Non-movers", 
#                                                                         "Movers"))
test <- recordPlot()
plot(0)
test

test2 <- ggdraw(aa) +
  draw_plot(test, .15, 0.55, width = 0.35, height = 0.4) +
  draw_plot_label(label = c("A", "B"), size = 25,
                  x = c(0, 0.12), y = c(1, 1))

cowplot::save_plot("plots/S1_raw_p_full.pdf", test2, base_height = 9)

#raw data plot of citation score

bb <- ggplot(raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_ncs_full_mean, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  geom_line(data = raw_data_plots_means_per_year, aes(years_from_obtaining_usa_affilation,median_ncs_full_mean), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_ncs_full_mean-se_ncs_full_mean, ymax = mean_ncs_full_mean+se_ncs_full_mean), width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) citation score") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))+
  scale_y_continuous(limits = c(0.75, 3.4))

vioplot(log(ncs_full_mean+1) ~ years_from_obtaining_usa_affilation, data = controls_data_forplots, h = 0.1, col = "lightblue", plotCentre = "line", 
        side = "left", ylim = c(0, 3), areaEqual = F,rectCol=NA,lineCol = NA, xlab  =NA, ylab = "log(mean ncs per individual +1)")
vioplot(log(ncs_full_mean+1) ~ years_from_obtaining_usa_affilation, data = movers_data_forplots, h = 0.1, col = "palevioletred", plotCentre = "line", 
        side = "right", ylim = c(0, 3), add = T, areaEqual = F,rectCol=NA,lineCol = NA)

test_bb <- recordPlot()
plot(0)
test_bb

ncs_jointplot <- ggdraw(bb) +
  draw_plot(test_bb, .17, 0.6, width = 0.35, height = 0.35) +
  draw_plot_label(label = c("A", "B"), size = 25,
                  x = c(0, 0.12), y = c(1, 1))

cowplot::save_plot("plots/S2_raw_ncs_full.pdf", ncs_jointplot, base_height = 9)

#raw data plot of njs
cc <- ggplot(raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_njs_full_mean, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  geom_line(data = raw_data_plots_means_per_year, aes(years_from_obtaining_usa_affilation,median_njs_full_mean), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_njs_full_mean-se_mean_njs_full_mean, ymax = mean_njs_full_mean+se_mean_njs_full_mean),width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) journal score") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1)) +
  scale_y_continuous(limits = c(1, 2.6))

vioplot(log(njs_full_mean+1) ~ years_from_obtaining_usa_affilation, data = controls_data_forplots, h = 0.1, col = "lightblue", plotCentre = "line", 
        side = "left", ylim = c(0, 3), areaEqual = F,rectCol=NA,lineCol = NA, xlab  =NA, ylab = "log(mean njs per individual +1)")
vioplot(log(njs_full_mean+1) ~ years_from_obtaining_usa_affilation, data = movers_data_forplots, h = 0.1, col = "palevioletred", plotCentre = "line", 
        side = "right", ylim = c(0, 3), add = T,areaEqual = F,rectCol=NA,lineCol = NA)

test_cc <- recordPlot()
plot(0)
test_cc

njs_jointplot <- ggdraw(cc) +
  draw_plot(test_cc, .17, 0.55, width = 0.35, height = 0.4) +
  draw_plot_label(label = c("A", "B"), size = 25,
                  x = c(0, 0.12), y = c(1, 1))

cowplot::save_plot("plots/S3_raw_njs.pdf", njs_jointplot, base_height = 9)


#raw data plot of njs top journals
dd <- ggplot(raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_njs_full_over2_yearsum, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  geom_line(data = raw_data_plots_means_per_year, aes(years_from_obtaining_usa_affilation,median_njs_full_over2_yearsum), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_njs_full_over2_yearsum-se_njs_full_over2_yearsum, ymax = mean_njs_full_over2_yearsum+se_njs_full_over2_yearsum),width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) sum of top journal publications (njs > 2)") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1)) +
  scale_y_continuous(limits = c(0, 1))

vioplot(log(njs_full_over2_yearsum+1) ~ years_from_obtaining_usa_affilation, data = controls_data_forplots, h = 0.1, col = "lightblue", plotCentre = "line", 
        side = "left", ylim = c(0, 3), areaEqual = F,rectCol=NA,lineCol = NA, xlab  =NA, ylab = "log (sum (njs > 2) + 1)")
vioplot(log(njs_full_over2_yearsum+1) ~ years_from_obtaining_usa_affilation, data = movers_data_forplots, h = 0.1, col = "palevioletred", plotCentre = "line", 
        side = "right", ylim = c(0, 3), add = T,areaEqual = F,rectCol=NA,lineCol = NA)

test_dd <- recordPlot()
plot(0)
test_dd

topjournal_jointplot <- ggdraw(dd) +
  draw_plot(test_dd, .17, 0.55, width = 0.35, height = 0.4) +
  draw_plot_label(label = c("A", "B"), size = 25,
                  x = c(0, 0.12), y = c(1, 1))

cowplot::save_plot("plots/S4_raw_topjournals.pdf", topjournal_jointplot, base_height = 9)


#raw data plot of number of pp10 articles

ee <- ggplot(raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_p_top_prop10_full_yearsum, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  geom_line(data = raw_data_plots_means_per_year, aes(years_from_obtaining_usa_affilation,median_p_top_prop10_full_yearsum), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_p_top_prop10_full_yearsum-se_mean_p_top_prop10_full_yearsum, ymax = mean_p_top_prop10_full_yearsum+se_mean_p_top_prop10_full_yearsum),width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) sum of top10% most cited papers") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))+
  scale_y_continuous(limits = c(0, 1))

vioplot(log(p_top_prop10_full_yearsum+1) ~ years_from_obtaining_usa_affilation, h = 0.1, data = controls_data_forplots, col = "lightblue", plotCentre = "line", 
        side = "left", ylim = c(0, 3), areaEqual = F,rectCol=NA,lineCol = NA, xlab  =NA, ylab = "log (sum (pptop10) + 1)")
vioplot(log(p_top_prop10_full_yearsum+1) ~ years_from_obtaining_usa_affilation, h = 0.1, data = movers_data_forplots, col = "palevioletred", plotCentre = "line", 
        side = "right", ylim = c(0, 3), add = T,areaEqual = F,rectCol=NA,lineCol = NA)


test_ee <- recordPlot()
plot(0)
test_ee

pp10_jointplot <- ggdraw(ee) +
  draw_plot(test_ee, .165, 0.55, width = 0.35, height = 0.4) +
  draw_plot_label(label = c("A", "B"), size = 25,
                  x = c(0, 0.12), y = c(1, 1))

cowplot::save_plot("plots/S5_raw_pp10.pdf", pp10_jointplot, base_height = 9)
