source("4. matching.R") #importing data and packages

pacman::p_load(sjPlot, cowplot, did, lmerTest, ggpubr, interplot,mediation) #https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
detach("package:dplyr", unload = TRUE)
library(dplyr)
set.seed(5030)

#setting up data for diff-in-diff
matched_dataset <- cluster_id_and_pair_id %>% 
  left_join(researcher_performance_years, by = "cluster_id") %>% 
  left_join(researcher_basic_info %>% select(cluster_id, origin_institution, origin_country,origin_region,USA_institution, discipline, specialty,end_of_career_year, gender, origin_qs_overall_score_mean,origin_qs_overall_rank_quartiles,USA_qs_overall_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean, origin_pp_top10_mean_quantile, USA_pp_top10_mean, USA_pp_top10_mean_quantile, gelman_difference_in_qs_overall_score,difference_in_qs_overall_ranking_quantile, gelman_difference_in_pptop10, difference_in_pptop10_quantile,origin_leiden_ranked, origin_type, USA_type), by = "cluster_id") %>% 
  select(cluster_id, pair_id, moving_year, condition, discipline, specialty, career_year,end_of_career_year, everything()) %>% 
  mutate(years_from_obtaining_usa_affilation = career_year-moving_year,
         post_move = if_else(years_from_obtaining_usa_affilation >= 0, 1, 0),
         late_mover = if_else(career_year >= 6, 1, 0), #dummy variable if mover moves later than the 3rd quartile of movers (i haven't checked this since I changed from months to years)
         career_over = if_else(career_year > end_of_career_year, 1, 0),
         condition_numeric = if_else(condition == "movers", 1, 0),
         woman = if_else(gender == "F", 1, 0),
         current_qs_ranking = if_else(post_move == 1 & condition_numeric == 1, USA_qs_overall_rank_quartiles, origin_qs_overall_rank_quartiles),
         current_leiden_ranking = if_else(post_move == 1 & condition_numeric == 1, USA_pp_top10_mean_quantile, origin_pp_top10_mean_quantile))

diffindiff_data <- matched_dataset %>% 
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2)

diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>%  count(discipline) #do this to see how many matches per discipline
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(origin_country) %>% print(n=100) #do this to see how many matches per discipline

diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>%filter(condition_numeric == 1) %>% count(gender) #do this to see how many matches per discipline
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(origin_type) #i manually went through the missing - 14 = education, 1 = archive, 25 = facility, 12 = government, 14 = healthcare, 6 = nonprofit
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(USA_type) # i manually went through the missing - 1 = education, 1 =archive, 1 = facility, 1= gov, 2 = health, 4 = nonprofit

alt_data_matched_dataset <- matched_dataset %>% 
  #filter(career_over == 0) %>% 
  mutate(moving_year_plus1 = moving_year+1, #to add interpretability I added one here. this means that the year that someone starts their career is year 1 instead of year 0
         career_year_plus_1 = career_year+1,
         moving_year_plus1 = if_else(condition == "stayers", 0, moving_year_plus1),
         difference_in_pptop10_quantile_with_zeros = if_else(condition_numeric == 0, 0, difference_in_pptop10_quantile))

did_model_pfull <- att_gt(yname = "p_full_yearsum",
              gname = "moving_year_plus1",
              idname = "cluster_id",
              tname = "career_year_plus_1",
              xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
              data = alt_data_matched_dataset,
              est_method = "dr",
              control_group = "nevertreated",
              anticipation = 1,
              allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_pfull_dynamic_short <- aggte(did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_pfull_dynamic_short)
p_full_did_plot <- ggdid(did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")


## normalised citation score
#year mean
did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                gname = "moving_year_plus1",
                                idname = "cluster_id",
                                tname = "career_year_plus_1",
                                xformla = ~1,
                                data = alt_data_matched_dataset,
                                est_method = "dr",
                                control_group = "nevertreated",
                                anticipation = 1,
                                allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_ncs_full_dynamic_short <- aggte(did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_ncs_full_dynamic_short)
ncs_full_did_plot <- ggdid(did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Citation score (yearly mean)")

# normalised journal score full
did_model_njs_full<- att_gt(yname = "njs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1,
                                      data = alt_data_matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_njs_full_dynamic_short <- aggte(did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_full_dynamic_short)
njs_full_did_plot <- ggdid(did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = " Journal score (yearly mean)")


# normalised journal score - number of "top journals" (njs >2 ) per year
did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = alt_data_matched_dataset,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_njs_topjournals_dynamic_short <- aggte(did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_topjournals_dynamic_short)
njs_topjournals_did_plot <- ggdid(did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top journals (yearly sum njs>2)")

# p_top_prop10_full
did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = alt_data_matched_dataset,
                            est_method = "dr",
                            control_group = "notyettreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_p_top_prop10_full_dynamic_short <- aggte(did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_p_top_prop10_full_dynamic_short)
pp10_full_did_plot <- ggdid(did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top cited papers (yearly sum pp10%)")

# making a panel plot (full count only)
did_plot_grid <- ggarrange(p_full_did_plot, 
          ncs_full_did_plot,
          njs_full_did_plot, 
          njs_topjournals_did_plot,
          pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3, labels = "AUTO",hjust=-2)

ggexport(did_plot_grid, filename = "plots/Fig2. DID.pdf")
#assessing reletive increases in performance

ATTs <- c(did_model_pfull_dynamic_short$overall.att,
          did_model_ncs_full_dynamic_short$overall.att,
          did_model_njs_full_dynamic_short$overall.att,
          did_model_njs_topjournals_dynamic_short$overall.att,
          did_model_p_top_prop10_full_dynamic_short$overall.att)

table2 <- alt_data_matched_dataset %>% #calculating relative increase
  filter(years_from_obtaining_usa_affilation >= 0 & years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 0) %>% 
  summarise(mean_p_full = mean(p_full_yearsum),
            mean_ncs_full = mean(ncs_full_mean, na.rm=T),
            mean_njs_full = mean(njs_full_mean, na.rm=T),
            mean_njs_topjournals = mean(njs_full_over2_yearsum, na.rm=T),
            mean_p_top_prop10_full = mean(p_top_prop10_full_yearsum)) %>% 
  pivot_longer(everything(),names_to = "bibliometric_measure", values_to = "mean") %>% 
  mutate(ATT = ATTs,
         percentage_increase = (ATT/mean)*100) %>% 
  mutate(mean = round(mean,2),
         ATT = round(ATT, 2),
         percentage_increase = round(percentage_increase, 2))

write.csv(table2, "tables/table1. att.csv")

tidy(did_model_pfull_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>%  write.csv("tables/S2. DID pfull.csv")
tidy(did_model_ncs_full_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S3. DID ncs.csv")
tidy(did_model_njs_full_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S4. DID njs.csv")
tidy(did_model_njs_topjournals_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S5. DID topjournals.csv")
tidy(did_model_p_top_prop10_full_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S6. DID pptop10.csv")
##################################################
### assessing moderation by university ranking ###
##################################################

#taking only movers and interacting 2 variables: "moving" with "difference in rank from origin to usa"
diffindiff_data_only_movers_qs_diff <- diffindiff_data %>% 
  filter(condition_numeric == 1,
         !is.na(difference_in_qs_overall_ranking_quantile)) %>% 
  mutate(difference_in_qs_quantile_zeropremove = if_else(post_move == 0, 0, as.double(difference_in_qs_overall_ranking_quantile)),
         gelman_difference_in_qs_overall_score_zeropremove = if_else(post_move == 0, 0, gelman_difference_in_qs_overall_score),
         gelman_origin_qs_overall_score_mean = effectsize::standardize(origin_qs_overall_score_mean, two_sd=T))

diffindiff_data_only_movers_leiden_diff <- diffindiff_data %>% 
  filter(condition_numeric == 1,
         !is.na(difference_in_pptop10_quantile)) %>% 
  mutate(difference_in_pptop10_quantile_zeropremove = if_else(post_move == 0, 0, difference_in_pptop10_quantile),
         gelman_difference_in_pptop10_zeropremove = if_else(post_move == 0, 0, gelman_difference_in_pptop10),
         gelman_origin_pp_top10_mean = effectsize::standardize(origin_pp_top10_mean, two_sd = T))

pfull_qs <- lmer(p_full_yearsum ~ gelman_difference_in_qs_overall_score:post_move + post_move + career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(pfull_qs); plot_model(pfull_qs, type = "int")
pfull_pptop10 <- lmer(p_full_yearsum ~ gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(pfull_pptop10); plot_model(pfull_pptop10, type = "int", show.values = T)

pfull_qs_moderation_plot <- interplot(m = pfull_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.3,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

pfull_pptop10_moderation_plot <- interplot(m = pfull_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.3,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

ncs_full_qs <- lmer(ncs_full_mean ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(ncs_full_qs); plot_model(ncs_full_qs, type = "int")
ncs_full_pptop10 <- lmer(ncs_full_mean ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(ncs_full_pptop10); plot_model(ncs_full_pptop10, type = "int", show.values = T)

ncs_full_qs_moderation_plot <- interplot(m = ncs_full_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.3, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

ncs_full_pptop10_moderation_plot <- interplot(m = ncs_full_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.3, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

njs_full_qs <-  lmer(njs_full_mean ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(njs_full_qs); plot_model(njs_full_qs, type = "int")
njs_full_pptop10 <-  lmer(njs_full_mean ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(njs_full_pptop10); plot_model(njs_full_pptop10, type = "int")

njs_full_qs_moderation_plot <- interplot(m = njs_full_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7),expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

njs_full_pptop10_moderation_plot <- interplot(m = njs_full_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7),expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

njs_topjournals_qs <-  lmer(njs_full_over2_yearsum ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(njs_topjournals_qs); plot_model(njs_topjournals_qs, type = "int")
njs_topjournals_pptop10 <-  lmer(njs_full_over2_yearsum ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(njs_topjournals_pptop10); plot_model(njs_topjournals_pptop10, type = "int")

njs_topjournals_qs_moderation_plot <- interplot(m = njs_topjournals_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

njs_topjournals_pptop10_moderation_plot <- interplot(m = njs_topjournals_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

p_top_prop10_full_qs <-  lmer(p_top_prop10_full_yearsum ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(p_top_prop10_full_qs); plot_model(p_top_prop10_full_qs, type = "int")
p_top_prop10_full_pptop10 <-lmer(p_top_prop10_full_yearsum ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(p_top_prop10_full_pptop10); plot_model(p_top_prop10_full_pptop10, type = "int")

p_top_prop10_full_qs_moderation_plot <- interplot(m = p_top_prop10_full_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

p_top_prop10_full_pptop10_moderation_plot <- interplot(m = p_top_prop10_full_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

qs_difference_plot <- ggplot(diffindiff_data_only_movers_qs_diff %>% distinct(cluster_id, .keep_all = T), aes(x=gelman_difference_in_qs_overall_score)) +
  geom_histogram()+
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,125), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))


leiden_difference_plot <- ggplot(diffindiff_data_only_movers_leiden_diff %>% distinct(cluster_id, .keep_all = T), aes(x = gelman_difference_in_pptop10)) +
  geom_histogram() +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,125), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

interaction_plots_left <- ggarrange(pfull_qs_moderation_plot,
          ncs_full_qs_moderation_plot, 
          njs_full_qs_moderation_plot,
          njs_topjournals_qs_moderation_plot, 
          p_top_prop10_full_qs_moderation_plot,
          qs_difference_plot,
          common.legend = T, legend = "bottom", ncol=1,nrow=6, labels = "auto",hjust = -3, vjust = 2,align = "v") %>% 
  annotate_figure(top = text_grob("QS", face = "bold"))

interaction_plots_right <- ggarrange(pfull_pptop10_moderation_plot,
                                     ncs_full_pptop10_moderation_plot,
                                     njs_full_pptop10_moderation_plot, 
                                     njs_topjournals_pptop10_moderation_plot,
                                     p_top_prop10_full_pptop10_moderation_plot,
                                     leiden_difference_plot,
                                     common.legend = T, legend = "bottom", ncol=1,nrow=6, align = "v") %>% 
  annotate_figure(top = text_grob("Leiden", face="bold"))

#moderation_plot_grid <- 
  ggarrange(interaction_plots_left, interaction_plots_right) %>% 
  annotate_figure(left = text_grob("Count                                                     Estimated Coefficient for moving to USA",rot = 90, size = 10, hjust = .56),
                  bottom = text_grob("Standardised difference in ranking score (positive = USA higher ranked)", size = 10))
ggexport(moderation_plot_grid, filename = "plots/Fig3. Moderation.pdf")

tab_model(pfull_qs,
          ncs_full_qs,
          njs_full_qs,
          njs_topjournals_qs,
          p_top_prop10_full_qs,
          show.p = F,
          show.re.var =F,
          file = "tables/table2 moderation qs.doc")

tab_model(pfull_pptop10,
          ncs_full_pptop10,
          njs_full_pptop10,
          njs_topjournals_pptop10,
          p_top_prop10_full_pptop10,
          show.p = F,
          show.re.var =F,
          file = "tables/table3 moderation leiden.doc")

##################################################
### assessing mediation by university ranking ###
##################################################
# Running did model in regression, and assessing difference in effect when adding "difference in rank from origin to usa"

#### QS RANKING ######
# p full
detach("package:lmerTest", unload = T)
pfull_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
pfull_qs_out.fit <- lmer(p_full_yearsum ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

pfull_qs_med.out <- mediate(pfull_qs_med.fit, pfull_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 100)
med_qs_pubs <- summary(pfull_qs_med.out)

# citation score
ncs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(ncs_full_mean))
ncs_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean + (1|cluster_id), data = ncs_qs_mediation_data)
ncs_qs_out.fit <- lmer(ncs_full_mean ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = ncs_qs_mediation_data)

ncs_qs_med.out <- mediate(ncs_qs_out.fit, ncs_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 100)
med_qs_ncs <- summary(ncs_qs_med.out)

# journal score
njs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(njs_full_mean))
njs_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = njs_qs_mediation_data)
njs_qs_out.fit <- lmer(njs_full_mean ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = njs_qs_mediation_data)

njs_qs_med.out <- mediate(njs_qs_med.fit, njs_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 100)
med_qs_njs <- summary(njs_qs_med.out)

# top journals
topjoural_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
topjournal_qs_out.fit <- lmer(njs_full_over2_yearsum ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

topjournal_qs_med.out <- mediate(topjoural_qs_med.fit, topjournal_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 100)
med_qs_topjournals <- summary(topjournal_qs_med.out)

# top 10%
top10_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
top10_qs_out.fit <- lmer(p_top_prop10_full_yearsum ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean +(1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

top10_qs_med.out <- mediate(top10_qs_med.fit, top10_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 100)
med_qs_pptop10 <- summary(top10_qs_med.out)

##### LEIDEN RANKING ######

# p full
detach("package:lmerTest", unload = T)
pfull_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
pfull_out.fit <- lmer(p_full_yearsum ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

pfull_med.out <- mediate(pfull_med.fit, pfull_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = T, sims = 100)
med_leiden_pubs <- summary(pfull_med.out)

# citation score
ncs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(ncs_full_mean))
ncs_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = ncs_mediation_data)
ncs_out.fit <- lmer(ncs_full_mean ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = ncs_mediation_data)

ncs_med.out <- mediate(ncs_med.fit, ncs_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 100)
med_leiden_ncs <- summary(ncs_med.out)

# journal score
njs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(njs_full_mean))
njs_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = njs_mediation_data)
njs_out.fit <- lmer(njs_full_mean ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = njs_mediation_data)

njs_med.out <- mediate(njs_med.fit, njs_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 100)
med_leiden_njs <- summary(njs_med.out)

# top journals
topjoural_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
topjournal_out.fit <- lmer(njs_full_over2_yearsum ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

topjournal_med.out <- mediate(topjoural_med.fit, topjournal_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 100)
med_leiden_topjournals <- summary(topjournal_med.out)

# top 10%
top10_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
top10_out.fit <- lmer(p_top_prop10_full_yearsum ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean +(1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

top10_med.out <- mediate(top10_med.fit, top10_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 100)
med_leiden_pptop10 <- summary(top10_med.out)

mediation_table <- c("", "", "publications","", "", "ncs","", "", "njs","", "", "top journals", "","", "top cited", "","") %>% 
  rbind(c("ranking", "mediation", "estimate", "CI_l","CI_U", "estimate",  "CI_l","CI_U", "estimate",  "CI_l","CI_U", "estimate", "CI_l","CI_U", "estimate",  "CI_l","CI_U"))%>% 
  rbind(c("QS", "ACME", round(med_qs_pubs$d0, 2), paste(round(med_qs_pubs$d0.ci,2)), round(med_qs_ncs$d0,2), paste(round(med_qs_ncs$d0.ci,2)),round(med_qs_njs$d0,2), paste(round(med_qs_njs$d0.ci,2)),round(med_qs_topjournals$d0,2), paste(round(med_qs_topjournals$d0.ci,2)),round(med_qs_pptop10$d0,2), paste(round(med_qs_pptop10$d0.ci,2)))) %>% 
  rbind(c("QS", "ADE", round(med_qs_pubs$z0, 2), paste(round(med_qs_pubs$z0.ci,2)), round(med_qs_ncs$z0,2), paste(round(med_qs_ncs$z0.ci,2)),round(med_qs_njs$z0,2), paste(round(med_qs_njs$z0.ci,2)),round(med_qs_topjournals$z0,2), paste(round(med_qs_topjournals$z0.ci,2)),round(med_qs_pptop10$z0,2), paste(round(med_qs_pptop10$z0.ci,2)))) %>% 
  rbind(c("QS", "Total Effect", round(med_qs_pubs$tau.coef, 2), paste(round(med_qs_pubs$tau.ci,2)), round(med_qs_ncs$tau.coef,2), paste(round(med_qs_ncs$tau.ci,2)),round(med_qs_njs$tau.coef,2), paste(round(med_qs_njs$tau.ci,2)),round(med_qs_topjournals$tau.coef,2), paste(round(med_qs_topjournals$tau.ci,2)),round(med_qs_pptop10$tau.coef,2), paste(round(med_qs_pptop10$tau.ci,2)))) %>% 
  rbind(c("QS", "Prop. Mediated", round(med_qs_pubs$n0, 2), paste(round(med_qs_pubs$n0.ci,2)), round(med_qs_ncs$n0,2), paste(round(med_qs_ncs$n0.ci,2)),round(med_qs_njs$n0,2), paste(round(med_qs_njs$n0.ci,2)),round(med_qs_topjournals$n0,2), paste(round(med_qs_topjournals$n0.ci,2)),round(med_qs_pptop10$n0,2), paste(round(med_qs_pptop10$n0.ci,2)))) %>% 
  rbind(c("", "", "","", "", "","", "", "","", "", "", "","", "", "","")) %>% 
  rbind(c("Leiden", "ACME", round(med_qs_pubs$d0, 2), paste(round(med_leiden_pubs$d0.ci,2)), round(med_leiden_ncs$d0,2), paste(round(med_leiden_ncs$d0.ci,2)),round(med_leiden_njs$d0,2), paste(round(med_leiden_njs$d0.ci,2)),round(med_leiden_topjournals$d0,2), paste(round(med_leiden_topjournals$d0.ci,2)),round(med_leiden_pptop10$d0,2), paste(round(med_leiden_pptop10$d0.ci,2)))) %>% 
  rbind(c("Leiden", "ADE", round(med_leiden_pubs$z0, 2), paste(round(med_leiden_pubs$z0.ci,2)), round(med_leiden_ncs$z0,2), paste(round(med_leiden_ncs$z0.ci,2)),round(med_leiden_njs$z0,2), paste(round(med_leiden_njs$z0.ci,2)),round(med_leiden_topjournals$z0,2), paste(round(med_leiden_topjournals$z0.ci,2)),round(med_leiden_pptop10$z0,2), paste(round(med_leiden_pptop10$z0.ci,2)))) %>% 
  rbind(c("Leiden", "Total Effect", round(med_leiden_pubs$tau.coef, 2), paste(round(med_leiden_pubs$tau.ci,2)), round(med_leiden_ncs$tau.coef,2), paste(round(med_leiden_ncs$tau.ci,2)),round(med_leiden_njs$tau.coef,2), paste(round(med_leiden_njs$tau.ci,2)),round(med_leiden_topjournals$tau.coef,2), paste(round(med_leiden_topjournals$tau.ci,2)),round(med_leiden_pptop10$tau.coef,2), paste(round(med_leiden_pptop10$tau.ci,2)))) %>% 
  rbind(c("Leiden", "Prop. Mediated", round(med_leiden_pubs$n0, 2), paste(round(med_leiden_pubs$n0.ci,2)), round(med_leiden_ncs$n0,2), paste(round(med_leiden_ncs$n0.ci,2)),round(med_leiden_njs$n0,2), paste(round(med_leiden_njs$n0.ci,2)),round(med_leiden_topjournals$n0,2), paste(round(med_leiden_topjournals$n0.ci,2)),round(med_leiden_pptop10$n0,2), paste(round(med_leiden_pptop10$n0.ci,2)))) %>% 
  as_tibble() %>% 
  unite(col = "pubs_CI",V4:V5, sep = ", ") %>% 
  unite(col = "ncs_CI",V7:V8, sep = ", ") %>% 
  unite(col = "njs_CI",V10:V11, sep = ", ") %>%
  unite(col = "topjournals_CI",V13:V14, sep = ", ") %>%
  unite(col = "pptop10_CI",V16:V17, sep = ", ")

write.csv(mediation_table, "tables/table4. mediation table.csv")


# raw data plots
se <- function(x) sd(x)/sqrt(length(x))

controls_data_forplots <- alt_data_matched_dataset %>%  
  filter(years_from_obtaining_usa_affilation <= 2 & years_from_obtaining_usa_affilation >= -5,
         condition_numeric == 0) 

movers_data_forplots <- alt_data_matched_dataset %>%  
  filter(years_from_obtaining_usa_affilation <= 2 & years_from_obtaining_usa_affilation >= -5,
         condition_numeric == 1) 

raw_data_plots_means_per_year <- alt_data_matched_dataset %>% 
  filter(years_from_obtaining_usa_affilation <= 2 & years_from_obtaining_usa_affilation >= -5) %>%
  group_by(condition, years_from_obtaining_usa_affilation) %>%
  summarise(mean_p_full = mean(p_full_yearsum),
            mean_ncs_full_mean = mean(ncs_full_mean, na.rm=T),
            mean_njs_full_mean = mean(njs_full_mean, na.rm=T),
            mean_njs_full_over2_yearsum = mean(njs_full_over2_yearsum),
            mean_p_top_prop10_full_yearsum =mean(p_top_prop10_full_yearsum),
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
  geom_errorbar(aes(ymin = mean_p_full-se_p_full, ymax = mean_p_full+se_p_full), width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Mean (SE) number of publications") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

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
  geom_errorbar(aes(ymin = mean_ncs_full_mean-se_ncs_full_mean, ymax = mean_ncs_full_mean+se_ncs_full_mean), width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Mean (SE) normalised citation score") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))+
  scale_y_continuous(limits = c(1.25, 2.75))

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
  geom_errorbar(aes(ymin = mean_njs_full_mean-se_mean_njs_full_mean, ymax = mean_njs_full_mean+se_mean_njs_full_mean),width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Mean (SE) normalised journal score") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1)) +
  scale_y_continuous(limits = c(1.25, 2.15))

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
  geom_errorbar(aes(ymin = mean_njs_full_over2_yearsum-se_njs_full_over2_yearsum, ymax = mean_njs_full_over2_yearsum+se_njs_full_over2_yearsum),width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Mean (SE) sum of top journal publications (njs > 2)") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

vioplot(log(njs_full_over2_yearsum+1) ~ years_from_obtaining_usa_affilation, data = controls_data_forplots, h = 0.1, col = "lightblue", plotCentre = "line", 
        side = "left", ylim = c(0, 3), areaEqual = F,rectCol=NA,lineCol = NA, xlab  =NA, ylab = "log (sum (njs > 2) + 1)")
vioplot(log(njs_full_over2_yearsum+1) ~ years_from_obtaining_usa_affilation, data = movers_data_forplots, h = 0.1, col = "palevioletred", plotCentre = "line", 
        side = "right", ylim = c(0, 3), add = T,areaEqual = F,rectCol=NA,lineCol = NA)

test_dd <- recordPlot()
plot(0)
test_dd

topjournal_jointplot <- ggdraw(dd) +
  draw_plot(test_dd, .15, 0.55, width = 0.35, height = 0.4) +
  draw_plot_label(label = c("A", "B"), size = 25,
                  x = c(0, 0.12), y = c(1, 1))

cowplot::save_plot("plots/S4_raw_topjournals.pdf", topjournal_jointplot, base_height = 9)


#raw data plot of number of pp10 articles

ee <- ggplot(raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_p_top_prop10_full_yearsum, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = mean_p_top_prop10_full_yearsum-se_mean_p_top_prop10_full_yearsum, ymax = mean_p_top_prop10_full_yearsum+se_mean_p_top_prop10_full_yearsum),width = 0.2) +
  theme_classic()+
  theme(axis.text=element_text(size=rel(2)), 
        legend.text = element_text(size=rel(2)), 
        axis.title = element_text(size=rel(2)),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Mean (SE) sum of top10% most cited papers in field") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

vioplot(log(p_top_prop10_full_yearsum+1) ~ years_from_obtaining_usa_affilation, h = 0.1, data = controls_data_forplots, col = "lightblue", plotCentre = "line", 
        side = "left", ylim = c(0, 3), areaEqual = F,rectCol=NA,lineCol = NA, xlab  =NA, ylab = "log (sum (pptop10) + 1)")
vioplot(log(p_top_prop10_full_yearsum+1) ~ years_from_obtaining_usa_affilation, h = 0.1, data = movers_data_forplots, col = "palevioletred", plotCentre = "line", 
        side = "right", ylim = c(0, 3), add = T,areaEqual = F,rectCol=NA,lineCol = NA)


test_ee <- recordPlot()
plot(0)
test_ee

pp10_jointplot <- ggdraw(ee) +
  draw_plot(test_ee, .15, 0.55, width = 0.35, height = 0.4) +
  draw_plot_label(label = c("A", "B"), size = 25,
                  x = c(0, 0.12), y = c(1, 1))

cowplot::save_plot("plots/S5_raw_pp10.pdf", pp10_jointplot, base_height = 9)


#distribution plots
hist(matched_dataset$p_frac_yearsum, 100)
hist(log(matched_dataset$p_full_yearsum+1), 50)
hist(log(matched_dataset$ncs_full_yearsum+1), 500)
hist(matched_dataset$ncs_frac_yearsum, 500)
hist(log(matched_dataset$njs_full_mean+1), 500)
hist(matched_dataset$njs_frac_mean, 500)
hist(log(matched_dataset$p_top_prop10_full_yearsum+1), 50)
hist(matched_dataset$p_top_prop10_frac_yearsum, 50)
hist(log(matched_dataset$njs_full_over2_yearsum+1),50)