#importing data and packages 
load("matched_dataset.RData") #for those downloading the code, you should use load("data_to_be_shared.RData") instead 

pacman::p_load(tidyverse, sjPlot, cowplot, did, lmerTest, ggpubr, interplot,mediation) #https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
detach("package:dplyr", unload = TRUE)
library(dplyr)
set.seed(5030)

##################################################################
######## ANALYSIS STEP 1 - DIFFERENCE IN DIFFERENCE ##############
##################################################################

##FULL PUBLICATIONS##
did_model_pfull <- att_gt(yname = "p_full_yearsum",
              gname = "moving_year_plus1",
              idname = "cluster_id",
              tname = "career_year_plus_1",
              xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
              data = matched_dataset,
              est_method = "dr",
              control_group = "nevertreated",
              anticipation = 1,
              allow_unbalanced_panel = T)

did_model_pfull_dynamic_short <- aggte(did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_pfull_dynamic_short)
p_full_did_plot <- ggdid(did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")


## NORMALISED CITATION SCORE ##
did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                gname = "moving_year_plus1",
                                idname = "cluster_id",
                                tname = "career_year_plus_1",
                                xformla = ~1,
                                data = matched_dataset,
                                est_method = "dr",
                                control_group = "nevertreated",
                                anticipation = 1,
                                allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_ncs_full_dynamic_short <- aggte(did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_ncs_full_dynamic_short)
ncs_full_did_plot <- ggdid(did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Citation score (yearly mean)")

## NORMALISED JOURNAL SCORE ## 
did_model_njs_full<- att_gt(yname = "njs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1,
                                      data = matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_njs_full_dynamic_short <- aggte(did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_full_dynamic_short)
njs_full_did_plot <- ggdid(did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = " Journal score (yearly mean)")


## NUMBER OF TOP JOURNALS (njs >2 ) ##
did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = matched_dataset,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_njs_topjournals_dynamic_short <- aggte(did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_topjournals_dynamic_short)
njs_topjournals_did_plot <- ggdid(did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top journals (yearly sum njs>2)")

## NUMBER OF TOP CITED PAPERS ## 
did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = matched_dataset,
                            est_method = "dr",
                            control_group = "notyettreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_p_top_prop10_full_dynamic_short <- aggte(did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_p_top_prop10_full_dynamic_short)
pp10_full_did_plot <- ggdid(did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top cited papers (yearly sum pp10%)")

## MAKING A FIGURE OF THE DIFFERENCE-IN-DIFFERENCE RESULTS ##
did_plot_grid <- ggarrange(p_full_did_plot, 
          ncs_full_did_plot,
          njs_full_did_plot, 
          njs_topjournals_did_plot,
          pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3, labels = "AUTO",hjust=-2)

ggexport(did_plot_grid, filename = "plots/Fig2. DID.pdf")
## MAKING TABLES OF THE DIFFERENCE-IN-DIFFERENCE RESULTS ##
# First a table of the relative increases in performance
ATTs <- c(did_model_pfull_dynamic_short$overall.att,
          did_model_ncs_full_dynamic_short$overall.att,
          did_model_njs_full_dynamic_short$overall.att,
          did_model_njs_topjournals_dynamic_short$overall.att,
          did_model_p_top_prop10_full_dynamic_short$overall.att)

table2 <- matched_dataset %>% #calculating relative increase
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

#Then tables of the individual difference-in-difference models
tidy(did_model_pfull_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>%  write.csv("tables/S2. DID pfull.csv")
tidy(did_model_ncs_full_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S3. DID ncs.csv")
tidy(did_model_njs_full_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S4. DID njs.csv")
tidy(did_model_njs_topjournals_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S5. DID topjournals.csv")
tidy(did_model_p_top_prop10_full_dynamic_short) %>% mutate(across(estimate:point.conf.high, round,2)) %>% write.csv("tables/S6. DID pptop10.csv")

############################################################################################################
### ANALYSIS STEP 2. MODERATION ############################################################################
# here we take only movers and interact  2 variables: "moving" with "difference in rank from origin to usa" #
############################################################################################################

# step 2a. first i make two new datasets, one that contains complete data those who we know the origin and usa QS ranking, and one where we know the origin and usa Leiden ranking.
diffindiff_data_only_movers_qs_diff <- matched_dataset %>% #the QS ranking moderation dataset
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 1,
         !is.na(difference_in_qs_overall_ranking_quantile)) %>% 
  mutate(difference_in_qs_quantile_zeropremove = if_else(post_move == 0, 0, as.double(difference_in_qs_overall_ranking_quantile)),
         gelman_difference_in_qs_overall_score_zeropremove = if_else(post_move == 0, 0, gelman_difference_in_qs_overall_score),
         gelman_origin_qs_overall_score_mean = effectsize::standardize(origin_qs_overall_score_mean, two_sd=T))

diffindiff_data_only_movers_leiden_diff <- matched_dataset %>% #the Leiden ranking moderation dataset
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 1,
         !is.na(difference_in_pptop10_quantile)) %>% 
  mutate(difference_in_pptop10_quantile_zeropremove = if_else(post_move == 0, 0, difference_in_pptop10_quantile),
         gelman_difference_in_pptop10_zeropremove = if_else(post_move == 0, 0, gelman_difference_in_pptop10),
         gelman_origin_pp_top10_mean = effectsize::standardize(origin_pp_top10_mean, two_sd = T))

# step 2b. Running the moderation analysis + making individual plots
# Publications
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

#Normalised citation score
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

#normalised Journal score
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

#Top journals
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

# Top cited papers
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

#step 2c. Making a panel plot that contains all of the moderation analyses + histograms of the raw ranking_difference data

qs_difference_plot <- ggplot(diffindiff_data_only_movers_qs_diff %>% distinct(cluster_id, .keep_all = T), aes(x=gelman_difference_in_qs_overall_score)) + #Histogram of difference in QS rankings
  geom_histogram()+
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,125), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))


leiden_difference_plot <- ggplot(diffindiff_data_only_movers_leiden_diff %>% distinct(cluster_id, .keep_all = T), aes(x = gelman_difference_in_pptop10)) + #histogram of difference in Leiden rankings
  geom_histogram() +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,125), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

interaction_plots_left <- ggarrange(pfull_qs_moderation_plot, #making the left half of the panel plot
          ncs_full_qs_moderation_plot, 
          njs_full_qs_moderation_plot,
          njs_topjournals_qs_moderation_plot, 
          p_top_prop10_full_qs_moderation_plot,
          qs_difference_plot,
          common.legend = T, legend = "bottom", ncol=1,nrow=6, labels = "auto",hjust = -3, vjust = 2,align = "v") %>% 
  annotate_figure(top = text_grob("QS", face = "bold"))

interaction_plots_right <- ggarrange(pfull_pptop10_moderation_plot, #making the right half of the panel plot
                                     ncs_full_pptop10_moderation_plot,
                                     njs_full_pptop10_moderation_plot, 
                                     njs_topjournals_pptop10_moderation_plot,
                                     p_top_prop10_full_pptop10_moderation_plot,
                                     leiden_difference_plot,
                                     common.legend = T, legend = "bottom", ncol=1,nrow=6, align = "v") %>% 
  annotate_figure(top = text_grob("Leiden", face="bold"))

moderation_plot_grid <- # this makes the final panel plot of the moderation analyses
  ggarrange(interaction_plots_left, interaction_plots_right) %>% 
  annotate_figure(left = text_grob("Count                                                     Estimated Coefficient for moving to USA",rot = 90, size = 10, hjust = .56),
                  bottom = text_grob("Standardised difference in ranking score (positive = USA higher ranked)", size = 10))
ggexport(moderation_plot_grid, filename = "plots/Fig3. Moderation.pdf") #saving the plot

# step 2d. Creating tables of the modeation results
tab_model(pfull_qs, #table of QS ranking moderation
          ncs_full_qs,
          njs_full_qs,
          njs_topjournals_qs,
          p_top_prop10_full_qs,
          show.p = F,
          show.re.var =F,
          file = "tables/table2 moderation qs.doc")

tab_model(pfull_pptop10, #table of leiden ranking moderation
          ncs_full_pptop10,
          njs_full_pptop10,
          njs_topjournals_pptop10,
          p_top_prop10_full_pptop10,
          show.p = F,
          show.re.var =F,
          file = "tables/table3 moderation leiden.doc")

###################################################################
############ ANALYSIS STEP 3. MEDIATION ##########################
##################################################################

#Step 3a. running the mediation analysis#
#### QS RANKING ######
# p full
set.seed(5030)
detach("package:lmerTest", unload = T)
pfull_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
pfull_qs_out.fit <- lmer(p_full_yearsum ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

pfull_qs_med.out <- mediate(pfull_qs_med.fit, pfull_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 1000)
med_qs_pubs <- summary(pfull_qs_med.out)

# citation score
ncs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(ncs_full_mean))
ncs_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean + (1|cluster_id), data = ncs_qs_mediation_data)
ncs_qs_out.fit <- lmer(ncs_full_mean ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = ncs_qs_mediation_data)

ncs_qs_med.out <- mediate(ncs_qs_out.fit, ncs_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 1000)
med_qs_ncs <- summary(ncs_qs_med.out)

# journal score
njs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(njs_full_mean))
njs_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = njs_qs_mediation_data)
njs_qs_out.fit <- lmer(njs_full_mean ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = njs_qs_mediation_data)

njs_qs_med.out <- mediate(njs_qs_med.fit, njs_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 1000)
med_qs_njs <- summary(njs_qs_med.out)

# top journals
topjoural_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
topjournal_qs_out.fit <- lmer(njs_full_over2_yearsum ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

topjournal_qs_med.out <- mediate(topjoural_qs_med.fit, topjournal_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 1000)
med_qs_topjournals <- summary(topjournal_qs_med.out)

# top 10%
top10_qs_med.fit <- lmer(gelman_difference_in_qs_overall_score_zeropremove ~ post_move + career_year + origin_qs_overall_score_mean+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
top10_qs_out.fit <- lmer(p_top_prop10_full_yearsum ~ gelman_difference_in_qs_overall_score_zeropremove + post_move + career_year + origin_qs_overall_score_mean +(1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

top10_qs_med.out <- mediate(top10_qs_med.fit, top10_qs_out.fit, treat = "post_move", mediator = "gelman_difference_in_qs_overall_score_zeropremove", robustSE = F, sims = 1000)
med_qs_pptop10 <- summary(top10_qs_med.out)

##### LEIDEN RANKING ######

# p full
pfull_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
pfull_out.fit <- lmer(p_full_yearsum ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

pfull_med.out <- mediate(pfull_med.fit, pfull_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = T, sims = 1000)
med_leiden_pubs <- summary(pfull_med.out)

# citation score
ncs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(ncs_full_mean))
ncs_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = ncs_mediation_data)
ncs_out.fit <- lmer(ncs_full_mean ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = ncs_mediation_data)

ncs_med.out <- mediate(ncs_med.fit, ncs_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 1000)
med_leiden_ncs <- summary(ncs_med.out)

# journal score
njs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(njs_full_mean))
njs_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = njs_mediation_data)
njs_out.fit <- lmer(njs_full_mean ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = njs_mediation_data)

njs_med.out <- mediate(njs_med.fit, njs_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 1000)
med_leiden_njs <- summary(njs_med.out)

# top journals
topjoural_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
topjournal_out.fit <- lmer(njs_full_over2_yearsum ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

topjournal_med.out <- mediate(topjoural_med.fit, topjournal_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 1000)
med_leiden_topjournals <- summary(topjournal_med.out)

# top 10%
top10_med.fit <- lmer(gelman_difference_in_pptop10_zeropremove ~ post_move + career_year + origin_pp_top10_mean+ (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
top10_out.fit <- lmer(p_top_prop10_full_yearsum ~ gelman_difference_in_pptop10_zeropremove + post_move + career_year + origin_pp_top10_mean +(1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

top10_med.out <- mediate(top10_med.fit, top10_out.fit, treat = "post_move", mediator = "gelman_difference_in_pptop10_zeropremove", robustSE = F, sims = 1000)
med_leiden_pptop10 <- summary(top10_med.out)

#step 3b creating a table with the mediation analysis results#
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