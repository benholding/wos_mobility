#importing data and packages 
load("matched_dataset.RData") #for those downloading the code, you should use load("data_to_be_shared.RData") instead 

pacman::p_load(tidyverse, sjPlot, cowplot, did, lmerTest, ggpubr, interplot,mediation, effectsize,esc, vioplot) #https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
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
p_full_did_plot <- ggdid(did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (year sum)")

esc_B(b = did_model_pfull_dynamic_short$overall.att, sdy = sd(matched_dataset$p_full_yearsum),grp1n = did_model_pfull_dynamic_short$DIDparams$n/2, grp2n = did_model_pfull_dynamic_short$DIDparams$n/2,es.type = c("d"))

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
ncs_full_did_plot <- ggdid(did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Citation score (year mean)")

esc_B(b = did_model_ncs_full_dynamic_short$overall.att, sdy = sd(matched_dataset$ncs_full_mean, na.rm=T),grp1n = did_model_ncs_full_dynamic_short$DIDparams$n/2, grp2n = did_model_ncs_full_dynamic_short$DIDparams$n/2,es.type = c("d"))

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
njs_full_did_plot <- ggdid(did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = " Journal score (year mean)")

esc_B(b = did_model_njs_full_dynamic_short$overall.att, sdy = sd(matched_dataset$njs_full_mean, na.rm=T),grp1n = did_model_njs_full_dynamic_short$DIDparams$n/2, grp2n = did_model_njs_full_dynamic_short$DIDparams$n/2,es.type = c("d"))


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
njs_topjournals_did_plot <- ggdid(did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top journal publications (year sum)")

esc_B(b = did_model_njs_topjournals_dynamic_short$overall.att, sdy = sd(matched_dataset$njs_full_over2_yearsum, na.rm=T),grp1n = did_model_njs_topjournals_dynamic_short$DIDparams$n/2, grp2n = did_model_njs_topjournals_dynamic_short$DIDparams$n/2,es.type = c("d"))

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
pp10_full_did_plot <- ggdid(did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top cited publications (year sum)")

esc_B(b = did_model_p_top_prop10_full_dynamic_short$overall.att, sdy = sd(matched_dataset$p_top_prop10_full_yearsum, na.rm=T),grp1n = did_model_p_top_prop10_full_dynamic_short$DIDparams$n/2, grp2n = did_model_p_top_prop10_full_dynamic_short$DIDparams$n/2,es.type = c("d"))

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
         !is.na(gelman_difference_in_qs_overall_score)) %>% 
  mutate(gelman_difference_in_qs_overall_score_zeropremove = if_else(post_move == 0, 0, gelman_difference_in_qs_overall_score),
         gelman_origin_qs_overall_score_mean = origin_qs_overall_score_mean/(2*sd(origin_qs_overall_score_mean, na.rm = T)))

diffindiff_data_only_movers_leiden_diff <- matched_dataset %>% #the Leiden ranking moderation dataset
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 1,
         !is.na(gelman_difference_in_pptop10)) %>% 
  mutate(gelman_difference_in_pptop10_zeropremove = if_else(post_move == 0, 0, gelman_difference_in_pptop10),
         gelman_origin_pp_top10_mean = origin_pp_top10_mean/(2*sd(origin_pp_top10_mean, na.rm = T)))

# step 2b. Running the moderation analysis + making individual plots
# Publications
pfull_qs <- lmer(p_full_yearsum ~ gelman_difference_in_qs_overall_score:post_move + post_move + career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(pfull_qs); plot_model(pfull_qs, type = "int")
pfull_pptop10 <- lmer(p_full_yearsum ~ gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(pfull_pptop10); plot_model(pfull_pptop10, type = "int", show.values = T)

pfull_qs_moderation_plot <- interplot(m = pfull_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.25,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0.5,0,0.5, 1),labels=c("-2", "-1", "0", "1", "2")) +
  geom_hline(yintercept=0, linetype="dotted")

pfull_pptop10_moderation_plot <- interplot(m = pfull_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.25,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0,1, 2),labels=c("-2", "0", "2", "4")) +
  geom_hline(yintercept=0, linetype="dotted")

#Normalised citation score
ncs_full_qs <- lmer(ncs_full_mean ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(ncs_full_qs); plot_model(ncs_full_qs, type = "int")
ncs_full_pptop10 <- lmer(ncs_full_mean ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(ncs_full_pptop10); plot_model(ncs_full_pptop10, type = "int", show.values = T)

ncs_full_qs_moderation_plot <- interplot(m = ncs_full_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.3, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0.5,0,0.5, 1),labels=c("-2", "-1", "0", "1", "2")) +
  geom_hline(yintercept=0, linetype="dotted")

ncs_full_pptop10_moderation_plot <- interplot(m = ncs_full_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.3, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0,1, 2),labels=c("-2", "0", "2", "4")) +
  geom_hline(yintercept=0, linetype="dotted")

#normalised Journal score
njs_full_qs <-  lmer(njs_full_mean ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(njs_full_qs); plot_model(njs_full_qs, type = "int")
njs_full_pptop10 <-  lmer(njs_full_mean ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(njs_full_pptop10); plot_model(njs_full_pptop10, type = "int")

njs_full_qs_moderation_plot <- interplot(m = njs_full_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7),expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0.5,0,0.5, 1),labels=c("-2", "-1", "0", "1", "2")) +
  geom_hline(yintercept=0, linetype="dotted")

njs_full_pptop10_moderation_plot <- interplot(m = njs_full_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7),expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0,1, 2),labels=c("-2", "0", "2", "4")) +
  geom_hline(yintercept=0, linetype="dotted")

#Top journals
njs_topjournals_qs <-  lmer(njs_full_over2_yearsum ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(njs_topjournals_qs); plot_model(njs_topjournals_qs, type = "int"); omega_squared(njs_topjournals_qs)
njs_topjournals_pptop10 <-  lmer(njs_full_over2_yearsum ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(njs_topjournals_pptop10); plot_model(njs_topjournals_pptop10, type = "int"); omega_squared(njs_topjournals_pptop10)

njs_topjournals_qs_moderation_plot <- interplot(m = njs_topjournals_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0.5,0,0.5, 1),labels=c("-2", "-1", "0", "1", "2")) +
  geom_hline(yintercept=0, linetype="dotted")

njs_topjournals_pptop10_moderation_plot <- interplot(m = njs_topjournals_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0,1, 2),labels=c("-2", "0", "2", "4")) +
  geom_hline(yintercept=0, linetype="dotted")

# Top cited papers
p_top_prop10_full_qs <-  lmer(p_top_prop10_full_yearsum ~gelman_difference_in_qs_overall_score:post_move + post_move +career_year + gelman_origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(p_top_prop10_full_qs); plot_model(p_top_prop10_full_qs, type = "int"); omega_squared(p_top_prop10_full_qs)
p_top_prop10_full_pptop10 <-lmer(p_top_prop10_full_yearsum ~gelman_difference_in_pptop10:post_move + post_move + career_year + gelman_origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(p_top_prop10_full_pptop10); plot_model(p_top_prop10_full_pptop10, type = "int"); omega_squared(p_top_prop10_full_pptop10)

p_top_prop10_full_qs_moderation_plot <- interplot(m = p_top_prop10_full_qs, var1 = "post_move", var2 = "gelman_difference_in_qs_overall_score") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.25,0.85), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0.5,0,0.5, 1),labels=c("-2", "-1", "0", "1", "2")) +
  geom_hline(yintercept=0, linetype="dotted")

p_top_prop10_full_pptop10_moderation_plot <- interplot(m = p_top_prop10_full_pptop10, var1 = "post_move", var2 = "gelman_difference_in_pptop10") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.25,0.85),expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0,1, 2),labels=c("-2", "0", "2", "4")) +
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
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0.5,0,0.5, 1),labels=c("-2", "-1", "0", "1", "2"))
  
leiden_difference_plot <- ggplot(diffindiff_data_only_movers_leiden_diff %>% distinct(cluster_id, .keep_all = T), aes(x = gelman_difference_in_pptop10)) + #histogram of difference in Leiden rankings
  geom_histogram() +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,125), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0),breaks=c(-1,-0,1, 2),labels=c("-2", "0", "2", "4"))
  
interaction_plots_left <- ggarrange(pfull_qs_moderation_plot, #making the left half of the panel plot
          ncs_full_qs_moderation_plot, 
          njs_full_qs_moderation_plot,
          njs_topjournals_qs_moderation_plot, 
          p_top_prop10_full_qs_moderation_plot,
          qs_difference_plot,
          common.legend = T, legend = "bottom", ncol=1,nrow=6, 
          labels = c("a. Publications", "b. Citation Score", "c. Journal Score", "d. Top journal publications", "e. Top cited publications", "f. Ranking change"),
          hjust = -0.1, vjust = -0.1,
          align = "hv",
          font.label = list(size = 10, color = "black", face = "bold.italic", family = NULL)) %>% 
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
  annotate_figure(left = text_grob("Count                                             Estimated Coefficient for effect of moving to USA",rot = 90, size = 10, hjust = .56),
                  bottom = text_grob("Standard deviation change in ranking score (Positive = USA higher ranked)", size = 10))
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

################## ANALYSIS sSTEP 4. DIFFERENCES IN H-INDEX 5 YEARS AFTER THE MOVE ##################

researchers_with_five_years_post_move <- matched_dataset %>% 
  filter(years_from_obtaining_usa_affilation == 5, career_over == 0) %>% 
  group_by(pair_id) %>% 
  mutate(number_in_pair_with5years = n()) %>% 
  filter(number_in_pair_with5years == 2) %>% 
  distinct(cluster_id)

hindex_at_5years_post <- researchers_with_five_years_post_move %>% 
  left_join(matched_dataset) %>%  
  filter(years_from_obtaining_usa_affilation <= 5) %>% 
  group_by(cluster_id) %>% 
  summarise(condition = first(condition),
            sum_ncs = sum(ncs_frac_yearsum),
            post_move_h_index = 0.54*sqrt(sum_ncs)) %>% 
  select(cluster_id, condition, post_move_h_index) #some people have zero, this may be due to missing data in our dataset

h_index_on_year_prior <- researchers_with_five_years_post_move %>% 
  left_join(matched_dataset) %>%  
  filter(years_from_obtaining_usa_affilation < 0) %>% 
  group_by(cluster_id) %>% 
  summarise(condition = first(condition),
            sum_ncs = sum(ncs_frac_yearsum),
            pre_move_h_index = 0.54*sqrt(sum_ncs)) %>% 
  select(cluster_id, condition, pre_move_h_index)

h_index_data <- h_index_on_year_prior %>% left_join(hindex_at_5years_post) %>% mutate(change_in_hindex = post_move_h_index-pre_move_h_index)

h_index_data_new <- h_index_data %>% pivot_longer(!c(cluster_id,condition, change_in_hindex), names_to = "pre_or_post", values_to = "hindex") %>% mutate(pre_or_post = factor(gsub("_h_index", "",pre_or_post), levels = c("pre_move", "post_move"), labels = c("Pre-move", "Post-move")),
                                                                                                                                                         condition = factor(condition, levels = c("stayers", "movers"), labels = c("Stayers", "Movers")))
#descriptive plots for 5-years plot

descriptive_5yearspost_data <- researchers_with_five_years_post_move %>% 
  left_join(matched_dataset) %>% 
  filter(years_from_obtaining_usa_affilation <= 5 & years_from_obtaining_usa_affilation >= -2) %>%
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

descriptive_5yearsplot_pubs.plot <- ggplot(descriptive_5yearspost_data, aes( years_from_obtaining_usa_affilation,mean_p_full, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_line(data = descriptive_5yearspost_data, aes(years_from_obtaining_usa_affilation,median_p_full), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_p_full-se_p_full, ymax = mean_p_full+se_p_full), width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) number of publications") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-2,5,1)) 

descriptive_5yearsplot_ncs.plot <- ggplot(descriptive_5yearspost_data, aes( years_from_obtaining_usa_affilation,mean_ncs_full_mean, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_line(data = descriptive_5yearspost_data, aes(years_from_obtaining_usa_affilation,median_ncs_full_mean), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_ncs_full_mean-se_ncs_full_mean, ymax = mean_ncs_full_mean+se_ncs_full_mean), width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) citation score") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-2,5,1))

descriptive_5yearsplot_njs.plot <- ggplot(descriptive_5yearspost_data, aes( years_from_obtaining_usa_affilation,mean_njs_full_mean, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_line(data = descriptive_5yearspost_data, aes(years_from_obtaining_usa_affilation,median_njs_full_mean), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_njs_full_mean-se_mean_njs_full_mean, ymax = mean_njs_full_mean+se_mean_njs_full_mean),width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) journal score") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-2,5,1)) 

descriptive_5yearsplot_topjournals.plot <- ggplot(descriptive_5yearspost_data, aes( years_from_obtaining_usa_affilation,mean_njs_full_over2_yearsum, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_line(data = descriptive_5yearspost_data, aes(years_from_obtaining_usa_affilation,median_njs_full_over2_yearsum), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_njs_full_over2_yearsum-se_njs_full_over2_yearsum, ymax = mean_njs_full_over2_yearsum+se_njs_full_over2_yearsum),width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) sum of top journal publications") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-2,5,1)) 

descriptive_5yearsplot_topcited.plot <- ggplot(descriptive_5yearspost_data, aes( years_from_obtaining_usa_affilation,mean_p_top_prop10_full_yearsum, group = condition, colour = condition, shape = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_line(data = descriptive_5yearspost_data, aes(years_from_obtaining_usa_affilation,median_p_top_prop10_full_yearsum), linetype = "dotted", size = 1, position = position_jitter(w=0.05, h=0),alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_p_top_prop10_full_yearsum-se_mean_p_top_prop10_full_yearsum, ymax = mean_p_top_prop10_full_yearsum+se_mean_p_top_prop10_full_yearsum),width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("palevioletred","lightblue")) +
  ylab("Average (SE) sum of top cited papers") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-2,5,1))

##

GeomSplitViolin <- ggproto(
  "GeomSplitViolin",
  GeomViolin,
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data,
                      xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x)
    )
    grp <- data[1, "group"]
    newdata <- plyr::arrange(
      transform(data, x = if (grp %% 2 == 1) xminv else xmaxv),
      if (grp %% 2 == 1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname(
        "geom_split_violin",
        grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob)
      )
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function(mapping = NULL,
                              data = NULL,
                              stat = "ydensity",
                              position = "identity", ...,
                              draw_quantiles = NULL,
                              trim = TRUE,
                              scale = "area",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSplitViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm, ...
    )
  )
}


violinplot_hindex <- ggplot(h_index_data_new, aes(pre_or_post,hindex, fill = condition)) + 
  geom_split_violin() + 
  theme_classic()+
  stat_summary(fun = median, geom = "crossbar", 
               width = 0.25,
               position = position_dodge(width = .25),
  ) +
  scale_fill_manual(values = c("lightblue", "palevioletred")) +
  ylab("'fractionalised h-index'") +
  xlab("Pre vs post (5 yrs) move") +
  ylim(0, 2.8)

descriptive_5yearsplot.panelplot <- ggarrange(descriptive_5yearsplot_pubs.plot,descriptive_5yearsplot_ncs.plot,descriptive_5yearsplot_njs.plot,descriptive_5yearsplot_topjournals.plot,descriptive_5yearsplot_topcited.plot,violinplot_hindex, common.legend = T, labels = "AUTO",vjust =-0.0, hjust =-2)
ggsave(descriptive_5yearsplot.panelplot, filename = "plots/Fig4. 5yearplot_a.pdf")

