#importing data and packages 
#robustness checks
load("matched_dataset.RData") #for those downloading the code, you should use load("data_to_be_shared.RData") instead 

pacman::p_load(sjPlot, cowplot, did, lmerTest, ggpubr, interplot,mediation) #https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
detach("package:dplyr", unload = TRUE)
library(dplyr)
set.seed(5030)

################################################################################
########## ROBUSTNESS CHECK 1. DID with control for prior performance ##########
################################################################################

matched_dataset_robustnesscheck %>% distinct(pair_id) %>% dim() #958 matched pairs

# Check 1, step 1 - Running the Difference-in-difference models #
# Publications #

controlled_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                          gname = "moving_year_plus1",
                          idname = "cluster_id",
                          tname = "career_year_plus_1",
                          xformla = ~ 1,  
                          data = matched_dataset_robustnesscheck,
                          est_method = "dr",
                          control_group = "nevertreated",
                          anticipation = 1,
                          allow_unbalanced_panel = T) 

controlled_did_model_pfull_dynamic_short <- aggte(controlled_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_pfull_dynamic_short)
controlled_p_full_did_plot <- ggdid(controlled_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (year sum)")

# Normalised citation score #

controlled_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1,
                                      data = matched_dataset_robustnesscheck,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T)  
#will give an error about missing data (because people don't always publish in a given year...)

controlled_did_model_ncs_full_dynamic_short <- aggte(controlled_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_ncs_full_dynamic_short)
controlled_ncs_full_did_plot <- ggdid(controlled_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Citation score (year mean)")

# Normalised journal score #
controlled_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = matched_dataset_robustnesscheck,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) 
#will give an error about missing data (because people don't always publish in a given year...)

controlled_did_model_njs_full_dynamic_short <- aggte(controlled_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_njs_full_dynamic_short)
controlled_njs_full_did_plot <- ggdid(controlled_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Journal score (year mean)")

# Number of "top journals" (njs >2 ) #
controlled_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1,
                                    data = matched_dataset_robustnesscheck,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

controlled_did_model_njs_topjournals_dynamic_short <- aggte(controlled_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_njs_topjournals_dynamic_short)
controlled_njs_topjournals_did_plot <- ggdid(controlled_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top journal publications (year sum)")

# Top cited papers #
controlled_did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1,
                                     data = matched_dataset_robustnesscheck,
                                     est_method = "dr",
                                     control_group = "notyettreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

controlled_did_model_p_top_prop10_full_dynamic_short <- aggte(controlled_did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_p_top_prop10_full_dynamic_short)
controlled_pp10_full_did_plot <- ggdid(controlled_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top cited publications (year sum)")

# Check 1, step 2 - making a panel plot for all the models in Check 1 #
robustness_check_performance_control <- ggarrange(controlled_p_full_did_plot, 
          controlled_ncs_full_did_plot,
          controlled_njs_full_did_plot, 
          controlled_njs_topjournals_did_plot,
          controlled_pp10_full_did_plot,
          labels = "AUTO", common.legend = T, legend = "bottom", ncol=2,nrow=3, hjust=-2) %>% 
  annotate_figure(top = text_grob("Difference-in-differences results using a sample matched on prior performance"))

ggexport(robustness_check_performance_control, filename = "plots/S6_robustness_performancecontrol.pdf")

####################################################################################
########### ROBUSTNESS CHECK 2. DID with only leiden ranked institutions ###########
####################################################################################

# Check 2, step 1 - making a dataset that contains only leiden origin researchers #
onlyleiden_matched_dataset <- matched_dataset %>% 
  filter(origin_leiden_ranked == 1)

onlyleiden_matched_dataset %>% distinct(pair_id) %>% dim() #1594

# Check 2, step 2 - running the difference in difference analysis #

#Publications
leidenonly_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~ 1,  
                                     data = onlyleiden_matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) 

leidenonly_did_model_pfull_dynamic_short <- aggte(leidenonly_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_pfull_dynamic_short)
leidenonly_p_full_did_plot <- ggdid(leidenonly_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (year sum)")

# Normalised citation score
leidenonly_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                                 gname = "moving_year_plus1",
                                                 idname = "cluster_id",
                                                 tname = "career_year_plus_1",
                                                 xformla = ~1,
                                                 data = onlyleiden_matched_dataset,
                                                 est_method = "dr",
                                                 control_group = "nevertreated",
                                                 anticipation = 1,
                                                 allow_unbalanced_panel = T) 
#will give an error about missing data (because people don't always publish in a given year...)

leidenonly_did_model_ncs_full_dynamic_short <- aggte(leidenonly_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_ncs_full_dynamic_short)
leidenonly_ncs_full_did_plot <- ggdid(leidenonly_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Citation score (year mean)")

# Normalised journal score
leidenonly_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1,
                                       data = onlyleiden_matched_dataset,
                                       est_method = "dr",
                                       control_group = "nevertreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

leidenonly_did_model_njs_full_dynamic_short <- aggte(leidenonly_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_njs_full_dynamic_short)
leidenonly_njs_full_did_plot <- ggdid(leidenonly_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Journal score (year mean)")


# Top journal publications
leidenonly_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                               gname = "moving_year_plus1",
                                               idname = "cluster_id",
                                               tname = "career_year_plus_1",
                                               xformla = ~1,
                                               data = onlyleiden_matched_dataset,
                                               est_method = "dr",
                                               control_group = "nevertreated",
                                               anticipation = 1,
                                               allow_unbalanced_panel = T) 
#will give an error about missing data (because people don't always publish in a given year...)

leidenonly_did_model_njs_topjournals_dynamic_short <- aggte(leidenonly_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_njs_topjournals_dynamic_short)
leidenonly_njs_topjournals_did_plot <- ggdid(leidenonly_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top journal publications (year sum)")

# top cited papers
leidenonly_did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                                                gname = "moving_year_plus1",
                                                idname = "cluster_id",
                                                tname = "career_year_plus_1",
                                                xformla = ~1,
                                                data = onlyleiden_matched_dataset,
                                                est_method = "dr",
                                                control_group = "notyettreated",
                                                anticipation = 1,
                                                allow_unbalanced_panel = T) 

leidenonly_did_model_p_top_prop10_full_dynamic_short <- aggte(leidenonly_did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_p_top_prop10_full_dynamic_short)
leidenonly_pp10_full_did_plot <- ggdid(leidenonly_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top cited publications (year sum)")

# Check 2, step 3 - making panel plot of the check 2 DID models #

robustness_check_leiden_only <- ggarrange(leidenonly_p_full_did_plot, 
          leidenonly_ncs_full_did_plot,
          leidenonly_njs_full_did_plot, 
          leidenonly_njs_topjournals_did_plot,
          leidenonly_pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3, hjust=-2)%>% 
  annotate_figure(top = text_grob("Difference-in-differences results with an only-leiden ranked origin institute sample"))

ggexport(robustness_check_leiden_only, filename = "plots/S7_robustness_leiden_only.pdf")

#######################################################################################################
######## ROBUSTNESS CHECK 3. DID with the comparison group those who had not moved yet ################
#######################################################################################################

# check 3, step 1 - making a dataset of only movers #
notyettreated_matched_dataset <- matched_dataset %>% 
  filter(condition_numeric == 1)

#check 3, step 2 - running the difference-in-difference analysis

notyettreated_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~ 1,  
                                     data = notyettreated_matched_dataset,
                                     est_method = "dr",
                                     control_group = "notyettreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) 

notyettreated_did_model_pfull_dynamic_short <- aggte(notyettreated_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_pfull_dynamic_short)
notyettreated_p_full_did_plot <- ggdid(notyettreated_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (year sum)")

## normalised citation score
#year mean
notyettreated_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                                 gname = "moving_year_plus1",
                                                 idname = "cluster_id",
                                                 tname = "career_year_plus_1",
                                                 xformla = ~1,
                                                 data = notyettreated_matched_dataset,
                                                 est_method = "dr",
                                                 control_group = "notyettreated",
                                                 anticipation = 1,
                                                 allow_unbalanced_panel = T) 
#will give an error about missing data (because people don't always publish in a given year...)

notyettreated_did_model_ncs_full_dynamic_short <- aggte(notyettreated_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_ncs_full_dynamic_short)
notyettreated_ncs_full_did_plot <- ggdid(notyettreated_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Citation score (year mean)")

# normalised journal score full
notyettreated_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1,
                                       data = notyettreated_matched_dataset,
                                       est_method = "dr",
                                       control_group = "notyettreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T) 
#will give an error about missing data (because people don't always publish in a given year...)

notyettreated_did_model_njs_full_dynamic_short <- aggte(notyettreated_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_njs_full_dynamic_short)
notyettreated_njs_full_did_plot <- ggdid(notyettreated_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Journal score (year mean)")


# normalised journal score - number of "top journals" (njs >2 ) per year
notyettreated_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                               gname = "moving_year_plus1",
                                               idname = "cluster_id",
                                               tname = "career_year_plus_1",
                                               xformla = ~1,
                                               data = notyettreated_matched_dataset,
                                               est_method = "dr",
                                               control_group = "notyettreated",
                                               anticipation = 1,
                                               allow_unbalanced_panel = T) 
#will give an error about missing data (because people don't always publish in a given year...)

notyettreated_did_model_njs_topjournals_dynamic_short <- aggte(notyettreated_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_njs_topjournals_dynamic_short)
notyettreated_njs_topjournals_did_plot <- ggdid(notyettreated_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top journal publications (year sum)")

# p_top_prop10_full
notyettreated_did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                                                gname = "moving_year_plus1",
                                                idname = "cluster_id",
                                                tname = "career_year_plus_1",
                                                xformla = ~1,
                                                data = notyettreated_matched_dataset,
                                                est_method = "dr",
                                                control_group = "notyettreated",
                                                anticipation = 1,
                                                allow_unbalanced_panel = T) 

notyettreated_did_model_p_top_prop10_full_dynamic_short <- aggte(notyettreated_did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_p_top_prop10_full_dynamic_short)
notyettreated_pp10_full_did_plot <- ggdid(notyettreated_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top cited publications (year sum)")

#check 3, step 3 - making panel plots #
robustness_check_notyettreated<- ggarrange(notyettreated_p_full_did_plot, 
          notyettreated_ncs_full_did_plot,
          notyettreated_njs_full_did_plot, 
          notyettreated_njs_topjournals_did_plot,
          notyettreated_pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3, hjust=-2)%>% 
  annotate_figure(top = text_grob("Difference-in-differences results with control group as those that have not yet moved"))

ggexport(robustness_check_notyettreated, filename = "plots/S8_robustness_notyettreated.pdf")

##################################################################################
############## ROBUSTNESS CHECK 4. DID with fractionalised measures ##############
##################################################################################

# Check 4, step 1. Running the difference in difference analysis #

#Publications
did_model_pfrac <- att_gt(yname = "p_frac_yearsum",
                          gname = "moving_year_plus1",
                          idname = "cluster_id",
                          tname = "career_year_plus_1",
                          xformla = ~1,
                          data = matched_dataset,
                          est_method = "dr",
                          control_group = "nevertreated",
                          anticipation = 1,
                          allow_unbalanced_panel = T) 

did_model_pfrac_dynamic_short <- aggte(did_model_pfrac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_pfrac_dynamic_short)
p_frac_did_plot <- ggdid(did_model_pfrac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Frac. publications (year sum)")

#Normalised citation score
did_model_ncs_frac_yearmean <- att_gt(yname = "ncs_frac_mean",
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
did_model_ncs_frac_dynamic_short <- aggte(did_model_ncs_frac_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_ncs_frac_dynamic_short)
ncs_frac_did_plot <- ggdid(did_model_ncs_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Frac. citation score (year mean)")

# Normalised journal score
did_model_njs_frac<- att_gt(yname = "njs_frac_mean",
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

did_model_njs_frac_dynamic_short <- aggte(did_model_njs_frac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_frac_dynamic_short)
njs_frac_did_plot <- ggdid(did_model_njs_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Frac. journal score (year mean)")

#top journals
did_model_njs_topjournals_frac <- att_gt(yname = "njs_full_over2_frac_yearsum",
                                                  gname = "moving_year_plus1",
                                                  idname = "cluster_id",
                                                  tname = "career_year_plus_1",
                                                  xformla = ~1,
                                                  data = matched_dataset,
                                                  est_method = "dr",
                                                  control_group = "notyettreated",
                                                  anticipation = 1,
                                                  allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_njs_topjournals_frac_dynamic_short <- aggte(did_model_njs_topjournals_frac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_topjournals_frac_dynamic_short)
njs_topjournals_frac_did_plot <- ggdid(did_model_njs_topjournals_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Frac. top journal pubs. (year sum)")


# Top cited papers
did_model_p_top_prop10_frac<- att_gt(yname = "p_top_prop10_frac_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1,
                                     data = matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_p_top_prop10_frac_dynamic_short <- aggte(did_model_p_top_prop10_frac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_p_top_prop10_frac_dynamic_short)
pp10_frac_did_plot <- ggdid(did_model_p_top_prop10_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Frac.top cited publications (year sum)")

# Check 4, step 2 - making panel plots of the difference-in-difference results #
robustness_check_fractionalised <- ggarrange(p_frac_did_plot, 
          ncs_frac_did_plot,
          njs_frac_did_plot, 
          njs_topjournals_frac_did_plot,
          pp10_frac_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3, hjust=-2) %>% 
  annotate_figure(top = text_grob("Difference-in-differences results using fractional publication measures"))

ggexport(robustness_check_fractionalised, filename = "plots/S9_robustness_fractionalised.pdf")

################################################################################################
############ ROBUSTNESS CHECK 5. Control participants must publish in the moving year ##########
################################################################################################

# check 5, step 1 - first making a dataset that contains only matched pairs where the "stayer" also published something in the first year. #
pair_ids_to_keep <- matched_dataset %>% 
  filter(condition_numeric == 0,
         career_year == moving_year,
         p_full_yearsum > 0) %>% 
  distinct(pair_id)

published_in_first_year_matched_dataset <- 
  pair_ids_to_keep %>% left_join(matched_dataset, by = "pair_id")

published_in_first_year_matched_dataset %>% distinct(pair_id) %>% dim() #1520 matched pairs

# Check 5, step 2 - running the difference in difference models #
#Publications
firstyear_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                          gname = "moving_year_plus1",
                          idname = "cluster_id",
                          tname = "career_year_plus_1",
                          xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                          data = published_in_first_year_matched_dataset,
                          est_method = "dr",
                          control_group = "nevertreated",
                          anticipation = 1,
                          allow_unbalanced_panel = T)

firstyear_did_model_pfull_dynamic_short <- aggte(firstyear_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_pfull_dynamic_short)
firstyear_p_full_did_plot <- ggdid(firstyear_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (year sum)")


# Normalised citation score
firstyear_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1,
                                      data = published_in_first_year_matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

firstyear_did_model_ncs_full_dynamic_short <- aggte(firstyear_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_ncs_full_dynamic_short)
firstyear_ncs_full_did_plot <- ggdid(firstyear_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Citation score (year mean)")

# Normalised journal score
firstyear_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = published_in_first_year_matched_dataset,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

firstyear_did_model_njs_full_dynamic_short <- aggte(firstyear_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_njs_full_dynamic_short)
firstyear_njs_full_did_plot <- ggdid(firstyear_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Journal score (year mean)")

# Top journal papers
firstyear_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1,
                                    data = published_in_first_year_matched_dataset,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T)
#will give an error about missing data (because people don't always publish in a given year...)

firstyear_did_model_njs_topjournals_dynamic_short <- aggte(firstyear_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_njs_topjournals_dynamic_short)
firstyear_njs_topjournals_did_plot <- ggdid(firstyear_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top journal publications (year sum)")

# Top cited papers
firstyear_did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1,
                                     data = published_in_first_year_matched_dataset,
                                     est_method = "dr",
                                     control_group = "notyettreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) 

firstyear_did_model_p_top_prop10_full_dynamic_short <- aggte(firstyear_did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_p_top_prop10_full_dynamic_short)
firstyear_pp10_full_did_plot <- ggdid(firstyear_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Top cited papers (year sum)")

# Check 5, step 3 - making a panel plot of all these difference in difference analyses #
robustness_check_firstyear <- ggarrange(firstyear_p_full_did_plot, 
          firstyear_ncs_full_did_plot,
          firstyear_njs_full_did_plot, 
          firstyear_njs_topjournals_did_plot,
          firstyear_pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3, hjust=-2) %>% 
  annotate_figure(top = text_grob("Difference-in-differences results using controls who had a publication on the moving year"))

ggexport(robustness_check_firstyear, filename = "plots/S10_robustness_firstyear.pdf")

#######################################################################################
######## ROBUSTNESS CHECK 6 - DOES THE EFFECT CHANGE DEPENDING ON CAREER AGE ##########
#######################################################################################
# check 6, step 1 - getting the models from the "5. analysis.R" script #

#source("3. analysis.R") #i would only run this if you want to run these models below, otherwise just ignore

# check 6, step 2 - using the aggte function within the DID package, to estimate differences in ATT by group (in our case this "group" is career age at move) #
did_model_pfull_group <- aggte(did_model_pfull, type = "group", min_e = -5, max_e = 2)
summary(did_model_pfull_group)
p_full_did_plot_group <- ggdid(did_model_pfull_group, ylab = "Career age", xlab = "Treatment effect", title = "Number of publications (yearly sum)")

did_model_ncs_full_group <- aggte(did_model_ncs_full_yearmean, type = "group", min_e = -5, max_e = 2)
summary(did_model_ncs_full_group)
ncs_full_did_plot_group <- ggdid(did_model_ncs_full_group, ylab = "Career age", xlab = "Treatment effect", title = "Field-normalised citation score (yearly mean)")

did_model_njs_full_group <- aggte(did_model_njs_full, type = "group", min_e = -5, max_e = 2)
summary(did_model_njs_full_group)
njs_full_did_plot_group <- ggdid(did_model_njs_full_group, ylab = "Career age", xlab = "Treatment effect", title = "Field-normalised journal score (yearly mean)")

did_model_njs_topjournals_group <- aggte(did_model_njs_topjournals, type = "group", min_e = -5, max_e = 2)
summary(did_model_njs_topjournals_group)
njs_topjournals_did_plot_group <- ggdid(did_model_njs_topjournals_group, ylab = "Career age", xlab = "Treatment effect", title = "Number of top journal (NJS >2) articles (yearly sum")

did_model_p_top_prop10_full_group <- aggte(did_model_p_top_prop10_full, type = "group", min_e = -5, max_e = 2)
summary(did_model_p_top_prop10_full_group)
pp10_full_did_plot_group <- ggdid(did_model_p_top_prop10_full_group, ylab = "Career age", xlab = "Treatment effect", title = "Number of top 10% cited papers in field (yearly sum)")

#Check 6, step 3 - making a panel plot for the by-career-age analysis #
robustness_check_careeryears <- ggarrange(p_full_did_plot_group, 
          ncs_full_did_plot_group,
          njs_full_did_plot_group, 
          njs_topjournals_did_plot_group,
          pp10_full_did_plot_group,
          common.legend = T, legend = "bottom", ncol=2,nrow=3) %>% 
  annotate_figure(top = text_grob("Difference-in-difference ATTs depending on career year at move"))

ggexport(robustness_check_careeryears, filename = "plots/S11_robustness_careeryears.pdf")

################################################################################################
######### ROBUSTNESS CHECK 7 - moderation using differences between ranking quantiles ##########
################################################################################################

#check 7, step 1 - making datasets for differences between rankings (this same code exists in script "script 5. analysis.R" but i add it here because otherwise you have to go back and run it) #
diffindiff_data_only_movers_qs_diff <- matched_dataset %>% #the QS ranking moderation dataset
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 1,
         !is.na(difference_in_qs_overall_ranking_quantile)) %>% 
  mutate(difference_in_qs_quantile_zeropremove = if_else(post_move == 0, 0, as.double(difference_in_qs_overall_ranking_quantile)))

diffindiff_data_only_movers_leiden_diff <- matched_dataset %>% #the Leiden ranking moderation dataset
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 1,
         !is.na(difference_in_pptop10_quantile)) %>% 
  mutate(difference_in_pptop10_quantile_zeropremove = if_else(post_move == 0, 0, difference_in_pptop10_quantile))

# check 7, step 2 - the actual moderation analysis (#taking only movers and interacting 2 variables: "moving" with "difference in rank from origin to usa") + making marginal moderation figures#

#Publications
RC_pfull_qs <- lmer(p_full_yearsum ~ difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_pfull_qs); plot_model(RC_pfull_qs, type = "int")
RC_pfull_pptop10 <- lmer(p_full_yearsum ~ difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_pfull_pptop10); plot_model(RC_pfull_pptop10, type = "int", show.values = T)

RC_pfull_qs_moderation_plot <- interplot(m = RC_pfull_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.3,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

RC_pfull_pptop10_moderation_plot <- interplot(m = RC_pfull_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.3,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

# Normalised citation score
RC_ncs_full_qs <- lmer(ncs_full_mean ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_ncs_full_qs); plot_model(RC_ncs_full_qs, type = "int")
RC_ncs_full_pptop10 <- lmer(ncs_full_mean ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_ncs_full_pptop10); plot_model(RC_ncs_full_pptop10, type = "int", show.values = T)

RC_ncs_full_qs_moderation_plot <- interplot(m = RC_ncs_full_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.5, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

RC_ncs_full_pptop10_moderation_plot <- interplot(m = RC_ncs_full_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.5, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

# Normalised journal score
RC_njs_full_qs <-  lmer(njs_full_mean ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_njs_full_qs); plot_model(RC_njs_full_qs, type = "int")
RC_njs_full_pptop10 <-  lmer(njs_full_mean ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_njs_full_pptop10); plot_model(RC_njs_full_pptop10, type = "int")

RC_njs_full_qs_moderation_plot <- interplot(m = RC_njs_full_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7),expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

RC_njs_full_pptop10_moderation_plot <- interplot(m = RC_njs_full_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

# Top journal papers
RC_njs_topjournals_qs <-  lmer(njs_full_over2_yearsum ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_njs_topjournals_qs); plot_model(RC_njs_topjournals_qs, type = "int")
RC_njs_topjournals_pptop10 <-  lmer(njs_full_over2_yearsum ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_njs_topjournals_pptop10); plot_model(RC_njs_topjournals_pptop10, type = "int")

RC_njs_topjournals_qs_moderation_plot <- interplot(m = RC_njs_topjournals_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

RC_njs_topjournals_pptop10_moderation_plot <- interplot(m = RC_njs_topjournals_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

#Top cited papers
RC_p_top_prop10_full_qs <-  lmer(p_top_prop10_full_yearsum ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_p_top_prop10_full_qs); plot_model(RC_p_top_prop10_full_qs, type = "int")
RC_p_top_prop10_full_pptop10 <-lmer(p_top_prop10_full_yearsum ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_p_top_prop10_full_pptop10); plot_model(RC_p_top_prop10_full_pptop10, type = "int")

RC_p_top_prop10_full_qs_moderation_plot <- interplot(m = RC_p_top_prop10_full_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

RC_p_top_prop10_full_pptop10_moderation_plot <- interplot(m = RC_p_top_prop10_full_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=0, linetype="dotted")

# Check 7, step 3 - a complete panel plot for the moderation analysis
RC_qs_difference_plot <- ggplot(diffindiff_data_only_movers_qs_diff %>% distinct(cluster_id, .keep_all = T), aes(x=difference_in_qs_overall_ranking_quantile)) + #making histogram of differences in QS rankings quantiles
  geom_histogram() + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,190), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_leiden_difference_plot <- ggplot(diffindiff_data_only_movers_leiden_diff %>% distinct(cluster_id, .keep_all = T), aes(x = difference_in_pptop10_quantile)) +#making histogram of differences in Leiden ranking quantiles
  geom_histogram() +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,190), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_interaction_plots_left <- ggarrange(RC_pfull_qs_moderation_plot,#Making left half of panel plot
                                    RC_ncs_full_qs_moderation_plot, 
                                    RC_njs_full_qs_moderation_plot,
                                    RC_njs_topjournals_qs_moderation_plot, 
                                    RC_p_top_prop10_full_qs_moderation_plot,
                                    RC_qs_difference_plot,
                                    common.legend = T, legend = "bottom", ncol=1,nrow=6, 
                                    labels = c("a. Publications", "b. Citation Score", "c. Journal Score", "d. Top journal publications", "e. Top cited publications", "f. Ranking change"),
                                    hjust = -0.1, vjust = -0.1,
                                    align = "hv",
                                    font.label = list(size = 10, color = "black", face = "bold.italic", family = NULL)) %>% 
  annotate_figure(top = text_grob("QS", face = "bold"))

RC_interaction_plots_right <- ggarrange(RC_pfull_pptop10_moderation_plot, #Making right half of panel plot
                                        RC_ncs_full_pptop10_moderation_plot,
                                        RC_njs_full_pptop10_moderation_plot, 
                                        RC_njs_topjournals_pptop10_moderation_plot,
                                        RC_p_top_prop10_full_pptop10_moderation_plot,
                                        RC_leiden_difference_plot,
                                        common.legend = T, legend = "bottom", ncol=1,nrow=6, align = "v") %>% 
  annotate_figure(top = text_grob("Leiden", face="bold"))

RC_moderation_plot_grid <- ggarrange(RC_interaction_plots_left, RC_interaction_plots_right) %>% #putting the two halves together
  annotate_figure(left = text_grob("Count                                             Estimated Coefficient for effect of moving to USA",rot = 90, size = 10, hjust = .56),
                  bottom = text_grob("Difference in ranking quantile (positive = USA higher ranked)", size = 10))
ggexport(RC_moderation_plot_grid, filename = "plots/S12_robustness_moderation.pdf") #saving the resulting plot


################################################################################################################
##########  ROBUSTNESS CHECK 8 - assessing mediation using the differences in university ranking quantile #######
################################################################################################################

# Check 8, step 1 - running the medation analyses #
#QS RANKING
# Publications
detach("package:lmerTest", unload = T)
RC_pfull_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
RC_pfull_qs_out.fit <- lmer(p_full_yearsum ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

RC_pfull_qs_med.out <- mediate(RC_pfull_qs_med.fit, RC_pfull_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_pubs <- summary(RC_pfull_qs_med.out)

# Normalised citation score
RC_ncs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(ncs_full_mean))
RC_ncs_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles + (1|cluster_id), data = RC_ncs_qs_mediation_data)
RC_ncs_qs_out.fit <- lmer(ncs_full_mean ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = RC_ncs_qs_mediation_data)

RC_ncs_qs_med.out <- mediate(RC_ncs_qs_out.fit, RC_ncs_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_ncs <- summary(RC_ncs_qs_med.out)

# Normalised journal score
RC_njs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(njs_full_mean))
RC_njs_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = RC_njs_qs_mediation_data)
RC_njs_qs_out.fit <- lmer(njs_full_mean ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = RC_njs_qs_mediation_data)

RC_njs_qs_med.out <- mediate(RC_njs_qs_med.fit, RC_njs_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_njs <- summary(RC_njs_qs_med.out)

# Top journal papers
RC_topjoural_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
RC_topjournal_qs_out.fit <- lmer(njs_full_over2_yearsum ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

RC_topjournal_qs_med.out <- mediate(RC_topjoural_qs_med.fit, RC_topjournal_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_topjournals <- summary(RC_topjournal_qs_med.out)

# Top cited papers
RC_top10_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
RC_top10_qs_out.fit <- lmer(p_top_prop10_full_yearsum ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles +(1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

RC_top10_qs_med.out <- mediate(RC_top10_qs_med.fit, RC_top10_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_pptop10 <- summary(RC_top10_qs_med.out)

# LEIDEN RANKING

# Publiations
RC_pfull_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
RC_pfull_out.fit <- lmer(p_full_yearsum ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

RC_pfull_med.out <- mediate(RC_pfull_med.fit, RC_pfull_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = T, sims = 100)
RC_med_leiden_pubs <- summary(RC_pfull_med.out)

# Normalised citation score
RC_ncs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(ncs_full_mean))
RC_ncs_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), da = RC_ncs_mediation_data)
RC_ncs_out.fit <- lmer(ncs_full_mean ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year +origin_pp_top10_mean_quantile + (1|cluster_id), data = RC_ncs_mediation_data)

RC_ncs_med.out <- mediate(RC_ncs_med.fit, RC_ncs_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_ncs <- summary(RC_ncs_med.out)

# Normalised journal score
RC_njs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(njs_full_mean))
RC_njs_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = RC_njs_mediation_data)
RC_njs_out.fit <- lmer(njs_full_mean ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = RC_njs_mediation_data)

RC_njs_med.out <- mediate(RC_njs_med.fit, RC_njs_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_njs <- summary(RC_njs_med.out)

# Top Journal papers
RC_topjoural_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
RC_topjournal_out.fit <- lmer(njs_full_over2_yearsum ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

RC_topjournal_med.out <- mediate(RC_topjoural_med.fit, RC_topjournal_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_topjournals <- summary(RC_topjournal_med.out)

# Top cited papers
RC_top10_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
RC_top10_out.fit <- lmer(p_top_prop10_full_yearsum ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile  +(1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

RC_top10_med.out <- mediate(RC_top10_med.fit, RC_top10_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_pptop10 <- summary(RC_top10_med.out)

# Check 8, step 2 - making a single table that contains all the above mediation robustness checks #

RC_mediation_table <- c("", "", "publications","", "", "ncs","", "", "njs","", "", "top journals", "","", "top cited", "","") %>% 
  rbind(c("ranking", "mediation", "estimate", "CI_l","CI_U", "estimate",  "CI_l","CI_U", "estimate",  "CI_l","CI_U", "estimate", "CI_l","CI_U", "estimate",  "CI_l","CI_U"))%>% 
  rbind(c("QS", "ACME", round(RC_med_qs_pubs$d0, 2), paste(round(RC_med_qs_pubs$d0.ci,2)), round(RC_med_qs_ncs$d0,2), paste(round(RC_med_qs_ncs$d0.ci,2)),round(RC_med_qs_njs$d0,2), paste(round(RC_med_qs_njs$d0.ci,2)),round(RC_med_qs_topjournals$d0,2), paste(round(RC_med_qs_topjournals$d0.ci,2)),round(RC_med_qs_pptop10$d0,2), paste(round(RC_med_qs_pptop10$d0.ci,2)))) %>% 
  rbind(c("QS", "ADE", round(RC_med_qs_pubs$z0, 2), paste(round(RC_med_qs_pubs$z0.ci,2)), round(RC_med_qs_ncs$z0,2), paste(round(RC_med_qs_ncs$z0.ci,2)),round(RC_med_qs_njs$z0,2), paste(round(RC_med_qs_njs$z0.ci,2)),round(RC_med_qs_topjournals$z0,2), paste(round(RC_med_qs_topjournals$z0.ci,2)),round(RC_med_qs_pptop10$z0,2), paste(round(RC_med_qs_pptop10$z0.ci,2)))) %>% 
  rbind(c("QS", "Total Effect", round(RC_med_qs_pubs$tau.coef, 2), paste(round(RC_med_qs_pubs$tau.ci,2)), round(RC_med_qs_ncs$tau.coef,2), paste(round(RC_med_qs_ncs$tau.ci,2)),round(RC_med_qs_njs$tau.coef,2), paste(round(RC_med_qs_njs$tau.ci,2)),round(RC_med_qs_topjournals$tau.coef,2), paste(round(RC_med_qs_topjournals$tau.ci,2)),round(RC_med_qs_pptop10$tau.coef,2), paste(round(RC_med_qs_pptop10$tau.ci,2)))) %>% 
  rbind(c("QS", "Prop. Mediated", round(RC_med_qs_pubs$n0, 2), paste(round(RC_med_qs_pubs$n0.ci,2)), round(RC_med_qs_ncs$n0,2), paste(round(RC_med_qs_ncs$n0.ci,2)),round(RC_med_qs_njs$n0,2), paste(round(RC_med_qs_njs$n0.ci,2)),round(RC_med_qs_topjournals$n0,2), paste(round(RC_med_qs_topjournals$n0.ci,2)),round(RC_med_qs_pptop10$n0,2), paste(round(RC_med_qs_pptop10$n0.ci,2)))) %>% 
  rbind(c("", "", "","", "", "","", "", "","", "", "", "","", "", "","")) %>% 
  rbind(c("Leiden", "ACME", round(RC_med_qs_pubs$d0, 2), paste(round(RC_med_leiden_pubs$d0.ci,2)), round(RC_med_leiden_ncs$d0,2), paste(round(RC_med_leiden_ncs$d0.ci,2)),round(RC_med_leiden_njs$d0,2), paste(round(RC_med_leiden_njs$d0.ci,2)),round(RC_med_leiden_topjournals$d0,2), paste(round(RC_med_leiden_topjournals$d0.ci,2)),round(RC_med_leiden_pptop10$d0,2), paste(round(RC_med_leiden_pptop10$d0.ci,2)))) %>% 
  rbind(c("Leiden", "ADE", round(RC_med_leiden_pubs$z0, 2), paste(round(RC_med_leiden_pubs$z0.ci,2)), round(RC_med_leiden_ncs$z0,2), paste(round(RC_med_leiden_ncs$z0.ci,2)),round(RC_med_leiden_njs$z0,2), paste(round(RC_med_leiden_njs$z0.ci,2)),round(RC_med_leiden_topjournals$z0,2), paste(round(RC_med_leiden_topjournals$z0.ci,2)),round(RC_med_leiden_pptop10$z0,2), paste(round(RC_med_leiden_pptop10$z0.ci,2)))) %>% 
  rbind(c("Leiden", "Total Effect", round(RC_med_leiden_pubs$tau.coef, 2), paste(round(RC_med_leiden_pubs$tau.ci,2)), round(RC_med_leiden_ncs$tau.coef,2), paste(round(RC_med_leiden_ncs$tau.ci,2)),round(RC_med_leiden_njs$tau.coef,2), paste(round(RC_med_leiden_njs$tau.ci,2)),round(RC_med_leiden_topjournals$tau.coef,2), paste(round(RC_med_leiden_topjournals$tau.ci,2)),round(RC_med_leiden_pptop10$tau.coef,2), paste(round(RC_med_leiden_pptop10$tau.ci,2)))) %>% 
  rbind(c("Leiden", "Prop. Mediated", round(RC_med_leiden_pubs$n0, 2), paste(round(RC_med_leiden_pubs$n0.ci,2)), round(RC_med_leiden_ncs$n0,2), paste(round(RC_med_leiden_ncs$n0.ci,2)),round(RC_med_leiden_njs$n0,2), paste(round(RC_med_leiden_njs$n0.ci,2)),round(RC_med_leiden_topjournals$n0,2), paste(round(RC_med_leiden_topjournals$n0.ci,2)),round(RC_med_leiden_pptop10$n0,2), paste(round(RC_med_leiden_pptop10$n0.ci,2)))) %>% 
  as_tibble() %>% 
  unite(col = "pubs_CI",V4:V5, sep = ", ") %>% 
  unite(col = "ncs_CI",V7:V8, sep = ", ") %>% 
  unite(col = "njs_CI",V10:V11, sep = ", ") %>%
  unite(col = "topjournals_CI",V13:V14, sep = ", ") %>%
  unite(col = "pptop10_CI",V16:V17, sep = ", ")

write.csv(RC_mediation_table, "tables/S7. Mediation Quantiles.csv")
########################################################################################################
########## ROBUSTNESS CHECK 9 - subsamples with people moving to different levels of prestige ##########
########################################################################################################

diffindiff_data_only_movers_qs_diff2 <- matched_dataset %>% #the QS ranking moderation dataset
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 1,
         !is.na(gelman_difference_in_qs_overall_score)) %>% 
  mutate(difference_in_qs_quantile_zeropremove = if_else(post_move == 0, 0, as.double(difference_in_qs_overall_ranking_quantile)))

diffindiff_data_only_movers_leiden_diff2 <- matched_dataset %>% #the Leiden ranking moderation dataset
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 1,
         !is.na(gelman_difference_in_pptop10)) %>% 
  mutate(difference_in_pptop10_quantile_zeropremove = if_else(post_move == 0, 0, difference_in_pptop10_quantile))


# Robustness check 9, step 1 - making datasets containing only individuals that can be classed as moving up (over 1SD from mean) or moving down (below 1SD from mean) #
qs_low <- diffindiff_data_only_movers_qs_diff2 %>% filter(gelman_difference_in_qs_overall_score < mean(gelman_difference_in_qs_overall_score, na.rm=T)-sd(gelman_difference_in_qs_overall_score, na.rm=T)) %>% distinct(pair_id) %>% left_join(matched_dataset, by = "pair_id")
qs_high <- diffindiff_data_only_movers_qs_diff2 %>% filter(gelman_difference_in_qs_overall_score > mean(gelman_difference_in_qs_overall_score, na.rm=T)+sd(gelman_difference_in_qs_overall_score, na.rm=T)) %>% distinct(pair_id) %>% left_join(matched_dataset, by = "pair_id")
  
leiden_low <- diffindiff_data_only_movers_leiden_diff2 %>% filter(gelman_difference_in_pptop10 < mean(gelman_difference_in_pptop10, na.rm=T)-sd(gelman_difference_in_pptop10, na.rm=T)) %>% distinct(pair_id) %>% left_join(matched_dataset, by = "pair_id")
leiden_high <- diffindiff_data_only_movers_leiden_diff2 %>% filter(gelman_difference_in_pptop10 > mean(gelman_difference_in_pptop10, na.rm=T)+sd(gelman_difference_in_pptop10, na.rm=T)) %>% distinct(pair_id) %>% left_join(matched_dataset, by = "pair_id")

# Robustness check 9, step 2 - running the difference in difference analysis for the four difference subsamples
# Publications
RC_QS_LOW_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                          gname = "moving_year_plus1",
                          idname = "cluster_id",
                          tname = "career_year_plus_1",
                          xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                          data = qs_low,
                          est_method = "dr",
                          control_group = "nevertreated",
                          anticipation = 1,
                          allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_LOW_did_model_pfull_dynamic_short <- aggte(RC_QS_LOW_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_LOW_did_model_pfull_dynamic_short)
RC_QS_LOW_p_full_did_plot <- ggdid(RC_QS_LOW_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

RC_QS_HIGH_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = qs_high,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_HIGH_did_model_pfull_dynamic_short <- aggte(RC_QS_HIGH_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_HIGH_did_model_pfull_dynamic_short)
RC_QS_HIGH_p_full_did_plot <- ggdid(RC_QS_HIGH_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")


RC_LEIDEN_LOW_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = leiden_low,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_LOW_did_model_pfull_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_LOW_did_model_pfull_dynamic_short)
RC_LEIDEN_LOW_p_full_did_plot <- ggdid(RC_LEIDEN_LOW_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

RC_LEIDEN_HIGH_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                     data = leiden_high,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_HIGH_did_model_pfull_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_HIGH_did_model_pfull_dynamic_short)
RC_LEIDEN_HIGH_p_full_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# Normalised Citation Score

RC_QS_LOW_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = qs_low,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_LOW_did_model_ncs_dynamic_short <- aggte(RC_QS_LOW_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_LOW_did_model_ncs_dynamic_short)
RC_QS_LOW_ncs_did_plot <- ggdid(RC_QS_LOW_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "ncs")

RC_QS_HIGH_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                     data = qs_high,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_HIGH_did_model_ncs_dynamic_short <- aggte(RC_QS_HIGH_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_HIGH_did_model_ncs_dynamic_short)
RC_QS_HIGH_ncs_did_plot <- ggdid(RC_QS_HIGH_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "ncs")


RC_LEIDEN_LOW_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                        gname = "moving_year_plus1",
                                        idname = "cluster_id",
                                        tname = "career_year_plus_1",
                                        xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                        data = leiden_low,
                                        est_method = "dr",
                                        control_group = "nevertreated",
                                        anticipation = 1,
                                        allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_LOW_did_model_ncs_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_LOW_did_model_ncs_dynamic_short)
RC_LEIDEN_LOW_ncs_did_plot <- ggdid(RC_LEIDEN_LOW_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "ncs")

RC_LEIDEN_HIGH_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = leiden_high,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_HIGH_did_model_ncs_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_HIGH_did_model_ncs_dynamic_short)
RC_LEIDEN_HIGH_ncs_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# Normalised Journal Score

RC_QS_LOW_did_model_njs <- att_gt(yname = "njs_full_mean",
                                  gname = "moving_year_plus1",
                                  idname = "cluster_id",
                                  tname = "career_year_plus_1",
                                  xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                  data = qs_low,
                                  est_method = "dr",
                                  control_group = "nevertreated",
                                  anticipation = 1,
                                  allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_LOW_did_model_njs_dynamic_short <- aggte(RC_QS_LOW_did_model_njs, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_LOW_did_model_njs_dynamic_short)
RC_QS_LOW_njs_did_plot <- ggdid(RC_QS_LOW_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "njs")

RC_QS_HIGH_did_model_ncj <- att_gt(yname = "njs_full_mean",
                                   gname = "moving_year_plus1",
                                   idname = "cluster_id",
                                   tname = "career_year_plus_1",
                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                   data = qs_high,
                                   est_method = "dr",
                                   control_group = "nevertreated",
                                   anticipation = 1,
                                   allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_HIGH_did_model_njs_dynamic_short <- aggte(RC_QS_HIGH_did_model_ncj, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_HIGH_did_model_njs_dynamic_short)
RC_QS_HIGH_njs_did_plot <- ggdid(RC_QS_HIGH_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "njs")


RC_LEIDEN_LOW_did_model_njs <- att_gt(yname = "njs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = leiden_low,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_LOW_did_model_njs_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_njs, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_LOW_did_model_njs_dynamic_short)
RC_LEIDEN_LOW_njs_did_plot <- ggdid(RC_LEIDEN_LOW_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "ncs")

RC_LEIDEN_HIGH_did_model_njs <- att_gt(yname = "njs_full_mean",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                       data = leiden_high,
                                       est_method = "dr",
                                       control_group = "nevertreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_HIGH_did_model_njs_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_njs, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_HIGH_did_model_njs_dynamic_short)
RC_LEIDEN_HIGH_njs_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# Top journal papers

RC_QS_LOW_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                  gname = "moving_year_plus1",
                                  idname = "cluster_id",
                                  tname = "career_year_plus_1",
                                  xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                  data = qs_low,
                                  est_method = "dr",
                                  control_group = "nevertreated",
                                  anticipation = 1,
                                  allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_LOW_did_model_topjournals_dynamic_short <- aggte(RC_QS_LOW_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_LOW_did_model_topjournals_dynamic_short)
RC_QS_LOW_topjournals_did_plot <- ggdid(RC_QS_LOW_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "njs")

RC_QS_HIGH_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                   gname = "moving_year_plus1",
                                   idname = "cluster_id",
                                   tname = "career_year_plus_1",
                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                   data = qs_high,
                                   est_method = "dr",
                                   control_group = "nevertreated",
                                   anticipation = 1,
                                   allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_HIGH_did_model_topjournals_dynamic_short <- aggte(RC_QS_HIGH_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_HIGH_did_model_topjournals_dynamic_short)
RC_QS_HIGH_topjournals_did_plot <- ggdid(RC_QS_HIGH_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "njs")


RC_LEIDEN_LOW_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = leiden_low,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_LOW_did_model_topjournals_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_LOW_did_model_topjournals_dynamic_short)
RC_LEIDEN_LOW_topjournals_did_plot <- ggdid(RC_LEIDEN_LOW_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "ncs")

RC_LEIDEN_HIGH_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                       data = leiden_high,
                                       est_method = "dr",
                                       control_group = "nevertreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short)
RC_LEIDEN_HIGH_topjournals_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# Top cited papers
RC_QS_LOW_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                          gname = "moving_year_plus1",
                                          idname = "cluster_id",
                                          tname = "career_year_plus_1",
                                          xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                          data = qs_low,
                                          est_method = "dr",
                                          control_group = "nevertreated",
                                          anticipation = 1,
                                          allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_LOW_did_model_topcited_dynamic_short <- aggte(RC_QS_LOW_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_LOW_did_model_topcited_dynamic_short)
RC_QS_LOW_topcited_did_plot <- ggdid(RC_QS_LOW_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "njs")

RC_QS_HIGH_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                           gname = "moving_year_plus1",
                                           idname = "cluster_id",
                                           tname = "career_year_plus_1",
                                           xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                           data = qs_high,
                                           est_method = "dr",
                                           control_group = "nevertreated",
                                           anticipation = 1,
                                           allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_QS_HIGH_did_model_topcited_dynamic_short <- aggte(RC_QS_HIGH_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_QS_HIGH_did_model_topcited_dynamic_short)
RC_QS_HIGH_topcited_did_plot <- ggdid(RC_QS_HIGH_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "njs")


RC_LEIDEN_LOW_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                              gname = "moving_year_plus1",
                                              idname = "cluster_id",
                                              tname = "career_year_plus_1",
                                              xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                              data = leiden_low,
                                              est_method = "dr",
                                              control_group = "nevertreated",
                                              anticipation = 1,
                                              allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_LOW_did_model_topcited_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_LOW_did_model_topcited_dynamic_short)
RC_LEIDEN_LOW_topcited_did_plot <- ggdid(RC_LEIDEN_LOW_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "ncs")

RC_LEIDEN_HIGH_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                               gname = "moving_year_plus1",
                                               idname = "cluster_id",
                                               tname = "career_year_plus_1",
                                               xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                               data = leiden_high,
                                               est_method = "dr",
                                               control_group = "nevertreated",
                                               anticipation = 1,
                                               allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

RC_LEIDEN_HIGH_did_model_topcited_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2,na.rm = TRUE)
summary(RC_LEIDEN_HIGH_did_model_topcited_dynamic_short)
RC_LEIDEN_HIGH_topcited_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Check 9, step 3 - making a table of the different ATTs from the 4x5 DID analysis just performed
low_high_rankings_table <- c("", "publications","", "ncs", "", "njs", "", "top journals", "", "top cited", "") %>% 
  rbind(c("QS low ATT", round(c(RC_QS_LOW_did_model_pfull_dynamic_short$overall.att, RC_QS_LOW_did_model_pfull_dynamic_short$overall.se, RC_QS_LOW_did_model_ncs_dynamic_short$overall.att, RC_QS_LOW_did_model_ncs_dynamic_short$overall.se, RC_QS_LOW_did_model_njs_dynamic_short$overall.att,RC_QS_LOW_did_model_njs_dynamic_short$overall.se, RC_QS_LOW_did_model_topjournals_dynamic_short$overall.att, RC_QS_LOW_did_model_topjournals_dynamic_short$overall.se, RC_QS_LOW_did_model_topcited_dynamic_short$overall.att, RC_QS_LOW_did_model_topcited_dynamic_short$overall.se),2))) %>% 
  rbind(c("QS high ATT", round(c(RC_QS_HIGH_did_model_pfull_dynamic_short$overall.att, RC_QS_HIGH_did_model_pfull_dynamic_short$overall.se, RC_QS_HIGH_did_model_ncs_dynamic_short$overall.att, RC_QS_HIGH_did_model_ncs_dynamic_short$overall.se, RC_QS_HIGH_did_model_njs_dynamic_short$overall.att,RC_QS_HIGH_did_model_njs_dynamic_short$overall.se, RC_QS_HIGH_did_model_topjournals_dynamic_short$overall.att, RC_QS_HIGH_did_model_topjournals_dynamic_short$overall.se, RC_QS_HIGH_did_model_topcited_dynamic_short$overall.att, RC_QS_HIGH_did_model_topcited_dynamic_short$overall.se),2))) %>% 
  rbind(c("Leiden low ATT", round(c(RC_LEIDEN_LOW_did_model_pfull_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_pfull_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_ncs_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_ncs_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_njs_dynamic_short$overall.att,RC_LEIDEN_LOW_did_model_njs_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_topjournals_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_topjournals_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_topcited_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_topcited_dynamic_short$overall.se),2))) %>% 
  rbind(c("Leiden high ATT" , round(c(RC_LEIDEN_HIGH_did_model_pfull_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_pfull_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_ncs_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_ncs_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_njs_dynamic_short$overall.att,RC_LEIDEN_HIGH_did_model_njs_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_topcited_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_topcited_dynamic_short$overall.se),2)))

write.csv(low_high_rankings_table, "tables/S8. low_vs_high_ranking_ATTs.csv")

###############################################################################################################################
########## ROBUSTNESS CHECK 10 - checking whether there are regional differences in the size of the movers advantage ##########
###############################################################################################################################
# check 10, step 1 - making seperate datasets for each region #
easterncountries_matched_dataset <- matched_dataset %>% filter(origin_region == "eastern")
northerncountries_matched_dataset <- matched_dataset %>% filter(origin_region == "northern")
southerncountries_matched_dataset <- matched_dataset %>% filter(origin_region == "southern")
westerncountries_matched_dataset <- matched_dataset %>% filter(origin_region == "western")

#check 10, step 2 - running the difference-in-difference analysis (4*5 models) #

################### EASTERN ##################
# Publications
eastern_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = easterncountries_matched_dataset,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) 

eastern_did_model_pfull_dynamic_short <- aggte(eastern_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(eastern_did_model_pfull_dynamic_short)
eastern_p_full_did_plot <- ggdid(eastern_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
eastern_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                  gname = "moving_year_plus1",
                                  idname = "cluster_id",
                                  tname = "career_year_plus_1",
                                  xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                  data = easterncountries_matched_dataset,
                                  est_method = "dr",
                                  control_group = "nevertreated",
                                  anticipation = 1,
                                  allow_unbalanced_panel = T) 

eastern_did_model_ncs_dynamic_short <- aggte(eastern_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(eastern_did_model_ncs_dynamic_short)
eastern_ncs_did_plot <- ggdid(eastern_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

eastern_did_model_njs <- att_gt(yname = "njs_full_mean",
                                gname = "moving_year_plus1",
                                idname = "cluster_id",
                                tname = "career_year_plus_1",
                                xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                data = easterncountries_matched_dataset,
                                est_method = "dr",
                                control_group = "nevertreated",
                                anticipation = 1,
                                allow_unbalanced_panel = T) 

eastern_did_model_njs_dynamic_short <- aggte(eastern_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(eastern_did_model_njs_dynamic_short)
eastern_njs_did_plot <- ggdid(eastern_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
eastern_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                gname = "moving_year_plus1",
                                idname = "cluster_id",
                                tname = "career_year_plus_1",
                                xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                data = easterncountries_matched_dataset,
                                est_method = "dr",
                                control_group = "nevertreated",
                                anticipation = 1,
                                allow_unbalanced_panel = T) 

eastern_did_model_topjournals_dynamic_short <- aggte(eastern_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(eastern_did_model_topjournals_dynamic_short)
eastern_topjournals_did_plot <- ggdid(eastern_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
eastern_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                        gname = "moving_year_plus1",
                                        idname = "cluster_id",
                                        tname = "career_year_plus_1",
                                        xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                        data = easterncountries_matched_dataset,
                                        est_method = "dr",
                                        control_group = "nevertreated",
                                        anticipation = 1,
                                        allow_unbalanced_panel = T) 

eastern_did_model_topcited_dynamic_short <- aggte(eastern_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(eastern_did_model_topcited_dynamic_short)
eastern_topcited_did_plot <- ggdid(eastern_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

################## NORTHERN ###########################
# Publications
northern_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                  gname = "moving_year_plus1",
                                  idname = "cluster_id",
                                  tname = "career_year_plus_1",
                                  xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                  data = northerncountries_matched_dataset,
                                  est_method = "dr",
                                  control_group = "nevertreated",
                                  anticipation = 1,
                                  allow_unbalanced_panel = T) 

northern_did_model_pfull_dynamic_short <- aggte(northern_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(northern_did_model_pfull_dynamic_short)
northern_p_full_did_plot <- ggdid(northern_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
northern_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                gname = "moving_year_plus1",
                                idname = "cluster_id",
                                tname = "career_year_plus_1",
                                xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                data = northerncountries_matched_dataset,
                                est_method = "dr",
                                control_group = "nevertreated",
                                anticipation = 1,
                                allow_unbalanced_panel = T) 

northern_did_model_ncs_dynamic_short <- aggte(northern_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(northern_did_model_ncs_dynamic_short)
northern_ncs_did_plot <- ggdid(northern_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

northern_did_model_njs <- att_gt(yname = "njs_full_mean",
                                gname = "moving_year_plus1",
                                idname = "cluster_id",
                                tname = "career_year_plus_1",
                                xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                data = northerncountries_matched_dataset,
                                est_method = "dr",
                                control_group = "nevertreated",
                                anticipation = 1,
                                allow_unbalanced_panel = T) 

northern_did_model_njs_dynamic_short <- aggte(northern_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(northern_did_model_njs_dynamic_short)
northern_njs_did_plot <- ggdid(northern_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
northern_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                        gname = "moving_year_plus1",
                                        idname = "cluster_id",
                                        tname = "career_year_plus_1",
                                        xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                        data = northerncountries_matched_dataset,
                                        est_method = "dr",
                                        control_group = "nevertreated",
                                        anticipation = 1,
                                        allow_unbalanced_panel = T) 

northern_did_model_topjournals_dynamic_short <- aggte(northern_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(northern_did_model_topjournals_dynamic_short)
northern_topjournals_did_plot <- ggdid(northern_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
northern_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                     data = northerncountries_matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) 

northern_did_model_topcited_dynamic_short <- aggte(northern_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(northern_did_model_topcited_dynamic_short)
northern_topcited_did_plot <- ggdid(northern_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

################################## SOUTHERN #################################

southern_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                   gname = "moving_year_plus1",
                                   idname = "cluster_id",
                                   tname = "career_year_plus_1",
                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                   data = southerncountries_matched_dataset,
                                   est_method = "dr",
                                   control_group = "nevertreated",
                                   anticipation = 1,
                                   allow_unbalanced_panel = T) 

southern_did_model_pfull_dynamic_short <- aggte(southern_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(southern_did_model_pfull_dynamic_short)
southern_p_full_did_plot <- ggdid(southern_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
southern_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                 data = southerncountries_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) 

southern_did_model_ncs_dynamic_short <- aggte(southern_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(southern_did_model_ncs_dynamic_short)
southern_ncs_did_plot <- ggdid(southern_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

southern_did_model_njs <- att_gt(yname = "njs_full_mean",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                 data = southerncountries_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) 

southern_did_model_njs_dynamic_short <- aggte(southern_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(southern_did_model_njs_dynamic_short)
southern_njs_did_plot <- ggdid(southern_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
southern_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = southerncountries_matched_dataset,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) 

southern_did_model_topjournals_dynamic_short <- aggte(southern_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(southern_did_model_topjournals_dynamic_short)
southern_topjournals_did_plot <- ggdid(southern_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
southern_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = southerncountries_matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) 

southern_did_model_topcited_dynamic_short <- aggte(southern_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(southern_did_model_topcited_dynamic_short)
southern_topcited_did_plot <- ggdid(southern_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

################################### WESTERN ##################################
western_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                   gname = "moving_year_plus1",
                                   idname = "cluster_id",
                                   tname = "career_year_plus_1",
                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                   data = westerncountries_matched_dataset,
                                   est_method = "dr",
                                   control_group = "nevertreated",
                                   anticipation = 1,
                                   allow_unbalanced_panel = T) 

western_did_model_pfull_dynamic_short <- aggte(western_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(western_did_model_pfull_dynamic_short)
western_p_full_did_plot <- ggdid(western_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
western_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                 data = westerncountries_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) 

western_did_model_ncs_dynamic_short <- aggte(western_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(western_did_model_ncs_dynamic_short)
western_ncs_did_plot <- ggdid(western_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

western_did_model_njs <- att_gt(yname = "njs_full_mean",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                 data = westerncountries_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) 

western_did_model_njs_dynamic_short <- aggte(western_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(western_did_model_njs_dynamic_short)
western_njs_did_plot <- ggdid(western_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
western_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = westerncountries_matched_dataset,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) 

western_did_model_topjournals_dynamic_short <- aggte(western_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(western_did_model_topjournals_dynamic_short)
western_topjournals_did_plot <- ggdid(western_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
western_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = westerncountries_matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) 

western_did_model_topcited_dynamic_short <- aggte(western_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(western_did_model_topcited_dynamic_short)
western_topcited_did_plot <- ggdid(western_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#####------- making a forest plot -------- #####
region_ATTs <-  c("Publications", "Northern", northern_did_model_pfull_dynamic_short$overall.att, northern_did_model_pfull_dynamic_short$overall.att-1.96*northern_did_model_pfull_dynamic_short$overall.se,northern_did_model_pfull_dynamic_short$overall.att+1.96*northern_did_model_pfull_dynamic_short$overall.se) %>% 
  rbind(c("Publications", "Eastern", eastern_did_model_pfull_dynamic_short$overall.att, eastern_did_model_pfull_dynamic_short$overall.att-1.96*eastern_did_model_pfull_dynamic_short$overall.se,eastern_did_model_pfull_dynamic_short$overall.att+1.96*eastern_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Publications", "Southern", southern_did_model_pfull_dynamic_short$overall.att, southern_did_model_pfull_dynamic_short$overall.att-1.96*southern_did_model_pfull_dynamic_short$overall.se,southern_did_model_pfull_dynamic_short$overall.att+1.96*southern_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Publications", "Western", western_did_model_pfull_dynamic_short$overall.att, western_did_model_pfull_dynamic_short$overall.att-1.96*western_did_model_pfull_dynamic_short$overall.se,western_did_model_pfull_dynamic_short$overall.att+1.96*western_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Northern", northern_did_model_ncs_dynamic_short$overall.att, northern_did_model_ncs_dynamic_short$overall.att-1.96*northern_did_model_ncs_dynamic_short$overall.se,northern_did_model_ncs_dynamic_short$overall.att+1.96*northern_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Eastern", eastern_did_model_ncs_dynamic_short$overall.att, eastern_did_model_ncs_dynamic_short$overall.att-1.96*eastern_did_model_ncs_dynamic_short$overall.se,eastern_did_model_ncs_dynamic_short$overall.att+1.96*eastern_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Southern", southern_did_model_ncs_dynamic_short$overall.att, southern_did_model_ncs_dynamic_short$overall.att-1.96*southern_did_model_ncs_dynamic_short$overall.se,southern_did_model_ncs_dynamic_short$overall.att+1.96*southern_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Western", western_did_model_ncs_dynamic_short$overall.att, western_did_model_ncs_dynamic_short$overall.att-1.96*western_did_model_ncs_dynamic_short$overall.se,western_did_model_ncs_dynamic_short$overall.att+1.96*western_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Northern", northern_did_model_njs_dynamic_short$overall.att, northern_did_model_njs_dynamic_short$overall.att-1.96*northern_did_model_njs_dynamic_short$overall.se,northern_did_model_njs_dynamic_short$overall.att+1.96*northern_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Eastern", eastern_did_model_njs_dynamic_short$overall.att, eastern_did_model_njs_dynamic_short$overall.att-1.96*eastern_did_model_njs_dynamic_short$overall.se,eastern_did_model_njs_dynamic_short$overall.att+1.96*eastern_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Southern", southern_did_model_njs_dynamic_short$overall.att, southern_did_model_njs_dynamic_short$overall.att-1.96*southern_did_model_njs_dynamic_short$overall.se,southern_did_model_njs_dynamic_short$overall.att+1.96*southern_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Western", western_did_model_njs_dynamic_short$overall.att, western_did_model_njs_dynamic_short$overall.att-1.96*western_did_model_njs_dynamic_short$overall.se,western_did_model_njs_dynamic_short$overall.att+1.96*western_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Northern", northern_did_model_topjournals_dynamic_short$overall.att, northern_did_model_topjournals_dynamic_short$overall.att-1.96*northern_did_model_topjournals_dynamic_short$overall.se,northern_did_model_topjournals_dynamic_short$overall.att+1.96*northern_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Eastern", eastern_did_model_topjournals_dynamic_short$overall.att, eastern_did_model_topjournals_dynamic_short$overall.att-1.96*eastern_did_model_topjournals_dynamic_short$overall.se,eastern_did_model_topjournals_dynamic_short$overall.att+1.96*eastern_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Southern", southern_did_model_topjournals_dynamic_short$overall.att, southern_did_model_topjournals_dynamic_short$overall.att-1.96*southern_did_model_topjournals_dynamic_short$overall.se,southern_did_model_topjournals_dynamic_short$overall.att+1.96*southern_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Western", western_did_model_topjournals_dynamic_short$overall.att, western_did_model_topjournals_dynamic_short$overall.att-1.96*western_did_model_topjournals_dynamic_short$overall.se,western_did_model_topjournals_dynamic_short$overall.att+1.96*western_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Northern", northern_did_model_topcited_dynamic_short$overall.att, northern_did_model_topcited_dynamic_short$overall.att-1.96*northern_did_model_topcited_dynamic_short$overall.se,northern_did_model_topcited_dynamic_short$overall.att+1.96*northern_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Eastern", eastern_did_model_topcited_dynamic_short$overall.att, eastern_did_model_topcited_dynamic_short$overall.att-1.96*eastern_did_model_topcited_dynamic_short$overall.se,eastern_did_model_topcited_dynamic_short$overall.att+1.96*eastern_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Southern", southern_did_model_topcited_dynamic_short$overall.att, southern_did_model_topcited_dynamic_short$overall.att-1.96*southern_did_model_topcited_dynamic_short$overall.se,southern_did_model_topcited_dynamic_short$overall.att+1.96*southern_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Western", western_did_model_topcited_dynamic_short$overall.att, western_did_model_topcited_dynamic_short$overall.att-1.96*western_did_model_topcited_dynamic_short$overall.se,western_did_model_topcited_dynamic_short$overall.att+1.96*western_did_model_topcited_dynamic_short$overall.se)) %>% 
  as_tibble()
  
names(region_ATTs) <- c("Measure","Region","Estimate", "95% CI low", "95% CI high")

region_ATTs <- region_ATTs %>% 
  mutate(across("Estimate":"95% CI high", as.numeric),
         Measure = factor(Measure, levels = rev(c("Publications", "Citation Score", "Journal Score", "Top Journals", "Top Cited"))),
         Region = factor(Region, levels = rev(c("Northern", "Eastern", "Southern", "Western"))))


#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "pink", "lightgreen")
barCOLS = c("#008fd5","#de6b35", "indianred2", "springgreen4")


p <- ggplot(region_ATTs, aes(x=Measure, y=Estimate, ymin=`95% CI low`, ymax=`95% CI high`,col=Region,fill=Region)) + 
  #specify position here
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Performance measure") +
  scale_y_continuous(name="Performance improvement (ATT)") +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = T))
p

ggsave(p, filename = "plots/S13. ATT by region.pdf")
