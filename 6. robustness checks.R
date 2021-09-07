#robustness checks
source("3. matching.R") #importing data and packages

pacman::p_load(sjPlot, cowplot, did, lmerTest, ggpubr, interplot,mediation) #https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
detach("package:dplyr", unload = TRUE)
library(dplyr)

## DID with control for prior performance
matched_robustnesscheck <- matched %>% 
  filter(career_year == year_before_move_movers, #keeping only rows where the year of stayers performance is equal to the time when "movers" left minus 1 year. This removes any "stayer" matches if they weren't active at this timepoint (i.e. quit science)
         final_article_at_origininstitution_year_stayers >= year_before_move_movers, #removes any matches if "stayers" weren't at origin when "mover" had last article before moving
         last_year_stayers-first_year_stayers >= years_between_starting_and_moving_movers+3, #the career length of the "stayers" most be equal to at least 2 years after their match moved to the USA. In this bit of code, I calculate the career length of the stayers, and then make sure it's as long as the "movers'" + 2 years (the code says +3 because the final year in the data refers to the first year of having no publications, so then would allow for a publication gap of at least 2 years)
         p_full_quantile_movers == p_full_quantile,
         #int_collab_quantile_movers == int_collab_quantile,
         #n_coauthors_quantile_movers == n_coauthors_quantile,
         njs_full_quantile_movers == njs_full_quantile #"stayer" matches must be in the same performance decile as "movers" at the time when "movers" left minus 1 year
  ) %>%  
  group_by(cluster_id_movers) %>% 
  mutate(number_of_matches = n()) %>% 
  ungroup()

unique_matched_robustnesscheck <- matched_robustnesscheck %>% 
  mutate(do_specialties_match = if_else(specialty_movers == specialty_stayers, 1, 0)) %>% 
  arrange(number_of_matches, desc(do_specialties_match), desc(end_of_career_year_stayers)) %>% #I match on specialties when possible. I also prefer stayers individuals in science for longer
  distinct(cluster_id_stayers, .keep_all = T) %>% 
  group_by(cluster_id_movers) %>% 
  mutate(number_of_uniquematches = n()) %>% 
  ungroup() %>% 
  distinct(cluster_id_movers, .keep_all=T) %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year = years_between_starting_and_moving_movers, discipline, do_specialties_match, specialty_movers,specialty_stayers) %>% 
  mutate(pair_id = row_number())

cluster_id_and_pair_id_robustnesscheck <- 
  unique_matched_robustnesscheck %>% 
  select(cluster_id_movers, cluster_id_stayers, moving_year, pair_id) %>% 
  pivot_longer(!c(pair_id,moving_year), names_to = "condition", values_to = "cluster_id") %>% 
  mutate(condition = str_replace(condition, "cluster_id_", ""))

matched_dataset_robustnesscheck  <- cluster_id_and_pair_id_robustnesscheck %>% 
  left_join(researcher_performance_years, by = "cluster_id") %>% 
  left_join(researcher_basic_info %>% select(cluster_id, origin_country,origin_region, discipline, specialty,end_of_career_year, gender, difference_in_qs_overall_score,difference_in_qs_reputation_score,difference_in_qs_overall_ranking_quantile, origin_qs_overall_score_mean,origin_qs_reputation_score_mean,origin_qs_overall_rank_quartiles,USA_qs_overall_score_mean,USA_qs_reputation_score_mean,USA_qs_overall_rank_quartiles,origin_pp_top10_mean_quantile,USA_pp_top10_mean_quantile, difference_in_pptop10,difference_in_pptop10_quantile), by = "cluster_id") %>% 
  select(cluster_id, pair_id, moving_year, condition, discipline, specialty, career_year,end_of_career_year, everything()) %>% 
  mutate(years_from_obtaining_usa_affilation = career_year-moving_year,
         post_move = if_else(years_from_obtaining_usa_affilation >= 0, 1, 0),
         late_mover = if_else(career_year >= 6, 1, 0), #dummy variable if mover moves later than the 3rd quartile of movers (i haven't checked this since I changed from months to years)
         career_over = if_else(career_year >= end_of_career_year, 1, 0),
         condition_numeric = if_else(condition == "movers", 1, 0),
         woman = if_else(gender == "F", 1, 0),
         current_qs_ranking = if_else(post_move == 1 & condition_numeric == 1, USA_qs_overall_rank_quartiles, origin_qs_overall_rank_quartiles),
         current_leiden_ranking = if_else(post_move == 1 & condition_numeric == 1, USA_pp_top10_mean_quantile, origin_pp_top10_mean_quantile))%>% 
  filter(career_over == 0) %>% 
  mutate(moving_year_plus1 = moving_year+1, #to add interpretability I added one here. this means that the year that someone starts their career is year 1 instead of year 0
         career_year_plus_1 = career_year+1,
         moving_year_plus1 = if_else(condition == "stayers", 0, moving_year_plus1),
         difference_in_pptop10_quantile_with_zeros = if_else(condition_numeric == 0, 0, difference_in_pptop10_quantile))


controlled_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                          gname = "moving_year_plus1",
                          idname = "cluster_id",
                          tname = "career_year_plus_1",
                          xformla = ~ 1,  
                          data = matched_dataset_robustnesscheck,
                          est_method = "dr",
                          control_group = "nevertreated",
                          anticipation = 1,
                          allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

controlled_did_model_pfull_dynamic_short <- aggte(controlled_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_pfull_dynamic_short)
controlled_p_full_did_plot <- ggdid(controlled_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of publications (yearly sum)")


## normalised citation score
#year mean
controlled_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1,
                                      data = matched_dataset_robustnesscheck,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

controlled_did_model_ncs_full_dynamic_short <- aggte(controlled_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_ncs_full_dynamic_short)
controlled_ncs_full_did_plot <- ggdid(controlled_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised citation score (yearly mean)")

# normalised journal score full
controlled_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = matched_dataset_robustnesscheck,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

controlled_did_model_njs_full_dynamic_short <- aggte(controlled_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_njs_full_dynamic_short)
controlled_njs_full_did_plot <- ggdid(controlled_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised journal score (yearly mean)")


# normalised journal score - number of "top journals" (njs >2 ) per year
controlled_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1,
                                    data = matched_dataset_robustnesscheck,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

controlled_did_model_njs_topjournals_dynamic_short <- aggte(controlled_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(controlled_did_model_njs_topjournals_dynamic_short)
controlled_njs_topjournals_did_plot <- ggdid(controlled_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top journal (NJS >2) articles (yearly sum")

# p_top_prop10_full
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
controlled_pp10_full_did_plot <- ggdid(controlled_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top 10% cited papers in field (yearly sum)")

robustness_check_performance_control <- ggarrange(controlled_p_full_did_plot, 
          controlled_ncs_full_did_plot,
          controlled_njs_full_did_plot, 
          controlled_njs_topjournals_did_plot,
          controlled_pp10_full_did_plot,
          labels = "AUTO", common.legend = T, legend = "bottom", ncol=2,nrow=3) %>% 
  annotate_figure(top = text_grob("Difference-in-difference results using a sample matched on prior performance"))

ggexport(robustness_check_performance_control, filename = "plots/S6_robustness_performancecontrol.pdf")

## DID with only leiden ranked institutions
onlyleiden_alt_data_matched_dataset <- alt_data_matched_dataset %>% 
  filter(origin_leiden_ranked == 1)

onlyleiden_alt_data_matched_dataset %>% distinct(pair_id) %>% dim() #1792

leidenonly_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~ 1,  
                                     data = onlyleiden_alt_data_matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

leidenonly_did_model_pfull_dynamic_short <- aggte(leidenonly_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_pfull_dynamic_short)
leidenonly_p_full_did_plot <- ggdid(leidenonly_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of publications (yearly sum)")

## normalised citation score
#year mean
leidenonly_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                                 gname = "moving_year_plus1",
                                                 idname = "cluster_id",
                                                 tname = "career_year_plus_1",
                                                 xformla = ~1,
                                                 data = onlyleiden_alt_data_matched_dataset,
                                                 est_method = "dr",
                                                 control_group = "nevertreated",
                                                 anticipation = 1,
                                                 allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

leidenonly_did_model_ncs_full_dynamic_short <- aggte(leidenonly_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_ncs_full_dynamic_short)
leidenonly_ncs_full_did_plot <- ggdid(leidenonly_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised citation score (yearly mean)")

# normalised journal score full
leidenonly_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1,
                                       data = onlyleiden_alt_data_matched_dataset,
                                       est_method = "dr",
                                       control_group = "nevertreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

leidenonly_did_model_njs_full_dynamic_short <- aggte(leidenonly_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_njs_full_dynamic_short)
leidenonly_njs_full_did_plot <- ggdid(leidenonly_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised journal score (yearly mean)")


# normalised journal score - number of "top journals" (njs >2 ) per year
leidenonly_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                               gname = "moving_year_plus1",
                                               idname = "cluster_id",
                                               tname = "career_year_plus_1",
                                               xformla = ~1,
                                               data = onlyleiden_alt_data_matched_dataset,
                                               est_method = "dr",
                                               control_group = "nevertreated",
                                               anticipation = 1,
                                               allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

leidenonly_did_model_njs_topjournals_dynamic_short <- aggte(leidenonly_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_njs_topjournals_dynamic_short)
leidenonly_njs_topjournals_did_plot <- ggdid(leidenonly_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top journal (NJS >2) articles (yearly sum")

# p_top_prop10_full
leidenonly_did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                                                gname = "moving_year_plus1",
                                                idname = "cluster_id",
                                                tname = "career_year_plus_1",
                                                xformla = ~1,
                                                data = onlyleiden_alt_data_matched_dataset,
                                                est_method = "dr",
                                                control_group = "notyettreated",
                                                anticipation = 1,
                                                allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

leidenonly_did_model_p_top_prop10_full_dynamic_short <- aggte(leidenonly_did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(leidenonly_did_model_p_top_prop10_full_dynamic_short)
leidenonly_pp10_full_did_plot <- ggdid(leidenonly_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top 10% cited papers in field (yearly sum)")

robustness_check_leiden_only <- ggarrange(leidenonly_p_full_did_plot, 
          leidenonly_ncs_full_did_plot,
          leidenonly_njs_full_did_plot, 
          leidenonly_njs_topjournals_did_plot,
          leidenonly_pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3)%>% 
  annotate_figure(top = text_grob("Difference-in-difference results with an only-leiden ranked origin institute sample"))

ggexport(robustness_check_leiden_only, filename = "plots/S7_robustness_leiden_only.pdf")

####################################################################################
################## DID compared against those who had not moved yet ################
########
# here the years before move, are compared to people that move at least a year later than them. So it's compared against people in same career year
notyettreated_dataset <- alt_data_matched_dataset %>% 
  filter(condition_numeric == 1)

notyettreated_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~ 1,  
                                     data = notyettreated_dataset,
                                     est_method = "dr",
                                     control_group = "notyettreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

notyettreated_did_model_pfull_dynamic_short <- aggte(notyettreated_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_pfull_dynamic_short)
notyettreated_p_full_did_plot <- ggdid(notyettreated_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of publications (yearly sum)")

## normalised citation score
#year mean
notyettreated_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                                 gname = "moving_year_plus1",
                                                 idname = "cluster_id",
                                                 tname = "career_year_plus_1",
                                                 xformla = ~1,
                                                 data = notyettreated_dataset,
                                                 est_method = "dr",
                                                 control_group = "notyettreated",
                                                 anticipation = 1,
                                                 allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

notyettreated_did_model_ncs_full_dynamic_short <- aggte(notyettreated_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_ncs_full_dynamic_short)
notyettreated_ncs_full_did_plot <- ggdid(notyettreated_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised citation score (yearly mean)")

# normalised journal score full
notyettreated_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1,
                                       data = notyettreated_dataset,
                                       est_method = "dr",
                                       control_group = "notyettreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

notyettreated_did_model_njs_full_dynamic_short <- aggte(notyettreated_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_njs_full_dynamic_short)
notyettreated_njs_full_did_plot <- ggdid(notyettreated_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised journal score (yearly mean)")


# normalised journal score - number of "top journals" (njs >2 ) per year
notyettreated_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                               gname = "moving_year_plus1",
                                               idname = "cluster_id",
                                               tname = "career_year_plus_1",
                                               xformla = ~1,
                                               data = notyettreated_dataset,
                                               est_method = "dr",
                                               control_group = "notyettreated",
                                               anticipation = 1,
                                               allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

notyettreated_did_model_njs_topjournals_dynamic_short <- aggte(notyettreated_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_njs_topjournals_dynamic_short)
notyettreated_njs_topjournals_did_plot <- ggdid(notyettreated_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top journal (NJS >2) articles (yearly sum")

# p_top_prop10_full
notyettreated_did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                                                gname = "moving_year_plus1",
                                                idname = "cluster_id",
                                                tname = "career_year_plus_1",
                                                xformla = ~1,
                                                data = notyettreated_dataset,
                                                est_method = "dr",
                                                control_group = "notyettreated",
                                                anticipation = 1,
                                                allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

notyettreated_did_model_p_top_prop10_full_dynamic_short <- aggte(notyettreated_did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(notyettreated_did_model_p_top_prop10_full_dynamic_short)
notyettreated_pp10_full_did_plot <- ggdid(notyettreated_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top 10% cited papers in field (yearly sum)")

robustness_check_notyettreated<- ggarrange(notyettreated_p_full_did_plot, 
          notyettreated_ncs_full_did_plot,
          notyettreated_njs_full_did_plot, 
          notyettreated_njs_topjournals_did_plot,
          notyettreated_pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3)%>% 
  annotate_figure(top = text_grob("Difference-in-difference results with control group as those that have not yet moved"))

ggexport(robustness_check_notyettreated, filename = "plots/S8_robustness_notyettreated.pdf")

#### DID with fractionalised measures
# publications per year fractionalised

did_model_pfrac <- att_gt(yname = "p_frac_yearsum",
                          gname = "moving_year_plus1",
                          idname = "cluster_id",
                          tname = "career_year_plus_1",
                          xformla = ~1,
                          data = alt_data_matched_dataset,
                          est_method = "dr",
                          control_group = "nevertreated",
                          anticipation = 1,
                          allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_pfrac_dynamic_short <- aggte(did_model_pfrac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_pfrac_dynamic_short)
p_frac_did_plot <- ggdid(did_model_pfrac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Fractionised number of publications (yearly sum)")


# ncs year mean fractionalised

did_model_ncs_frac_yearmean <- att_gt(yname = "ncs_frac_mean",
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
did_model_ncs_frac_dynamic_short <- aggte(did_model_ncs_frac_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_ncs_frac_dynamic_short)
ncs_frac_did_plot <- ggdid(did_model_ncs_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Fracionalised field-normalised citation score (yearly mean)")

# normalised journal score frac
did_model_njs_frac<- att_gt(yname = "njs_frac_mean",
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

did_model_njs_frac_dynamic_short <- aggte(did_model_njs_frac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_frac_dynamic_short)
njs_frac_did_plot <- ggdid(did_model_njs_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Fractionalised field-normalised journal score (yearly mean)")

# fractionalised normalised journal score - number of "top journals" (njs >2 ) per year
did_model_njs_topjournals_frac <- att_gt(yname = "njs_full_over2_frac_yearsum",
                                                  gname = "moving_year_plus1",
                                                  idname = "cluster_id",
                                                  tname = "career_year_plus_1",
                                                  xformla = ~1,
                                                  data = alt_data_matched_dataset,
                                                  est_method = "dr",
                                                  control_group = "notyettreated",
                                                  anticipation = 1,
                                                  allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

did_model_njs_topjournals_frac_dynamic_short <- aggte(did_model_njs_topjournals_frac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_njs_topjournals_frac_dynamic_short)
njs_topjournals_frac_did_plot <- ggdid(did_model_njs_topjournals_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top journal (NJS >2) articles (yearly sum")


# p_top_prop10_frac
did_model_p_top_prop10_frac<- att_gt(yname = "p_top_prop10_frac_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1,
                                     data = alt_data_matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_p_top_prop10_frac_dynamic_short <- aggte(did_model_p_top_prop10_frac, type = "dynamic", min_e = -5, max_e = 2)
summary(did_model_p_top_prop10_frac_dynamic_short)
pp10_frac_did_plot <- ggdid(did_model_p_top_prop10_frac_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Fractionalised number of top 10% cited papers in field (yearly sum)")

robustness_check_fractionalised <- ggarrange(p_frac_did_plot, 
          ncs_frac_did_plot,
          njs_frac_did_plot, 
          njs_topjournals_frac_did_plot,
          pp10_frac_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3) %>% 
  annotate_figure(top = text_grob("Difference-in-difference results using fractional publication measures"))

ggexport(robustness_check_fractionalised, filename = "plots/S9_robustness_fractionalised.pdf")


#######################
## only control participants that also published on the "moving" year

pair_ids_to_keep <- alt_data_matched_dataset %>% 
  filter(condition_numeric == 0,
         career_year == moving_year,
         p_full_yearsum > 0) %>% 
  distinct(pair_id)

published_in_first_year_dataset <- 
  pair_ids_to_keep %>% left_join(alt_data_matched_dataset, by = "pair_id")

published_in_first_year_dataset %>% distinct(pair_id) %>% dim()

firstyear_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                          gname = "moving_year_plus1",
                          idname = "cluster_id",
                          tname = "career_year_plus_1",
                          xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                          data = published_in_first_year_dataset,
                          est_method = "dr",
                          control_group = "nevertreated",
                          anticipation = 1,
                          allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

firstyear_did_model_pfull_dynamic_short <- aggte(firstyear_did_model_pfull, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_pfull_dynamic_short)
firstyear_p_full_did_plot <- ggdid(firstyear_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of publications (yearly sum)")


## normalised citation score
#year mean
firstyear_did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1,
                                      data = published_in_first_year_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

firstyear_did_model_ncs_full_dynamic_short <- aggte(firstyear_did_model_ncs_full_yearmean, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_ncs_full_dynamic_short)
firstyear_ncs_full_did_plot <- ggdid(firstyear_did_model_ncs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised citation score (yearly mean)")

# normalised journal score full
firstyear_did_model_njs_full<- att_gt(yname = "njs_full_mean",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~1,
                            data = published_in_first_year_dataset,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

firstyear_did_model_njs_full_dynamic_short <- aggte(firstyear_did_model_njs_full, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_njs_full_dynamic_short)
firstyear_njs_full_did_plot <- ggdid(firstyear_did_model_njs_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised journal score (yearly mean)")


# normalised journal score - number of "top journals" (njs >2 ) per year
firstyear_did_model_njs_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1,
                                    data = published_in_first_year_dataset,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
#will give an error about missing data (because people don't always publish in a given year...)

firstyear_did_model_njs_topjournals_dynamic_short <- aggte(firstyear_did_model_njs_topjournals, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_njs_topjournals_dynamic_short)
firstyear_njs_topjournals_did_plot <- ggdid(firstyear_did_model_njs_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top journal (NJS >2) articles (yearly sum")

# p_top_prop10_full
firstyear_did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1,
                                     data = published_in_first_year_dataset,
                                     est_method = "dr",
                                     control_group = "notyettreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

firstyear_did_model_p_top_prop10_full_dynamic_short <- aggte(firstyear_did_model_p_top_prop10_full, type = "dynamic", min_e = -5, max_e = 2)
summary(firstyear_did_model_p_top_prop10_full_dynamic_short)
firstyear_pp10_full_did_plot <- ggdid(firstyear_did_model_p_top_prop10_full_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Number of top 10% cited papers in field (yearly sum)")

# making a panel plot (full count only)
robustness_check_firstyear <- ggarrange(firstyear_p_full_did_plot, 
          firstyear_ncs_full_did_plot,
          firstyear_njs_full_did_plot, 
          firstyear_njs_topjournals_did_plot,
          firstyear_pp10_full_did_plot,
          common.legend = T, legend = "bottom", ncol=2,nrow=3) %>% 
  annotate_figure(top = text_grob("Difference-in-difference results using controls who had a publication on the moving year"))

ggexport(robustness_check_firstyear, filename = "plots/S10_robustness_firstyear.pdf")

#############
###### DID showing how much the effect is dependent on career year of move ##########

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

robustness_check_careeryears <- ggarrange(p_full_did_plot_group, 
          ncs_full_did_plot_group,
          njs_full_did_plot_group, 
          njs_topjournals_did_plot_group,
          pp10_full_did_plot_group,
          common.legend = T, legend = "bottom", ncol=2,nrow=3) %>% 
  annotate_figure(top = text_grob("Difference-in-difference ATTs depending on career year at move"))

ggexport(robustness_check_careeryears, filename = "plots/S11_robustness_careeryears.pdf")

##################################
## moderation robustness checks ##
##################################
##################################################
### assessing moderation by university ranking ###
##################################################
library(lmerTest)
#taking only movers and interacting 2 variables: "moving" with "difference in rank from origin to usa"
RC_pfull_qs <- lmer(p_full_yearsum ~ difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_pfull_qs); plot_model(RC_pfull_qs, type = "int")
RC_pfull_pptop10 <- lmer(p_full_yearsum ~ difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_pfull_pptop10); plot_model(RC_pfull_pptop10, type = "int", show.values = T)

RC_pfull_qs_moderation_plot <- interplot(m = RC_pfull_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  #xlab("Difference in QS Ranking ventile based on PPtop10% \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.3,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_pfull_pptop10_moderation_plot <- interplot(m = RC_pfull_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  #xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(0.3,2), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

## ncs ##

RC_ncs_full_qs <- lmer(ncs_full_mean ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_ncs_full_qs); plot_model(RC_ncs_full_qs, type = "int")
RC_ncs_full_pptop10 <- lmer(ncs_full_mean ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_ncs_full_pptop10); plot_model(RC_ncs_full_pptop10, type = "int", show.values = T)

RC_ncs_full_qs_moderation_plot <- interplot(m = RC_ncs_full_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  #xlab("Difference in QS Ranking ventile \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.3, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_ncs_full_pptop10_moderation_plot <- interplot(m = RC_ncs_full_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  #xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-1.3, 2.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_njs_full_qs <-  lmer(njs_full_mean ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_njs_full_qs); plot_model(RC_njs_full_qs, type = "int")
RC_njs_full_pptop10 <-  lmer(njs_full_mean ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_njs_full_pptop10); plot_model(RC_njs_full_pptop10, type = "int")

RC_njs_full_qs_moderation_plot <- interplot(m = RC_njs_full_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  #xlab("Difference in QS Ranking ventile \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated Coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7),expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_njs_full_pptop10_moderation_plot <- interplot(m = RC_njs_full_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  #xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated Coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits =c(-0.8, 1.7), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_njs_topjournals_qs <-  lmer(njs_full_over2_yearsum ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_njs_topjournals_qs); plot_model(RC_njs_topjournals_qs, type = "int")
RC_njs_topjournals_pptop10 <-  lmer(njs_full_over2_yearsum ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_njs_topjournals_pptop10); plot_model(RC_njs_topjournals_pptop10, type = "int")

RC_njs_topjournals_qs_moderation_plot <- interplot(m = RC_njs_topjournals_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  #xlab("Difference in QS Ranking ventile \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated Coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_njs_topjournals_pptop10_moderation_plot <- interplot(m = RC_njs_topjournals_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  #xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated Coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limits = c(-0.4,1.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_p_top_prop10_full_qs <-  lmer(p_top_prop10_full_yearsum ~difference_in_qs_overall_ranking_quantile:post_move + post_move +career_year + origin_qs_overall_score_mean + (1|cluster_id), data = diffindiff_data_only_movers_qs_diff); summary(RC_p_top_prop10_full_qs); plot_model(RC_p_top_prop10_full_qs, type = "int")
RC_p_top_prop10_full_pptop10 <-lmer(p_top_prop10_full_yearsum ~difference_in_pptop10_quantile:post_move + post_move + career_year + origin_pp_top10_mean + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff); summary(RC_p_top_prop10_full_pptop10); plot_model(RC_p_top_prop10_full_pptop10, type = "int")

RC_p_top_prop10_full_qs_moderation_plot <- interplot(m = RC_p_top_prop10_full_qs, var1 = "post_move", var2 = "difference_in_qs_overall_ranking_quantile") + 
  #xlab("Difference in QS Ranking ventile based on PPtop10% \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated Coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_p_top_prop10_full_pptop10_moderation_plot <- interplot(m = RC_p_top_prop10_full_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile") + 
  #xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(positive = higher USA ranked than origin)") +
  #ylab("Estimated Coefficient for\nmoving to USA") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_qs_difference_plot <- ggplot(diffindiff_data_only_movers_qs_diff %>% distinct(cluster_id, .keep_all = T), aes(x=difference_in_qs_overall_ranking_quantile)) +
  geom_histogram() + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,190), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_leiden_difference_plot <- ggplot(diffindiff_data_only_movers_leiden_diff %>% distinct(cluster_id, .keep_all = T), aes(x = difference_in_pptop10_quantile)) +
  geom_histogram() +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0.2,0,0.1), "cm")) + # ("top", "right", "bottom", "left")
  scale_y_continuous(limit = c(0,190), expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0))

RC_interaction_plots_left <- ggarrange(RC_pfull_qs_moderation_plot,
                                    RC_ncs_full_qs_moderation_plot, 
                                    RC_njs_full_qs_moderation_plot,
                                    RC_njs_topjournals_qs_moderation_plot, 
                                    RC_p_top_prop10_full_qs_moderation_plot,
                                    RC_qs_difference_plot,
                                    common.legend = T, legend = "bottom", ncol=1,nrow=6, labels = "AUTO",hjust = -3, vjust = 2,align = "v") 

RC_interaction_plots_right <- ggarrange(RC_pfull_pptop10_moderation_plot,
                                        RC_ncs_full_pptop10_moderation_plot,
                                        RC_njs_full_pptop10_moderation_plot, 
                                        RC_njs_topjournals_pptop10_moderation_plot,
                                        RC_p_top_prop10_full_pptop10_moderation_plot,
                                        RC_leiden_difference_plot,
                                     common.legend = T, legend = "bottom", ncol=1,nrow=6, align = "v") 

RC_moderation_plot_grid <- ggarrange(RC_interaction_plots_left, RC_interaction_plots_right) %>% 
  annotate_figure(left = text_grob("Estimated Coefficient for moving to USA",rot = 90, size = 10),
                  bottom = text_grob("Difference in ranking (left: QS, right: Leiden) quantile (positive = USA higher ranked)", size = 10))
ggexport(RC_moderation_plot_grid, filename = "plots/S12_robustness_moderation.pdf")


##################################################
### assessing mediation by university ranking ###
##################################################
# Running did model in regression, and assessing difference in effect when adding "difference in rank from origin to usa"

#### QS RANKING ######
# p full
detach("package:lmerTest", unload = T)
RC_pfull_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
RC_pfull_qs_out.fit <- lmer(p_full_yearsum ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

RC_pfull_qs_med.out <- mediate(RC_pfull_qs_med.fit, RC_pfull_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_pubs <- summary(RC_pfull_qs_med.out)

# citation score
RC_ncs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(ncs_full_mean))
RC_ncs_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles + (1|cluster_id), data = RC_ncs_qs_mediation_data)
RC_ncs_qs_out.fit <- lmer(ncs_full_mean ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = RC_ncs_qs_mediation_data)

RC_ncs_qs_med.out <- mediate(RC_ncs_qs_out.fit, RC_ncs_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_ncs <- summary(RC_ncs_qs_med.out)

# journal score
RC_njs_qs_mediation_data <- diffindiff_data_only_movers_qs_diff %>% filter(!is.na(njs_full_mean))
RC_njs_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = RC_njs_qs_mediation_data)
RC_njs_qs_out.fit <- lmer(njs_full_mean ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = RC_njs_qs_mediation_data)

RC_njs_qs_med.out <- mediate(RC_njs_qs_med.fit, RC_njs_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_njs <- summary(RC_njs_qs_med.out)

# top journals
RC_topjoural_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
RC_topjournal_qs_out.fit <- lmer(njs_full_over2_yearsum ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

RC_topjournal_qs_med.out <- mediate(topjoural_qs_med.fit, topjournal_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_topjournals <- summary(RC_topjournal_qs_med.out)

# top 10%
RC_top10_qs_med.fit <- lmer(difference_in_qs_quantile_zeropremove ~ post_move + career_year + origin_qs_overall_rank_quartiles+ (1|cluster_id), data = diffindiff_data_only_movers_qs_diff)
RC_top10_qs_out.fit <- lmer(p_top_prop10_full_yearsum ~ difference_in_qs_quantile_zeropremove + post_move + career_year + origin_qs_overall_rank_quartiles +(1|cluster_id), data = diffindiff_data_only_movers_qs_diff)

RC_top10_qs_med.out <- mediate(RC_top10_qs_med.fit, top10_qs_out.fit, treat = "post_move", mediator = "difference_in_qs_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_qs_pptop10 <- summary(RC_top10_qs_med.out)

##### LEIDEN RANKING ######

# p full
detach("package:lmerTest", unload = T)
RC_pfull_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
RC_pfull_out.fit <- lmer(p_full_yearsum ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

RC_pfull_med.out <- mediate(RC_pfull_med.fit, RC_pfull_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = T, sims = 100)
RC_med_leiden_pubs <- summary(RC_pfull_med.out)

# citation score
RC_ncs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(ncs_full_mean))
RC_ncs_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), da = RC_ncs_mediation_data)
RC_ncs_out.fit <- lmer(ncs_full_mean ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year +origin_pp_top10_mean_quantile + (1|cluster_id), data = RC_ncs_mediation_data)

RC_ncs_med.out <- mediate(RC_ncs_med.fit, RC_ncs_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_ncs <- summary(RC_ncs_med.out)

# journal score
RC_njs_mediation_data <- diffindiff_data_only_movers_leiden_diff %>% filter(!is.na(njs_full_mean))
RC_njs_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = RC_njs_mediation_data)
RC_njs_out.fit <- lmer(njs_full_mean ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = RC_njs_mediation_data)

RC_njs_med.out <- mediate(RC_njs_med.fit, RC_njs_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_njs <- summary(RC_njs_med.out)

# top journals
RC_topjoural_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
RC_topjournal_out.fit <- lmer(njs_full_over2_yearsum ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

RC_topjournal_med.out <- mediate(RC_topjoural_med.fit, RC_topjournal_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_topjournals <- summary(RC_topjournal_med.out)

# top 10%
RC_top10_med.fit <- lmer(difference_in_pptop10_quantile_zeropremove ~ post_move + career_year + origin_pp_top10_mean_quantile + (1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)
RC_top10_out.fit <- lmer(p_top_prop10_full_yearsum ~ difference_in_pptop10_quantile_zeropremove + post_move + career_year + origin_pp_top10_mean_quantile  +(1|cluster_id), data = diffindiff_data_only_movers_leiden_diff)

RC_top10_med.out <- mediate(RC_top10_med.fit, RC_top10_out.fit, treat = "post_move", mediator = "difference_in_pptop10_quantile_zeropremove", robustSE = F, sims = 100)
RC_med_leiden_pptop10 <- summary(RC_top10_med.out)

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

##### subsamples with people moving to different levels of prestige
qs_low <- diffindiff_data_only_movers_qs_diff %>% filter(gelman_origin_qs_overall_score_mean < mean(gelman_origin_qs_overall_score_mean, na.rm=T)-sd(gelman_origin_qs_overall_score_mean, na.rm=T)) %>% distinct(pair_id) %>% left_join(alt_data_matched_dataset, by = "pair_id")
qs_high <- diffindiff_data_only_movers_qs_diff %>% filter(gelman_origin_qs_overall_score_mean > mean(gelman_origin_qs_overall_score_mean, na.rm=T)+sd(gelman_origin_qs_overall_score_mean, na.rm=T)) %>% distinct(pair_id) %>% left_join(alt_data_matched_dataset, by = "pair_id")
  
leiden_low <- diffindiff_data_only_movers_leiden_diff %>% filter(gelman_origin_pp_top10_mean < mean(gelman_origin_pp_top10_mean, na.rm=T)-sd(gelman_origin_pp_top10_mean, na.rm=T)) %>% distinct(pair_id) %>% left_join(alt_data_matched_dataset, by = "pair_id")
leiden_high <- diffindiff_data_only_movers_leiden_diff %>% filter(gelman_origin_pp_top10_mean > mean(gelman_origin_pp_top10_mean, na.rm=T)+sd(gelman_origin_pp_top10_mean, na.rm=T)) %>% distinct(pair_id) %>% left_join(alt_data_matched_dataset, by = "pair_id")


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

RC_QS_LOW_did_model_pfull_dynamic_short <- aggte(RC_QS_LOW_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
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

RC_QS_HIGH_did_model_pfull_dynamic_short <- aggte(RC_QS_HIGH_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_LOW_did_model_pfull_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_HIGH_did_model_pfull_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(RC_LEIDEN_HIGH_did_model_pfull_dynamic_short)
RC_LEIDEN_HIGH_p_full_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# ncs #

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

RC_QS_LOW_did_model_ncs_dynamic_short <- aggte(RC_QS_LOW_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
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

RC_QS_HIGH_did_model_ncs_dynamic_short <- aggte(RC_QS_HIGH_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_LOW_did_model_ncs_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_HIGH_did_model_ncs_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(RC_LEIDEN_HIGH_did_model_ncs_dynamic_short)
RC_LEIDEN_HIGH_ncs_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# njs #

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

RC_QS_LOW_did_model_njs_dynamic_short <- aggte(RC_QS_LOW_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
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

RC_QS_HIGH_did_model_njs_dynamic_short <- aggte(RC_QS_HIGH_did_model_ncj, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_LOW_did_model_njs_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_HIGH_did_model_njs_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(RC_LEIDEN_HIGH_did_model_njs_dynamic_short)
RC_LEIDEN_HIGH_njs_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# top journals #

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

RC_QS_LOW_did_model_topjournals_dynamic_short <- aggte(RC_QS_LOW_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
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

RC_QS_HIGH_did_model_topjournals_dynamic_short <- aggte(RC_QS_HIGH_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_LOW_did_model_topjournals_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short)
RC_LEIDEN_HIGH_topjournals_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

# top 10%
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

RC_QS_LOW_did_model_topcited_dynamic_short <- aggte(RC_QS_LOW_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
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

RC_QS_HIGH_did_model_topcited_dynamic_short <- aggte(RC_QS_HIGH_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_LOW_did_model_topcited_dynamic_short <- aggte(RC_LEIDEN_LOW_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
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

RC_LEIDEN_HIGH_did_model_topcited_dynamic_short <- aggte(RC_LEIDEN_HIGH_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(RC_LEIDEN_HIGH_did_model_topcited_dynamic_short)
RC_LEIDEN_HIGH_topcited_did_plot <- ggdid(RC_LEIDEN_HIGH_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

low_high_rankings_table <- c("", "publications","", "ncs", "", "njs", "", "top journals", "", "top cited", "") %>% 
  rbind(c("QS low ATT", round(c(RC_QS_LOW_did_model_pfull_dynamic_short$overall.att, RC_QS_LOW_did_model_pfull_dynamic_short$overall.se, RC_QS_LOW_did_model_ncs_dynamic_short$overall.att, RC_QS_LOW_did_model_ncs_dynamic_short$overall.se, RC_QS_LOW_did_model_njs_dynamic_short$overall.att,RC_QS_LOW_did_model_njs_dynamic_short$overall.se, RC_QS_LOW_did_model_topjournals_dynamic_short$overall.att, RC_QS_LOW_did_model_topjournals_dynamic_short$overall.se, RC_QS_LOW_did_model_topcited_dynamic_short$overall.att, RC_QS_LOW_did_model_topcited_dynamic_short$overall.se),2))) %>% 
  rbind(c("QS high ATT", round(c(RC_QS_HIGH_did_model_pfull_dynamic_short$overall.att, RC_QS_HIGH_did_model_pfull_dynamic_short$overall.se, RC_QS_HIGH_did_model_ncs_dynamic_short$overall.att, RC_QS_HIGH_did_model_ncs_dynamic_short$overall.se, RC_QS_HIGH_did_model_njs_dynamic_short$overall.att,RC_QS_HIGH_did_model_njs_dynamic_short$overall.se, RC_QS_HIGH_did_model_topjournals_dynamic_short$overall.att, RC_QS_HIGH_did_model_topjournals_dynamic_short$overall.se, RC_QS_HIGH_did_model_topcited_dynamic_short$overall.att, RC_QS_HIGH_did_model_topcited_dynamic_short$overall.se),2))) %>% 
  rbind(c("Leiden low ATT", round(c(RC_LEIDEN_LOW_did_model_pfull_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_pfull_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_ncs_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_ncs_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_njs_dynamic_short$overall.att,RC_LEIDEN_LOW_did_model_njs_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_topjournals_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_topjournals_dynamic_short$overall.se, RC_LEIDEN_LOW_did_model_topcited_dynamic_short$overall.att, RC_LEIDEN_LOW_did_model_topcited_dynamic_short$overall.se),2))) %>% 
  rbind(c("Leiden high ATT" , round(c(RC_LEIDEN_HIGH_did_model_pfull_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_pfull_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_ncs_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_ncs_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_njs_dynamic_short$overall.att,RC_LEIDEN_HIGH_did_model_njs_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_topjournals_dynamic_short$overall.se, RC_LEIDEN_HIGH_did_model_topcited_dynamic_short$overall.att, RC_LEIDEN_HIGH_did_model_topcited_dynamic_short$overall.se),2)))

write.csv(low_high_rankings_table, "tables/S8. low_vs_high_ranking_ATTs.csv")
