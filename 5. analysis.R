source("4. matching.R") #importing data and packages

pacman::p_load(sjPlot, cowplot)

#setting up data for diff-in-diff
matched_dataset <- cluster_id_and_pair_id %>% 
  left_join(researcher_performance_years, by = "cluster_id") %>% 
  left_join(researcher_basic_info %>% select(cluster_id, origin_country,origin_region, discipline, specialty,end_of_career_year, gender, difference_in_qs_overall_score,difference_in_qs_reputation_score,difference_in_qs_overall_ranking_quantile, origin_qs_overall_score_mean,origin_qs_reputation_score_mean,origin_qs_overall_rank_quartiles,USAinstitute_qs_overall_score_mean,USAinstitute_qs_reputation_score_mean,USAinstitute_qs_overall_rank_quartiles,origin_pp_top10_mean_quantile,USAinstitute_pp_top10_mean_quantile, difference_in_pptop10,difference_in_pptop10_quantile), by = "cluster_id") %>% 
  select(cluster_id, pair_id, moving_year, condition, discipline, specialty, career_year,end_of_career_year, everything()) %>% 
  mutate(years_from_obtaining_usa_affilation = career_year-moving_year,
         post_move = if_else(years_from_obtaining_usa_affilation >= 0, 1, 0),
         late_mover = if_else(career_year >= 6, 1, 0), #dummy variable if mover moves later than the 3rd quartile of movers (i haven't checked this since I changed from months to years)
         career_over = if_else(career_year > end_of_career_year, 1, 0),
         condition_numeric = if_else(condition == "movers", 1, 0),
         woman = if_else(gender == "F", 1, 0),
         current_qs_ranking = if_else(post_move == 1 & condition_numeric == 1, USAinstitute_qs_overall_rank_quartiles, origin_qs_overall_rank_quartiles),
         current_leiden_ranking = if_else(post_move == 1 & condition_numeric == 1, USAinstitute_pp_top10_mean_quantile, origin_pp_top10_mean_quantile))

diffindiff_data <- matched_dataset %>% 
  filter(years_from_obtaining_usa_affilation >= -2,
         years_from_obtaining_usa_affilation <= 2)

diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% count(discipline) #do this to see how many matches per discipline
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% count(origin_country) %>% print(n=100) #do this to see how many matches per discipline
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% count(origin_region) %>% print(n=100) #do this to see how many matches per discipline


## running difference in difference with lm
test_p_full <- lm(p_full_yearsum ~condition*post_move + career_year , data = diffindiff_data); summary(test_p_full); plot_model(test_p_full, type = "int")
test_p_frac <- lm(p_frac_yearsum ~condition*post_move + career_year, data = diffindiff_data); summary(test_p_frac); plot_model(test_p_frac, type = "int")
test_cs_frac <- lm(cs_full_yearsum ~condition*post_move + career_year, data = diffindiff_data); summary(test_cs_frac); plot_model(test_cs_frac, type = "int")
test_ncs_frac <- lm(ncs_frac_yearsum ~condition*post_move + career_year, data = diffindiff_data); summary(test_ncs_frac); plot_model(test_ncs_frac, type = "int")
test_p_top_prop10_frac <- lm(p_top_prop10_full_yearsum ~condition*post_move + career_year, data = diffindiff_data); summary(test_p_top_prop10_frac); plot_model(test_p_top_prop10_frac, type = "int")
test_njs_full <- lm(njs_full_mean ~condition*post_move + career_year, data = diffindiff_data); summary(test_njs_frac); plot_model(test_njs_full, type = "int")
test_njs_frac <- lm(njs_frac_mean ~condition*post_move + career_year, data = diffindiff_data); summary(test_njs_frac); plot_model(test_njs_frac, type = "int")

# running did with fixest (see https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#323_Staggered_difference-in-differences_(Sun_and_Abraham,_2020))
pacman::p_load(fixest)
hist(diffindiff_data$p_full_yearsum,40)

diffindiff_data_fixest <- diffindiff_data %>% 
  mutate(moving_year = if_else(condition == "stayers", 10000, moving_year),
         years_from_obtaining_usa_affilation = if_else(condition == "stayers", -1000, years_from_obtaining_usa_affilation)) # to run this version, we need to change the "year_treated" and the "time_to_treatment" for the control group

fixest_poison = fepois(p_full_yearsum ~ i(years_from_obtaining_usa_affilation, condition_numeric, ref = c(-1, -1000)) + woman:i(years_from_obtaining_usa_affilation, condition_numeric, ref = c(-1, -1000)) | discipline + cluster_id, diffindiff_data_fixest)
summary(fixest_poison, se = "twoway")
iplot(fixest_poison, xlab = 'Years until move',  ref.line = -1, se = "twoway")

fixest_poison_sa20 = fepois(p_full_yearsum ~ woman*sunab(moving_year, career_year)|  cluster_id + discipline, diffindiff_data_fixest)
summary(fixest_poison_sa20, se = "twoway")
iplot(fixest_poison_sa20, se = "twoway", xlab = 'Years until move',  ref.line = -1)
summary(fixest_poison_sa20, agg = "att") #how to get the average treatment effect for the treated (ATT)


iplot(list(fixest_poison, fixest_poison_sa20), sep = 0.5,  se = "twoway") #note that the x axis now has the title career_year (because it is calculating the difference between )
legend("topleft", col = c(1, 2), pch = c(20, 15), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

summary(fixest_poison_sa20, agg = "att") #how to get the average treatment effect for the treated (ATT)

## running diff-in-diff with the did package (https://bcallaway11.github.io/did/index.html)
pacman::p_load(did, readstata13, devtools)
devtools::install_github("bcallaway11/did")

alt_data_matched_dataset <- matched_dataset %>% 
  filter(career_over == 0) %>% 
  mutate(moving_year_plus1 = moving_year+1, #to add interpretability I added one here. this means that the year that someone starts their career is year 1 instead of year 0
         career_year_plus_1 = career_year+1,
         moving_year_plus1 = if_else(condition == "stayers", 0, moving_year_plus1),
         difference_in_pptop10_quantile_with_zeros = if_else(condition_numeric == 0, 0, difference_in_pptop10_quantile))

alt_data_matched_dataset_mini <- alt_data_matched_dataset %>% 
  filter(!is.na(difference_in_pptop10_quantile_with_zeros))

did_model_pfull <- att_gt(yname = "p_full_yearsum",
              gname = "moving_year_plus1",
              idname = "cluster_id",
              tname = "career_year_plus_1",
              xformla = ~woman,
              data = alt_data_matched_dataset,
              est_method = "dr",
              control_group = "nevertreated",
              clustervars = "cluster_id",
              anticipation = 1,
              allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_pfull_dynamic <- aggte(did_model_pfull, type = "dynamic")
summary(did_model_pfull_dynamic)
tidy(did_model_pfull_dynamic)
p_full_did_plot <- ggdid(did_model_pfull_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Full publications (sum)")
group_effects <- aggte(did_model_pfull, type = "group")
summary(group_effects)
ggdid(group_effects)

did_model_pfull_dynamic_short <- aggte(did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_pfull_dynamic_short)
ggdid(did_model_pfull_dynamic_short)

# fractionalised

did_model_pfrac <- att_gt(yname = "p_frac_yearsum",
              gname = "moving_year_plus1",
              idname = "cluster_id",
              tname = "career_year_plus_1",
              xformla = ~woman,
              data = alt_data_matched_dataset,
              est_method = "dr",
              control_group = "nevertreated",
              anticipation = 1,
              allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_pfrac_dynamic <- aggte(did_model_pfrac, type = "dynamic")
summary(did_model_pfrac_dynamic)
p_frac_did_plot <- ggdid(did_model_pfrac_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Fractional publications (sum)")
did_model_pfrac_groupwise <- aggte(did_model_pfrac, type = "group")
summary(did_model_pfrac_groupwise)
ggdid(did_model_pfrac_groupwise)

did_model_pfrac_dynamic_short <- aggte(did_model_pfrac, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_pfrac_dynamic_short)
ggdid(did_model_pfrac_dynamic_short)

## normalised citation score
#year mean
did_model_ncs_full_yearmean <- att_gt(yname = "ncs_full_mean",
                                gname = "moving_year_plus1",
                                idname = "cluster_id",
                                tname = "career_year_plus_1",
                                xformla = ~woman,
                                data = alt_data_matched_dataset,
                                est_method = "dr",
                                control_group = "nevertreated",
                                anticipation = 1,
                                allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

did_model_ncs_full_yearmean_yearmean_dynamic <- aggte(did_model_ncs_full_yearmean, type = "dynamic")
summary(did_model_ncs_full_yearmean_yearmean_dynamic)
ncs_full_did_plot <- ggdid(did_model_ncs_full_yearmean_yearmean_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised citation score (mean)")
did_model_ncs_full_yearmean_groupwise <- aggte(did_model_ncs_full_yearmean, type = "group")
summary(did_model_ncs_full_yearmean_groupwise)
ggdid(did_model_ncs_full_yearmean_groupwise)

did_model_ncs_full_dynamic_short <- aggte(did_model_ncs_full_yearmean, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_ncs_full_dynamic_short)
ggdid(did_model_ncs_full_dynamic_short)

# year mean fractionalised

did_model_ncs_frac_yearmean <- att_gt(yname = "ncs_frac_mean",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~woman,
                                 data = alt_data_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)

ggdid(did_model_ncs_frac_yearmean)
did_model_ncs_frac_yearmean_dynamic <- aggte(did_model_ncs_frac_yearmean, type = "dynamic")
summary(did_model_ncs_frac_yearmean_dynamic)
ncs_frac_did_plot <- ggdid(did_model_ncs_frac_yearmean_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Fracionalised field-normalised citation score (mean)")
did_model_ncs_frac_yearmean_groupwise <- aggte(did_model_ncs_frac_yearmean, type = "group")
summary(did_model_ncs_frac_yearmean_groupwise)
ggdid(did_model_ncs_frac_yearmean_groupwise)

did_model_ncs_frac_dynamic_short <- aggte(did_model_ncs_frac_yearmean, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_ncs_dynamic_short)
ggdid(did_model_ncs_dynamic_short)

# normalised journal score full
did_model_njs_full<- att_gt(yname = "njs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~woman,
                                      data = alt_data_matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
did_model_njs_full_dynamic <- aggte(did_model_njs_full, type = "dynamic")
summary(did_model_njs_full_dynamic)
njs_full_did_plot <- ggdid(did_model_njs_full_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Field-normalised journal score (mean)")

did_model_njs_full_dynamic_short <- aggte(did_model_njs_full, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_njs_full_dynamic_short)
ggdid(did_model_njs_full_dynamic_short)

# normalised journal score frac
did_model_njs_frac<- att_gt(yname = "njs_frac_mean",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~woman,
                            data = alt_data_matched_dataset,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
did_model_njs_frac_dynamic <- aggte(did_model_njs_frac, type = "dynamic")
summary(did_model_njs_frac_dynamic)
njs_frac_did_plot <- ggdid(did_model_njs_frac_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Fractionalised field-normalised journal score (mean)")

did_model_njs_frac_dynamic_short <- aggte(did_model_njs_frac, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_njs_frac_dynamic_short)
ggdid(did_model_njs_frac_dynamic_short)

# p_top_prop10_full
did_model_p_top_prop10_full<- att_gt(yname = "p_top_prop10_full_yearsum",
                            gname = "moving_year_plus1",
                            idname = "cluster_id",
                            tname = "career_year_plus_1",
                            xformla = ~woman,
                            data = alt_data_matched_dataset,
                            est_method = "dr",
                            control_group = "nevertreated",
                            anticipation = 1,
                            allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
did_model_p_top_prop10_full_dynamic <- aggte(did_model_p_top_prop10_full, type = "dynamic")
summary(did_model_p_top_prop10_full_dynamic)
pp10_full_did_plot <- ggdid(did_model_p_top_prop10_full_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Top 10% cited papers in field (mean)")

did_model_p_top_prop10_full_dynamic_short <- aggte(did_model_p_top_prop10_full, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_p_top_prop10_full_dynamic_short)
ggdid(did_model_p_top_prop10_full_dynamic_short)

# p_top_prop10_frac
did_model_p_top_prop10_frac<- att_gt(yname = "p_top_prop10_frac_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~woman,
                                     data = alt_data_matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) #i think this should account for the fact that some people have quit science (and we therefore don't have full data for them)
did_model_p_top_prop10_frac_dynamic <- aggte(did_model_p_top_prop10_frac, type = "dynamic")
summary(did_model_p_top_prop10_frac_dynamic)
pp10_frac_did_plot <- ggdid(did_model_p_top_prop10_frac_dynamic, xlab = "Years from move", ylab = "Treatment effect", title = "Fractionalised top 10% cited papers in field (mean)")

did_model_p_top_prop10_frac_dynamic_short <- aggte(did_model_p_top_prop10_frac, type = "dynamic", min_e = -2, max_e = 2)
summary(did_model_p_top_prop10_frac_dynamic_short)
ggdid(did_model_p_top_prop10_frac_dynamic_short)

ggarrange(p_full_did_plot, p_frac_did_plot, 
  ncs_full_did_plot, ncs_frac_did_plot,
  njs_full_did_plot, njs_frac_did_plot,
  pp10_full_did_plot, pp10_frac_did_plot,
  common.legend = T, legend = "bottom", ncol=2, nrow=4)

ATTs <- c(did_model_pfull_dynamic_short$overall.att,
          did_model_pfrac_dynamic_short$overall.att,
          did_model_ncs_full_dynamic_short$overall.att,
          did_model_ncs_frac_dynamic_short$overall.att,
          did_model_njs_full_dynamic_short$overall.att,
          did_model_njs_frac_dynamic_short$overall.att,
          did_model_p_top_prop10_full_dynamic_short$overall.att,
          did_model_p_top_prop10_frac_dynamic_short$overall.att)

alt_data_matched_dataset %>% #calculating relative increase
  filter(years_from_obtaining_usa_affilation >= 0 & years_from_obtaining_usa_affilation <= 2,
         condition_numeric == 0) %>% 
  summarise(mean_p_full = mean(p_full_yearsum),
            mean_p_frac = mean(p_frac_yearsum),
            mean_ncs_full = mean(ncs_full_mean, na.rm=T),
            mean_ncs_frac = mean(ncs_full_mean, na.rm=T),
            mean_njs_full = mean(njs_full_mean, na.rm=T),
            mean_njs_frac = mean(njs_full_mean, na.rm=T),
            mean_p_top_prop10_full = mean(p_top_prop10_full_yearsum),
            mean_p_top_prop10_frac = mean(p_top_prop10_frac_yearsum)) %>% 
  pivot_longer(everything(),names_to = "bibliometric_measure", values_to = "mean") %>% 
  mutate(ATT = ATTs,
         percentage_increase = (ATT/mean)*100)
##################################################
### assessing moderation by university ranking ###
##################################################
pacman::p_load(lmerTest, ggpubr)

diffindiff_data_only_movers <- diffindiff_data %>% filter(condition_numeric == 1) %>% 
  mutate(factor_post_move = as.factor(post_move))

pfull_overallranking3 <- lmer(p_full_yearsum ~difference_in_qs_overall_ranking_quantile*post_move +career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(pfull_overallranking3); plot_model(pfull_overallranking3, type = "int")
pfull_pptop10 <- lmer(p_full_yearsum ~difference_in_pptop10_quantile*post_move +  career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(pfull_pptop10); plot_model(pfull_pptop10, type = "int", show.values = T)

pfull_pptop10_moderation_plot <- interplot(m = pfull_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile",hist = TRUE) + 
  xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(negative = higher USA ranked than origin)") +
  ylab("Estimated Coefficient for\nmoving to USA") +
  ggtitle("Full publications (sum)") +
  theme_bw() 

pfrac_overallranking <- lmer(p_frac_yearsum ~difference_in_qs_overall_ranking_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(pfrac_overallranking); plot_model(pfrac_overallranking, type = "int", mdrt.values = "meansd")
pfrac_pptop10 <- lmer(p_frac_yearsum ~difference_in_pptop10_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(pfrac_pptop10); plot_model(pfrac_pptop10, type = "int", show.values = T)

pfrac_pptop10_moderation_plot <- interplot(m = pfrac_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile",hist = TRUE) + 
  xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(negative = higher USA ranked than origin)") +
  ylab("Estimated Coefficient for\nmoving to USA") +
  ggtitle("Fractionalised full publications (sum)") +
  theme_bw()

ncs_full_overallranking <- lmer(ncs_full_yearsum ~difference_in_qs_overall_ranking_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(ncs_full_overallranking); plot_model(ncs_full_overallranking, type = "int")
ncs_full_pptop10 <- lmer(ncs_full_yearsum ~difference_in_pptop10_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(ncs_full_pptop10); plot_model(ncs_full_pptop10, type = "int", show.values = T)

ncs_frac_overallranking <- lmer(ncs_frac_yearsum ~difference_in_qs_overall_ranking_quantile*post_move + career_year + (1|cluster_id), data = diffindiff_data_only_movers); summary(ncs_frac_overallranking); plot_model(ncs_frac_overallranking, type = "int")
ncs_frac_pptop10 <- lmer(ncs_frac_yearsum ~difference_in_pptop10_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(ncs_frac_pptop10); plot_model(ncs_frac_pptop10, type = "int", show.values = T)

ncs_frac_pptop10_moderation_plot <- interplot(m = ncs_frac_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile",hist = TRUE) + 
  xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(negative = higher USA ranked than origin)") +
  ylab("Estimated Coefficient for\nmoving to USA") +
  ggtitle("Fractionalised field-normalised citation score (mean)") +
  theme_bw()

njs_full_overallranking <-  lmer(njs_full_mean ~difference_in_qs_overall_ranking_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(njs_full_overallranking); plot_model(njs_full_overallranking, type = "int")
njs_full_pptop10 <-  lmer(njs_full_mean ~difference_in_pptop10_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(njs_full_pptop10); plot_model(njs_full_pptop10, type = "int")

njs_frac_overallranking <-  lmer(njs_frac_mean ~difference_in_qs_overall_ranking_quantile*post_move + career_year  + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(njs_frac_overallranking); plot_model(njs_frac_overallranking, type = "int")
njs_frac_pptop10 <-  lmer(njs_frac_mean ~difference_in_pptop10_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(njs_frac_pptop10); plot_model(njs_frac_pptop10, type = "int")

p_top_prop10_full_overallranking <-  lmer(p_top_prop10_full_yearsum ~difference_in_qs_overall_ranking_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(p_top_prop10_full_overallranking); plot_model(p_top_prop10_full_overallranking, type = "int")
p_top_prop10_full_pptop10 <-lmer(p_top_prop10_full_yearsum ~difference_in_pptop10_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(p_top_prop10_full_pptop10); plot_model(p_top_prop10_full_pptop10, type = "int")

p_top_prop10_full_pptop10_moderation_plot <- interplot(m = p_top_prop10_full_pptop10, var1 = "post_move", var2 = "difference_in_pptop10_quantile",hist = TRUE) + 
  xlab("Difference in Leiden Ranking ventile based on PPtop10% \n(negative = higher USA ranked than origin)") +
  ylab("Estimated Coefficient for\nmoving to USA") +
  ggtitle("Top 10% cited papers in field (mean)") +
  theme_bw()

p_top_prop10_frac_overallranking <-  lmer(p_top_prop10_frac_yearsum ~difference_in_qs_overall_ranking_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(p_top_prop10_frac_overallranking); plot_model(p_top_prop10_frac_overallranking, type = "int")
p_top_prop10_frac_pptop10 <-lmer(p_top_prop10_frac_yearsum ~difference_in_pptop10_quantile*post_move + career_year + gender + origin_region + (1|cluster_id), data = diffindiff_data_only_movers); summary(p_top_prop10_frac_pptop10); plot_model(p_top_prop10_frac_pptop10, type = "int")

pacman::p_load(interplot) #https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html

#QS ranking tables
tab_model(pfull_overallranking3, pfrac_overallranking)
tab_model(ncs_full_overallranking,ncs_frac_overallranking)
tab_model(njs_full_overallranking,njs_frac_overallranking)
tab_model(p_top_prop10_full_overallranking,p_top_prop10_frac_overallranking)

#Leiden ranking tables
tab_model(pfull_pptop10, pfrac_pptop10)
tab_model(ncs_full_pptop10, ncs_frac_pptop10)
tab_model(njs_full_pptop10, njs_frac_pptop10)
tab_model(p_top_prop10_full_pptop10, p_top_prop10_frac_pptop10)

ggarrange(pfull_pptop10_moderation_plot,
          pfrac_pptop10_moderation_plot,
          ncs_frac_pptop10_moderation_plot,
          p_top_prop10_full_pptop10_moderation_plot)

