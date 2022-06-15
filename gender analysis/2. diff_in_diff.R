#importing data and packages
pacman::p_load(did, dplyr, ggplot2, ggpubr, tidyr, readr,stringr, scales, bibtex)
load("matched_dataset.RData") #for those downloading the code, you should use load("data_to_be_shared.RData") instead 
set.seed(5030)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) #function i got from: https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r?rq=1

female_matched_dataset <- matched_dataset %>% filter(gender == "F"); female_matched_dataset %>% group_by(condition) %>% summarise(distinct = n_distinct(cluster_id)) 
male_matched_dataset <- matched_dataset %>% filter(gender == "M"); male_matched_dataset %>% group_by(condition) %>% summarise(distinct = n_distinct(cluster_id))

#### Step 1. DO WOMEN AND MEN PERFORM DIFFERENTLY AT BASELINE? #####
data_for_premove_ttest <- matched_dataset %>% filter(years_from_obtaining_usa_affilation < 0 & years_from_obtaining_usa_affilation >= -2) %>% select(gender, p_full_yearsum, p_frac_yearsum, ncs_full_mean, ncs_frac_mean, njs_full_mean, njs_frac_mean, p_top_prop10_full_yearsum, p_top_prop10_frac_yearsum, njs_full_over2_yearsum, njs_full_over2_frac_yearsum)

d_p_full_yearsum <- effectsize::cohens_d(p_full_yearsum ~ gender, pooled_sd =F, data = data_for_premove_ttest)
d_ncs_full_mean <- effectsize::cohens_d(ncs_full_mean ~ gender, pooled_sd =F, data = data_for_premove_ttest)
d_njs_full_mean <- effectsize::cohens_d(njs_full_mean ~ gender, pooled_sd =F, data = data_for_premove_ttest)
d_njs_full_over2_yearsum <- effectsize::cohens_d(njs_full_over2_yearsum ~ gender, pooled_sd =F, data = data_for_premove_ttest)
d_p_top_prop10_full_yearsum <- effectsize::cohens_d(p_top_prop10_full_yearsum ~ gender, pooled_sd =F, data = data_for_premove_ttest)

premove_differences <- tidy(t.test(p_full_yearsum ~ gender, var.equal =F, data = data_for_premove_ttest)) %>% 
  rbind(tidy(t.test(ncs_full_mean ~ gender, var.equal =F, data = data_for_premove_ttest))) %>% 
  rbind(tidy(t.test(njs_full_mean ~ gender, var.equal =F, data = data_for_premove_ttest))) %>% 
  rbind(tidy(t.test(njs_full_over2_yearsum ~ gender, var.equal =F, data = data_for_premove_ttest))) %>% 
  rbind(tidy(t.test(p_top_prop10_full_yearsum ~ gender, var.equal =F, data = data_for_premove_ttest))) %>% 
  mutate(performance_measure = c("Pulblications","Citation Score", "Journal Score", "Top Journal Papers", "Top Cited Papers")) %>% 
  select(performance_measure, everything()) %>% 
  mutate(cohens_d = c(d_p_full_yearsum$Cohens_d, d_ncs_full_mean$Cohens_d, d_njs_full_mean$Cohens_d, d_njs_full_over2_yearsum$Cohens_d, d_p_top_prop10_full_yearsum$Cohens_d),
         cohens_d_ci_low = c(d_p_full_yearsum$CI_low, d_ncs_full_mean$CI_low, d_njs_full_mean$CI_low, d_njs_full_over2_yearsum$CI_low, d_p_top_prop10_full_yearsum$CI_low),
         cohens_d_ci_high = c(d_p_full_yearsum$CI_high, d_ncs_full_mean$CI_high, d_njs_full_mean$CI_high, d_njs_full_over2_yearsum$CI_high, d_p_top_prop10_full_yearsum$CI_high)) %>% 
  mutate(p.value = specify_decimal(p.value, 3),
         across(where(is.double), specify_decimal, 2)) %>% 
  select(performance_measure, estimated_difference = estimate, women_mean = estimate1, men_mean = estimate2, t_statistic = statistic,df = parameter, p.value, conf.low, conf.high, cohens_d, cohens_d_ci_low,cohens_d_ci_high)

write.csv(premove_differences, "tables/premove_differences_ttests.csv")


#### Step 2. RUNNING GENDER STRATIFIED DID ####

##### WOMEN  #####
# Publications
female_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                 data = female_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) 

female_did_model_pfull_dynamic_short <- aggte(female_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(female_did_model_pfull_dynamic_short)
female_p_full_did_plot <- ggdid(female_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
female_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                               gname = "moving_year_plus1",
                               idname = "cluster_id",
                               tname = "career_year_plus_1",
                               xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                               data = female_matched_dataset,
                               est_method = "dr",
                               control_group = "nevertreated",
                               anticipation = 1,
                               allow_unbalanced_panel = T) 

female_did_model_ncs_dynamic_short <- aggte(female_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(female_did_model_ncs_dynamic_short)
female_ncs_did_plot <- ggdid(female_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

female_did_model_njs <- att_gt(yname = "njs_full_mean",
                               gname = "moving_year_plus1",
                               idname = "cluster_id",
                               tname = "career_year_plus_1",
                               xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                               data = female_matched_dataset,
                               est_method = "dr",
                               control_group = "nevertreated",
                               anticipation = 1,
                               allow_unbalanced_panel = T) 

female_did_model_njs_dynamic_short <- aggte(female_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(female_did_model_njs_dynamic_short)
female_njs_did_plot <- ggdid(female_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
female_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                       data = female_matched_dataset,
                                       est_method = "dr",
                                       control_group = "nevertreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T) 

female_did_model_topjournals_dynamic_short <- aggte(female_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(female_did_model_topjournals_dynamic_short)
female_topjournals_did_plot <- ggdid(female_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
female_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = female_matched_dataset,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) 

female_did_model_topcited_dynamic_short <- aggte(female_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(female_did_model_topcited_dynamic_short)
female_topcited_did_plot <- ggdid(female_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

##### MEN ######

# Publications
male_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                               gname = "moving_year_plus1",
                               idname = "cluster_id",
                               tname = "career_year_plus_1",
                               xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                               data = male_matched_dataset,
                               est_method = "dr",
                               control_group = "nevertreated",
                               anticipation = 1,
                               allow_unbalanced_panel = T) 

male_did_model_pfull_dynamic_short <- aggte(male_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(male_did_model_pfull_dynamic_short)
male_p_full_did_plot <- ggdid(male_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
male_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                             gname = "moving_year_plus1",
                             idname = "cluster_id",
                             tname = "career_year_plus_1",
                             xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                             data = male_matched_dataset,
                             est_method = "dr",
                             control_group = "nevertreated",
                             anticipation = 1,
                             allow_unbalanced_panel = T) 

male_did_model_ncs_dynamic_short <- aggte(male_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(male_did_model_ncs_dynamic_short)
male_ncs_did_plot <- ggdid(male_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

male_did_model_njs <- att_gt(yname = "njs_full_mean",
                             gname = "moving_year_plus1",
                             idname = "cluster_id",
                             tname = "career_year_plus_1",
                             xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                             data = male_matched_dataset,
                             est_method = "dr",
                             control_group = "nevertreated",
                             anticipation = 1,
                             allow_unbalanced_panel = T) 

male_did_model_njs_dynamic_short <- aggte(male_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(male_did_model_njs_dynamic_short)
male_njs_did_plot <- ggdid(male_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
male_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                     data = male_matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) 

male_did_model_topjournals_dynamic_short <- aggte(male_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(male_did_model_topjournals_dynamic_short)
male_topjournals_did_plot <- ggdid(male_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
male_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                  gname = "moving_year_plus1",
                                  idname = "cluster_id",
                                  tname = "career_year_plus_1",
                                  xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                  data = male_matched_dataset,
                                  est_method = "dr",
                                  control_group = "nevertreated",
                                  anticipation = 1,
                                  allow_unbalanced_panel = T) 

male_did_model_topcited_dynamic_short <- aggte(male_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(male_did_model_topcited_dynamic_short)
male_topcited_did_plot <- ggdid(male_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#------- making a forest plot -------- #
gender_ATTs <-  c("Publications", "Female", female_did_model_pfull_dynamic_short$overall.att, female_did_model_pfull_dynamic_short$overall.att-1.96*female_did_model_pfull_dynamic_short$overall.se,female_did_model_pfull_dynamic_short$overall.att+1.96*female_did_model_pfull_dynamic_short$overall.se) %>% 
  rbind(c("Publications", "Male", male_did_model_pfull_dynamic_short$overall.att, male_did_model_pfull_dynamic_short$overall.att-1.96*male_did_model_pfull_dynamic_short$overall.se,male_did_model_pfull_dynamic_short$overall.att+1.96*male_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Female", female_did_model_ncs_dynamic_short$overall.att, female_did_model_ncs_dynamic_short$overall.att-1.96*female_did_model_ncs_dynamic_short$overall.se,female_did_model_ncs_dynamic_short$overall.att+1.96*female_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Male", male_did_model_ncs_dynamic_short$overall.att, male_did_model_ncs_dynamic_short$overall.att-1.96*male_did_model_ncs_dynamic_short$overall.se,male_did_model_ncs_dynamic_short$overall.att+1.96*male_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Female", female_did_model_njs_dynamic_short$overall.att, female_did_model_njs_dynamic_short$overall.att-1.96*female_did_model_njs_dynamic_short$overall.se,female_did_model_njs_dynamic_short$overall.att+1.96*female_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Male", male_did_model_njs_dynamic_short$overall.att, male_did_model_njs_dynamic_short$overall.att-1.96*male_did_model_njs_dynamic_short$overall.se,male_did_model_njs_dynamic_short$overall.att+1.96*male_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Female", female_did_model_topjournals_dynamic_short$overall.att, female_did_model_topjournals_dynamic_short$overall.att-1.96*female_did_model_topjournals_dynamic_short$overall.se,female_did_model_topjournals_dynamic_short$overall.att+1.96*female_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Male", male_did_model_topjournals_dynamic_short$overall.att, male_did_model_topjournals_dynamic_short$overall.att-1.96*male_did_model_topjournals_dynamic_short$overall.se,male_did_model_topjournals_dynamic_short$overall.att+1.96*male_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Female", female_did_model_topcited_dynamic_short$overall.att, female_did_model_topcited_dynamic_short$overall.att-1.96*female_did_model_topcited_dynamic_short$overall.se,female_did_model_topcited_dynamic_short$overall.att+1.96*female_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Male", male_did_model_topcited_dynamic_short$overall.att, male_did_model_topcited_dynamic_short$overall.att-1.96*male_did_model_topcited_dynamic_short$overall.se,male_did_model_topcited_dynamic_short$overall.att+1.96*male_did_model_topcited_dynamic_short$overall.se)) %>% 
  as_tibble()

names(gender_ATTs) <- c("Measure","Gender","Estimate", "95% CI low", "95% CI high")

gender_ATTs <- gender_ATTs %>% 
  mutate(across("Estimate":"95% CI high", as.numeric),
         Measure = factor(Measure, levels = rev(c("Publications", "Citation Score", "Journal Score", "Top Journals", "Top Cited"))),
         Gender = factor(Gender, levels = rev(c("Female", "Male"))))%>% 
  ungroup() %>% 
  mutate(across(where(is.double), round, 2))

write.csv(gender_ATTs, "tables/DID_results.csv")

dotCOLS = c("#a6d8f0","#f9b282")# define colours for dots and bars
barCOLS = c("#008fd5","#de6b35")


p2 <- ggplot(gender_ATTs, aes(x=Measure, y=Estimate, ymin=`95% CI low`, ymax=`95% CI high`,col=Gender,fill=Gender)) + 
  #specify position here
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Performance measure") +
  scale_y_continuous(name="Increase in performance (ATT)") +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = T))
p2

ggsave(p2, filename = "plots/DID_gender.pdf", device = "pdf", width =6, height = 5)

gender_ATTs %>% group_by(Measure) %>% mutate(percetage_diff = 1-(Estimate/lead(Estimate))) ; mean(c(0.24,0.22, 0.23))

#------- assessing differences between groups -------#

# m1, m2: the sample means # code from: #https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df),df)    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value","df")
  return(dat) 
}

publications <- t.test2(female_did_model_pfull_dynamic_short$overall.att, male_did_model_pfull_dynamic_short$overall.att, (female_did_model_pfull_dynamic_short$overall.se*sqrt(female_did_model_pfull_dynamic_short$DIDparams$n)), (male_did_model_pfull_dynamic_short$overall.se*sqrt(male_did_model_pfull_dynamic_short$DIDparams$n)), female_did_model_pfull_dynamic_short$DIDparams$n, male_did_model_pfull_dynamic_short$DIDparams$n)
citation_score <- t.test2(female_did_model_ncs_dynamic_short$overall.att, male_did_model_ncs_dynamic_short$overall.att, (female_did_model_ncs_dynamic_short$overall.se*sqrt(female_did_model_ncs_dynamic_short$DIDparams$n)), (male_did_model_ncs_dynamic_short$overall.se*sqrt(male_did_model_ncs_dynamic_short$DIDparams$n)), female_did_model_ncs_dynamic_short$DIDparams$n, male_did_model_ncs_dynamic_short$DIDparams$n)
journal_score <- t.test2(female_did_model_njs_dynamic_short$overall.att, male_did_model_njs_dynamic_short$overall.att, (female_did_model_njs_dynamic_short$overall.se*sqrt(female_did_model_njs_dynamic_short$DIDparams$n)), (male_did_model_njs_dynamic_short$overall.se*sqrt(male_did_model_njs_dynamic_short$DIDparams$n)), female_did_model_njs_dynamic_short$DIDparams$n, male_did_model_njs_dynamic_short$DIDparams$n)
top_journals <- t.test2(female_did_model_topjournals_dynamic_short$overall.att, male_did_model_topjournals_dynamic_short$overall.att, (female_did_model_topjournals_dynamic_short$overall.se*sqrt(female_did_model_topjournals_dynamic_short$DIDparams$n)), (male_did_model_topjournals_dynamic_short$overall.se*sqrt(male_did_model_topjournals_dynamic_short$DIDparams$n)), female_did_model_topjournals_dynamic_short$DIDparams$n, male_did_model_topjournals_dynamic_short$DIDparams$n)
top_cited <- t.test2(female_did_model_topcited_dynamic_short$overall.att, male_did_model_topcited_dynamic_short$overall.att, (female_did_model_topcited_dynamic_short$overall.se*sqrt(female_did_model_topcited_dynamic_short$DIDparams$n)), (male_did_model_topcited_dynamic_short$overall.se*sqrt(male_did_model_topcited_dynamic_short$DIDparams$n)), female_did_model_topcited_dynamic_short$DIDparams$n, male_did_model_topcited_dynamic_short$DIDparams$n)

effectsize::t_to_d(publications[3] ,df = publications[5])
effectsize::t_to_d(citation_score[3] ,df = citation_score[5])
effectsize::t_to_d(journal_score[3] ,df = journal_score[5])
effectsize::t_to_d(top_journals[3] ,df = top_journals[5])
effectsize::t_to_d(top_cited[3] ,df = top_cited[5])


####################################### ROBUSTNESS CHECKS ##########################################
#### ROBUSTNESS CHECK 1. USING TWO MOST POPULAR DISCIPLINES #####

matched_dataset %>% group_by(gender) %>% count(discipline) #biomedical research + clinical medicine

###### BIOMEDICINE ######
biomed_female <- female_matched_dataset %>% filter(discipline == "Biomedical Research")
biomed_male <- male_matched_dataset %>% filter(discipline == "Biomedical Research")

##### WOMEN  #####
# Publications
biomed_female_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                        gname = "moving_year_plus1",
                                        idname = "cluster_id",
                                        tname = "career_year_plus_1",
                                        xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                        data = biomed_female,
                                        est_method = "dr",
                                        control_group = "nevertreated",
                                        anticipation = 1,
                                        allow_unbalanced_panel = T) 

biomed_female_did_model_pfull_dynamic_short <- aggte(biomed_female_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_female_did_model_pfull_dynamic_short)
biomed_female_p_full_did_plot <- ggdid(biomed_female_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
biomed_female_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = biomed_female,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) 

biomed_female_did_model_ncs_dynamic_short <- aggte(biomed_female_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_female_did_model_ncs_dynamic_short)
biomed_female_ncs_did_plot <- ggdid(biomed_female_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

biomed_female_did_model_njs <- att_gt(yname = "njs_full_mean",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = biomed_female,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) 

biomed_female_did_model_njs_dynamic_short <- aggte(biomed_female_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_female_did_model_njs_dynamic_short)
biomed_female_njs_did_plot <- ggdid(biomed_female_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
biomed_female_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                              gname = "moving_year_plus1",
                                              idname = "cluster_id",
                                              tname = "career_year_plus_1",
                                              xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                              data = biomed_female,
                                              est_method = "dr",
                                              control_group = "nevertreated",
                                              anticipation = 1,
                                              allow_unbalanced_panel = T) 

biomed_female_did_model_topjournals_dynamic_short <- aggte(biomed_female_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_female_did_model_topjournals_dynamic_short)
biomed_female_topjournals_did_plot <- ggdid(biomed_female_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
biomed_female_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                           gname = "moving_year_plus1",
                                           idname = "cluster_id",
                                           tname = "career_year_plus_1",
                                           xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                           data = biomed_female,
                                           est_method = "dr",
                                           control_group = "nevertreated",
                                           anticipation = 1,
                                           allow_unbalanced_panel = T) 

biomed_female_did_model_topcited_dynamic_short <- aggte(biomed_female_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_female_did_model_topcited_dynamic_short)
biomed_female_topcited_did_plot <- ggdid(biomed_female_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

##### MEN ######

# Publications
biomed_male_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = biomed_male,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) 

biomed_male_did_model_pfull_dynamic_short <- aggte(biomed_male_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_male_did_model_pfull_dynamic_short)
biomed_male_p_full_did_plot <- ggdid(biomed_male_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
biomed_male_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = biomed_male,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) 

biomed_male_did_model_ncs_dynamic_short <- aggte(biomed_male_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_male_did_model_ncs_dynamic_short)
biomed_male_ncs_did_plot <- ggdid(biomed_male_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

biomed_male_did_model_njs <- att_gt(yname = "njs_full_mean",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = biomed_male,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) 

biomed_male_did_model_njs_dynamic_short <- aggte(biomed_male_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_male_did_model_njs_dynamic_short)
biomed_male_njs_did_plot <- ggdid(biomed_male_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
biomed_male_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                            gname = "moving_year_plus1",
                                            idname = "cluster_id",
                                            tname = "career_year_plus_1",
                                            xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                            data = biomed_male,
                                            est_method = "dr",
                                            control_group = "nevertreated",
                                            anticipation = 1,
                                            allow_unbalanced_panel = T) 

biomed_male_did_model_topjournals_dynamic_short <- aggte(biomed_male_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_male_did_model_topjournals_dynamic_short)
biomed_male_topjournals_did_plot <- ggdid(biomed_male_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
biomed_male_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = biomed_male,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) 

biomed_male_did_model_topcited_dynamic_short <- aggte(biomed_male_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(biomed_male_did_model_topcited_dynamic_short)
biomed_male_topcited_did_plot <- ggdid(biomed_male_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

###### Clinical Medicine ######
clinicalmed_female <- female_matched_dataset %>% filter(discipline == "Clinical Medicine")
clinicalmed_male <- male_matched_dataset %>% filter(discipline == "Clinical Medicine")

##### WOMEN  #####
# Publications
clinicalmed_female_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                             gname = "moving_year_plus1",
                                             idname = "cluster_id",
                                             tname = "career_year_plus_1",
                                             xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                             data = clinicalmed_female,
                                             est_method = "dr",
                                             control_group = "nevertreated",
                                             anticipation = 1,
                                             allow_unbalanced_panel = T) 

clinicalmed_female_did_model_pfull_dynamic_short <- aggte(clinicalmed_female_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_female_did_model_pfull_dynamic_short)
clinicalmed_female_p_full_did_plot <- ggdid(clinicalmed_female_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
clinicalmed_female_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                           gname = "moving_year_plus1",
                                           idname = "cluster_id",
                                           tname = "career_year_plus_1",
                                           xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                           data = clinicalmed_female,
                                           est_method = "dr",
                                           control_group = "nevertreated",
                                           anticipation = 1,
                                           allow_unbalanced_panel = T) 

clinicalmed_female_did_model_ncs_dynamic_short <- aggte(clinicalmed_female_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_female_did_model_ncs_dynamic_short)
clinicalmed_female_ncs_did_plot <- ggdid(clinicalmed_female_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

clinicalmed_female_did_model_njs <- att_gt(yname = "njs_full_mean",
                                           gname = "moving_year_plus1",
                                           idname = "cluster_id",
                                           tname = "career_year_plus_1",
                                           xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                           data = clinicalmed_female,
                                           est_method = "dr",
                                           control_group = "nevertreated",
                                           anticipation = 1,
                                           allow_unbalanced_panel = T) 

clinicalmed_female_did_model_njs_dynamic_short <- aggte(clinicalmed_female_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_female_did_model_njs_dynamic_short)
clinicalmed_female_njs_did_plot <- ggdid(clinicalmed_female_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
clinicalmed_female_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                                   gname = "moving_year_plus1",
                                                   idname = "cluster_id",
                                                   tname = "career_year_plus_1",
                                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                                   data = clinicalmed_female,
                                                   est_method = "dr",
                                                   control_group = "nevertreated",
                                                   anticipation = 1,
                                                   allow_unbalanced_panel = T) 

clinicalmed_female_did_model_topjournals_dynamic_short <- aggte(clinicalmed_female_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_female_did_model_topjournals_dynamic_short)
clinicalmed_female_topjournals_did_plot <- ggdid(clinicalmed_female_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
clinicalmed_female_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                                gname = "moving_year_plus1",
                                                idname = "cluster_id",
                                                tname = "career_year_plus_1",
                                                xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                                data = clinicalmed_female,
                                                est_method = "dr",
                                                control_group = "nevertreated",
                                                anticipation = 1,
                                                allow_unbalanced_panel = T) 

clinicalmed_female_did_model_topcited_dynamic_short <- aggte(clinicalmed_female_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_female_did_model_topcited_dynamic_short)
clinicalmed_female_topcited_did_plot <- ggdid(clinicalmed_female_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

##### MEN ######

# Publications
clinicalmed_male_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                           gname = "moving_year_plus1",
                                           idname = "cluster_id",
                                           tname = "career_year_plus_1",
                                           xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                           data = clinicalmed_male,
                                           est_method = "dr",
                                           control_group = "nevertreated",
                                           anticipation = 1,
                                           allow_unbalanced_panel = T) 

clinicalmed_male_did_model_pfull_dynamic_short <- aggte(clinicalmed_male_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_male_did_model_pfull_dynamic_short)
clinicalmed_male_p_full_did_plot <- ggdid(clinicalmed_male_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
clinicalmed_male_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = clinicalmed_male,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) 

clinicalmed_male_did_model_ncs_dynamic_short <- aggte(clinicalmed_male_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_male_did_model_ncs_dynamic_short)
clinicalmed_male_ncs_did_plot <- ggdid(clinicalmed_male_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

clinicalmed_male_did_model_njs <- att_gt(yname = "njs_full_mean",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = clinicalmed_male,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) 

clinicalmed_male_did_model_njs_dynamic_short <- aggte(clinicalmed_male_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_male_did_model_njs_dynamic_short)
clinicalmed_male_njs_did_plot <- ggdid(clinicalmed_male_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
clinicalmed_male_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                                 gname = "moving_year_plus1",
                                                 idname = "cluster_id",
                                                 tname = "career_year_plus_1",
                                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                                 data = clinicalmed_male,
                                                 est_method = "dr",
                                                 control_group = "nevertreated",
                                                 anticipation = 1,
                                                 allow_unbalanced_panel = T) 

clinicalmed_male_did_model_topjournals_dynamic_short <- aggte(clinicalmed_male_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_male_did_model_topjournals_dynamic_short)
clinicalmed_male_topjournals_did_plot <- ggdid(clinicalmed_male_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
clinicalmed_male_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                              gname = "moving_year_plus1",
                                              idname = "cluster_id",
                                              tname = "career_year_plus_1",
                                              xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                              data = clinicalmed_male,
                                              est_method = "dr",
                                              control_group = "nevertreated",
                                              anticipation = 1,
                                              allow_unbalanced_panel = T) 

clinicalmed_male_did_model_topcited_dynamic_short <- aggte(clinicalmed_male_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(clinicalmed_male_did_model_topcited_dynamic_short)
clinicalmed_male_topcited_did_plot <- ggdid(clinicalmed_male_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

####------- making a forest plot for biomedicine -------- #####
biomed_gender_ATTs <-  
  c("Publications", "Female", biomed_female_did_model_pfull_dynamic_short$overall.att, biomed_female_did_model_pfull_dynamic_short$overall.att-1.96*biomed_female_did_model_pfull_dynamic_short$overall.se,biomed_female_did_model_pfull_dynamic_short$overall.att+1.96*biomed_female_did_model_pfull_dynamic_short$overall.se) %>% 
  rbind(c("Publications", "Male", biomed_male_did_model_pfull_dynamic_short$overall.att, biomed_male_did_model_pfull_dynamic_short$overall.att-1.96*biomed_male_did_model_pfull_dynamic_short$overall.se,biomed_male_did_model_pfull_dynamic_short$overall.att+1.96*biomed_male_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Female", biomed_female_did_model_ncs_dynamic_short$overall.att, biomed_female_did_model_ncs_dynamic_short$overall.att-1.96*biomed_female_did_model_ncs_dynamic_short$overall.se,biomed_female_did_model_ncs_dynamic_short$overall.att+1.96*biomed_female_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Male", biomed_male_did_model_ncs_dynamic_short$overall.att, biomed_male_did_model_ncs_dynamic_short$overall.att-1.96*biomed_male_did_model_ncs_dynamic_short$overall.se,biomed_male_did_model_ncs_dynamic_short$overall.att+1.96*biomed_male_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Female", biomed_female_did_model_njs_dynamic_short$overall.att, biomed_female_did_model_njs_dynamic_short$overall.att-1.96*biomed_female_did_model_njs_dynamic_short$overall.se,biomed_female_did_model_njs_dynamic_short$overall.att+1.96*biomed_female_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Male", biomed_male_did_model_njs_dynamic_short$overall.att, biomed_male_did_model_njs_dynamic_short$overall.att-1.96*biomed_male_did_model_njs_dynamic_short$overall.se,biomed_male_did_model_njs_dynamic_short$overall.att+1.96*biomed_male_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Female", biomed_female_did_model_topjournals_dynamic_short$overall.att, biomed_female_did_model_topjournals_dynamic_short$overall.att-1.96*biomed_female_did_model_topjournals_dynamic_short$overall.se,biomed_female_did_model_topjournals_dynamic_short$overall.att+1.96*biomed_female_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Male", biomed_male_did_model_topjournals_dynamic_short$overall.att, biomed_male_did_model_topjournals_dynamic_short$overall.att-1.96*biomed_male_did_model_topjournals_dynamic_short$overall.se,biomed_male_did_model_topjournals_dynamic_short$overall.att+1.96*biomed_male_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Female", biomed_female_did_model_topcited_dynamic_short$overall.att, biomed_female_did_model_topcited_dynamic_short$overall.att-1.96*biomed_female_did_model_topcited_dynamic_short$overall.se,biomed_female_did_model_topcited_dynamic_short$overall.att+1.96*biomed_female_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Male", biomed_male_did_model_topcited_dynamic_short$overall.att, biomed_male_did_model_topcited_dynamic_short$overall.att-1.96*biomed_male_did_model_topcited_dynamic_short$overall.se,biomed_male_did_model_topcited_dynamic_short$overall.att+1.96*biomed_male_did_model_topcited_dynamic_short$overall.se)) %>% 
  as_tibble()

names(biomed_gender_ATTs) <- c("Measure","Gender","Estimate", "95% CI low", "95% CI high")

biomed_gender_ATTs <- biomed_gender_ATTs %>% 
  mutate(across("Estimate":"95% CI high", as.numeric),
         Measure = factor(Measure, levels = rev(c("Publications", "Citation Score", "Journal Score", "Top Journals", "Top Cited"))),
         Gender = factor(Gender, levels = rev(c("Female", "Male"))))

p3 <- ggplot(biomed_gender_ATTs, aes(x=Measure, y=Estimate, ymin=`95% CI low`, ymax=`95% CI high`,col=Gender,fill=Gender)) + 
  #specify position here
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Performance measure") +
  scale_y_continuous(name="Increase in performance (ATT)", limits = c(0, 2)) +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = T))

####------- making a forest plot for Clinical Medicine -------- #####
clinicalmedicine_gender_ATTs <-  
  c("Publications", "Female", clinicalmed_female_did_model_pfull_dynamic_short$overall.att, clinicalmed_female_did_model_pfull_dynamic_short$overall.att-1.96*clinicalmed_female_did_model_pfull_dynamic_short$overall.se,clinicalmed_female_did_model_pfull_dynamic_short$overall.att+1.96*clinicalmed_female_did_model_pfull_dynamic_short$overall.se) %>% 
  rbind(c("Publications", "Male", clinicalmed_male_did_model_pfull_dynamic_short$overall.att, clinicalmed_male_did_model_pfull_dynamic_short$overall.att-1.96*clinicalmed_male_did_model_pfull_dynamic_short$overall.se,clinicalmed_male_did_model_pfull_dynamic_short$overall.att+1.96*clinicalmed_male_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Female", clinicalmed_female_did_model_ncs_dynamic_short$overall.att, clinicalmed_female_did_model_ncs_dynamic_short$overall.att-1.96*clinicalmed_female_did_model_ncs_dynamic_short$overall.se,clinicalmed_female_did_model_ncs_dynamic_short$overall.att+1.96*clinicalmed_female_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Male", clinicalmed_male_did_model_ncs_dynamic_short$overall.att, clinicalmed_male_did_model_ncs_dynamic_short$overall.att-1.96*clinicalmed_male_did_model_ncs_dynamic_short$overall.se,clinicalmed_male_did_model_ncs_dynamic_short$overall.att+1.96*clinicalmed_male_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Female", clinicalmed_female_did_model_njs_dynamic_short$overall.att, clinicalmed_female_did_model_njs_dynamic_short$overall.att-1.96*clinicalmed_female_did_model_njs_dynamic_short$overall.se,clinicalmed_female_did_model_njs_dynamic_short$overall.att+1.96*clinicalmed_female_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Male", clinicalmed_male_did_model_njs_dynamic_short$overall.att, clinicalmed_male_did_model_njs_dynamic_short$overall.att-1.96*clinicalmed_male_did_model_njs_dynamic_short$overall.se,clinicalmed_male_did_model_njs_dynamic_short$overall.att+1.96*clinicalmed_male_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Female", clinicalmed_female_did_model_topjournals_dynamic_short$overall.att, clinicalmed_female_did_model_topjournals_dynamic_short$overall.att-1.96*clinicalmed_female_did_model_topjournals_dynamic_short$overall.se,clinicalmed_female_did_model_topjournals_dynamic_short$overall.att+1.96*clinicalmed_female_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Male", clinicalmed_male_did_model_topjournals_dynamic_short$overall.att, clinicalmed_male_did_model_topjournals_dynamic_short$overall.att-1.96*clinicalmed_male_did_model_topjournals_dynamic_short$overall.se,clinicalmed_male_did_model_topjournals_dynamic_short$overall.att+1.96*clinicalmed_male_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Female", clinicalmed_female_did_model_topcited_dynamic_short$overall.att, clinicalmed_female_did_model_topcited_dynamic_short$overall.att-1.96*clinicalmed_female_did_model_topcited_dynamic_short$overall.se,clinicalmed_female_did_model_topcited_dynamic_short$overall.att+1.96*clinicalmed_female_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Male", clinicalmed_male_did_model_topcited_dynamic_short$overall.att, clinicalmed_male_did_model_topcited_dynamic_short$overall.att-1.96*clinicalmed_male_did_model_topcited_dynamic_short$overall.se,clinicalmed_male_did_model_topcited_dynamic_short$overall.att+1.96*clinicalmed_male_did_model_topcited_dynamic_short$overall.se)) %>% 
  as_tibble()

names(clinicalmedicine_gender_ATTs) <- c("Measure","Gender","Estimate", "95% CI low", "95% CI high")

clinicalmedicine_gender_ATTs <- clinicalmedicine_gender_ATTs %>% 
  mutate(across("Estimate":"95% CI high", as.numeric),
         Measure = factor(Measure, levels = rev(c("Publications", "Citation Score", "Journal Score", "Top Journals", "Top Cited"))),
         Gender = factor(Gender, levels = rev(c("Female", "Male"))))

p4 <- ggplot(clinicalmedicine_gender_ATTs, aes(x=Measure, y=Estimate, ymin=`95% CI low`, ymax=`95% CI high`,col=Gender,fill=Gender)) + 
  #specify position here
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="") +
  scale_y_continuous(name="Increase in performance (ATT)", limits = c(0, 2)) +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = T))

p3_and_p4 <- ggarrange(p3,p4, common.legend = T, labels = c("Biomedical Research","   Clinical Medicine"),legend = "bottom", vjust = 2, hjust = -0.7)

ggsave(p3_and_p4, filename = "plots/DID_gender_clinicalmed_and_biomedical.pdf", device = "pdf", width =7, height = 5)
#### raw plots ####

dotCOLS = c("#a6d8f0","#f9b282")
barCOLS = c("#008fd5","#de6b35")
gender_raw_data_plots_means_per_year <- matched_dataset %>% 
  filter(years_from_obtaining_usa_affilation <= 2 & years_from_obtaining_usa_affilation >= -5) %>%
  group_by(gender, condition, years_from_obtaining_usa_affilation) %>%
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
  ) %>% 
  mutate(gender = if_else(gender == "M", "Male", "Female"))

gender_publications <- ggplot(gender_raw_data_plots_means_per_year, aes(x  = years_from_obtaining_usa_affilation,mean_p_full, colour = gender, linetype = condition)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_p_full-se_p_full, ymax = mean_p_full+se_p_full), width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("#f9b282","#a6d8f0")) +
  ylab("Mean (SE) # of pubs") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

gender_citationscore <- ggplot(gender_raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_ncs_full_mean, colour = gender, linetype = condition)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_ncs_full_mean-se_ncs_full_mean, ymax = mean_ncs_full_mean+se_ncs_full_mean), width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("#f9b282","#a6d8f0")) +
  ylab("Mean (SE) ncs") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

gender_journalscore <- ggplot(gender_raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_njs_full_mean, colour = gender, linetype = condition)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_njs_full_mean-se_mean_njs_full_mean, ymax = mean_njs_full_mean+se_mean_njs_full_mean),width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("#f9b282","#a6d8f0")) +
  ylab("Mean (SE) njs") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

gender_topjournals <- ggplot(gender_raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_njs_full_over2_yearsum, colour = gender, linetype = condition)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_njs_full_over2_yearsum-se_njs_full_over2_yearsum, ymax = mean_njs_full_over2_yearsum+se_njs_full_over2_yearsum),width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("#f9b282","#a6d8f0")) +
  ylab("Mean (SE) # of top journal pubs") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

gender_topcited <- ggplot(gender_raw_data_plots_means_per_year, aes( years_from_obtaining_usa_affilation,mean_p_top_prop10_full_yearsum, colour = gender, linetype = condition)) +
  geom_line() + 
  geom_errorbar(aes(ymin = mean_p_top_prop10_full_yearsum-se_mean_p_top_prop10_full_yearsum, ymax = mean_p_top_prop10_full_yearsum+se_mean_p_top_prop10_full_yearsum),width = 0.2) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values=c("#f9b282","#a6d8f0")) +
  ylab("Mean (SE) # of top cited pubs") +
  xlab("Years from move") +
  scale_x_continuous(breaks = seq(-5,2,1))

# combine the plot
gender_diff_raw_plots <- ggpubr::ggarrange(gender_publications, gender_citationscore, gender_journalscore, gender_topjournals, gender_topcited, ncol=2, nrow =3, common.legend = T)
ggsave(gender_diff_raw_plots, filename = "plots/raw_data.pdf", device = "pdf", width =7, height = 6)


####### ROBUSTNESS CHECK 2 .COMPARING MOVERS TO THEMSELVES #########


#### ----  FEMALE ---- ####
female_ATTs <- c(female_did_model_pfull_dynamic_short$overall.att,
                 female_did_model_ncs_dynamic_short$overall.att,
                 female_did_model_njs_dynamic_short$overall.att,
                 female_did_model_topjournals_dynamic_short$overall.att,
                 female_did_model_topcited_dynamic_short$overall.att)



# female_increase_percentage_comparedtocontrols <- female_matched_dataset %>% #calculating relative increase
#   filter(years_from_obtaining_usa_affilation >= 0 & years_from_obtaining_usa_affilation <= 2,
#          condition_numeric == 0) %>% 
#   summarise(mean_p_full = mean(p_full_yearsum),
#             mean_ncs_full = mean(ncs_full_mean, na.rm=T),
#             mean_njs_full = mean(njs_full_mean, na.rm=T),
#             mean_njs_topjournals = mean(njs_full_over2_yearsum, na.rm=T),
#             mean_p_top_prop10_full = mean(p_top_prop10_full_yearsum)) %>% 
#   pivot_longer(everything(),names_to = "bibliometric_measure", values_to = "mean") %>% 
#   mutate(ATT = female_ATTs,
#          percentage_increase = (ATT/mean)*100) %>% 
#   mutate(mean = round(mean,2),
#          ATT = round(ATT, 2),
#          percentage_increase = round(percentage_increase, 2))

female_increase_percentage_comparedtoself <- female_matched_dataset %>% #calculating relative increase
  filter(years_from_obtaining_usa_affilation == -1 |years_from_obtaining_usa_affilation == -2,
         condition_numeric == 1) %>% 
  summarise(mean_p_full = mean(p_full_yearsum),
            mean_ncs_full = mean(ncs_full_mean, na.rm=T),
            mean_njs_full = mean(njs_full_mean, na.rm=T),
            mean_njs_topjournals = mean(njs_full_over2_yearsum, na.rm=T),
            mean_p_top_prop10_full = mean(p_top_prop10_full_yearsum)) %>% 
  pivot_longer(everything(),names_to = "bibliometric_measure", values_to = "mean") %>% 
  mutate(ATT = female_ATTs,
         percentage_increase = (ATT/mean)*100) %>% 
  mutate(mean = round(mean,2),
         ATT = round(ATT, 2),
         percentage_increase = round(percentage_increase, 2)) %>% 
  select(bibliometric_measure, female_premove_mean = mean, female_att = ATT, female_increase = percentage_increase)

#### ----  MALE ---- ####
male_ATTs <- c(male_did_model_pfull_dynamic_short$overall.att,
               male_did_model_ncs_dynamic_short$overall.att,
               male_did_model_njs_dynamic_short$overall.att,
               male_did_model_topjournals_dynamic_short$overall.att,
               male_did_model_topcited_dynamic_short$overall.att)

# male_increase_percentage_comparedtocontrols <- male_matched_dataset %>% #calculating relative increase
#   filter(years_from_obtaining_usa_affilation >= 0 & years_from_obtaining_usa_affilation <= 2,
#          condition_numeric == 0) %>% 
#   summarise(mean_p_full = mean(p_full_yearsum),
#             mean_ncs_full = mean(ncs_full_mean, na.rm=T),
#             mean_njs_full = mean(njs_full_mean, na.rm=T),
#             mean_njs_topjournals = mean(njs_full_over2_yearsum, na.rm=T),
#             mean_p_top_prop10_full = mean(p_top_prop10_full_yearsum)) %>% 
#   pivot_longer(everything(),names_to = "bibliometric_measure", values_to = "mean") %>% 
#   mutate(ATT = male_ATTs,
#          percentage_increase = (ATT/mean)*100) %>% 
#   mutate(mean = round(mean,2),
#          ATT = round(ATT, 2),
#          percentage_increase = round(percentage_increase, 2))

male_increase_percentage_comparedtoself <- male_matched_dataset %>% #calculating relative increase
  filter(years_from_obtaining_usa_affilation == -1 |years_from_obtaining_usa_affilation == -2,
         condition_numeric == 1) %>% 
  summarise(mean_p_full = mean(p_full_yearsum),
            mean_ncs_full = mean(ncs_full_mean, na.rm=T),
            mean_njs_full = mean(njs_full_mean, na.rm=T),
            mean_njs_topjournals = mean(njs_full_over2_yearsum, na.rm=T),
            mean_p_top_prop10_full = mean(p_top_prop10_full_yearsum)) %>% 
  pivot_longer(everything(),names_to = "bibliometric_measure", values_to = "mean") %>% 
  mutate(ATT = male_ATTs,
         percentage_increase = (ATT/mean)*100) %>% 
  mutate(mean = round(mean,2),
         ATT = round(ATT, 2),
         percentage_increase = round(percentage_increase, 2)) %>% 
  select(bibliometric_measure, male_premove_mean = mean, male_att = ATT, male_increase = percentage_increase)

#gender differences compared to controls
#female_increase_percentage_comparedtocontrols$percentage_increase-male_increase_percentage_comparedtocontrols$percentage_increase

percentage_diff_table <- female_increase_percentage_comparedtoself %>% 
  left_join(male_increase_percentage_comparedtoself, by = "bibliometric_measure") %>%
  mutate(percentage_difference = female_increase-male_increase) %>% 
  mutate(across(where(is.double), specify_decimal, 2))

write.csv(percentage_diff_table, "tables/did_relative_gender_difference.csv")

mixed_effect_relativedifferences_data <- male_matched_dataset %>% #calculating relative increase
  filter(years_from_obtaining_usa_affilation == -1 |years_from_obtaining_usa_affilation == -2,
         condition_numeric == 1) %>% 
  rbind(female_matched_dataset %>% #calculating relative increase
          filter(years_from_obtaining_usa_affilation == -1 |years_from_obtaining_usa_affilation == -2,
                 condition_numeric == 1))
pacman::p_load(lme4, lmerTest)
mixed_pubs <- lmer(p_full_yearsum ~ woman*post_move + (post_move|cluster_id), matched_dataset %>% filter(years_from_obtaining_usa_affilation >=-2,
                                                                                                         years_from_obtaining_usa_affilation <= 2) %>% mutate(woman = factor(woman)))
summary(mixed_pubs);sjPlot::plot_model(mixed_pubs, type = "emm", terms = c("post_move","woman"))
mixed_citations <- lmer(ncs_full_mean ~ woman*post_move + (1|cluster_id), matched_dataset %>% filter(years_from_obtaining_usa_affilation >=-2,
                                                                                                     years_from_obtaining_usa_affilation <= 2) %>% mutate(woman = factor(woman)))
summary(mixed_citations);sjPlot::plot_model(mixed_citations, type = "emm", terms = c("post_move","woman"))

mixed_njs <- lmer(njs_full_mean ~ woman*post_move + (1|cluster_id), matched_dataset %>% filter(years_from_obtaining_usa_affilation >=-2,
                                                                                               years_from_obtaining_usa_affilation <= 2) %>% mutate(woman = factor(woman)))
summary(mixed_njs);sjPlot::plot_model(mixed_njs, type = "emm", terms = c("post_move","woman"))

mixed_topjournals <- lmer(njs_full_over2_yearsum ~ woman*post_move + (1|cluster_id), matched_dataset %>% filter(years_from_obtaining_usa_affilation >=-2,
                                                                                                                years_from_obtaining_usa_affilation <= 2) %>% mutate(woman = factor(woman)))
summary(mixed_topjournals);sjPlot::plot_model(mixed_topjournals, type = "emm", terms = c("post_move","woman"))

mixed_topcited <- lmer(p_top_prop10_full_yearsum ~ woman*post_move + (1|cluster_id), matched_dataset %>% filter(years_from_obtaining_usa_affilation >=-2,
                                                                                                                years_from_obtaining_usa_affilation <= 2) %>% mutate(woman = factor(woman)))
summary(mixed_topcited);sjPlot::plot_model(mixed_topcited, type = "emm", terms = c("post_move","woman"))





#### ROBUSTNESS CHECK 3. MATCHING WOMEN VS MEN BASED ON THOSE WHO MOVED TO THE SAME INSTITUTE ####

#first check whether there is a mean difference in leiden or QS score of male compared to female movers?
data_for_ttest <- matched_dataset %>% filter(condition_numeric == 1) %>% distinct(cluster_id, .keep_all = T) %>% select(gender, USA_qs_overall_score_mean, USA_pp_top10_mean, origin_qs_overall_score_mean, origin_pp_top10_mean) %>% mutate(diff_pp10 = USA_pp_top10_mean-origin_pp_top10_mean, diff_qs = USA_qs_overall_score_mean-origin_qs_overall_score_mean)

t.test(USA_pp_top10_mean ~ gender, var.equal =F, data = data_for_ttest) #difference in destination 
#t.test(USA_qs_overall_score_mean ~ gender, var.equal =F, data = data_for_ttest) 
effectsize::cohens_d(USA_pp_top10_mean ~ gender, var.equal =F, data = data_for_ttest)

t.test(origin_pp_top10_mean ~ gender, var.equal =F, data = data_for_ttest)  #difference in origin 
#t.test(origin_qs_overall_score_mean ~ gender, var.equal =F, data = data_for_ttest)
effectsize::cohens_d(origin_pp_top10_mean ~ gender, var.equal =F, data = data_for_ttest)

t.test(diff_pp10 ~ gender, var.equal =F, data = data_for_ttest)#difference in the difference between destination and origin 
#t.test(diff_qs ~ gender, var.equal =F, data = data_for_ttest) 
effectsize::cohens_d(diff_pp10 ~ gender, var.equal =F, data = data_for_ttest)

#then prepare the data
women <- female_matched_dataset %>% select(cluster_id, pair_id, discipline,specialty,  USA_institution, origin_institution, origin_country) %>% distinct(cluster_id, .keep_all = T) %>% filter(!is.na(USA_institution))
men <- male_matched_dataset %>% select(cluster_id, pair_id, discipline, specialty, USA_institution, origin_institution, origin_country) %>% distinct(cluster_id, .keep_all = T) %>% filter(!is.na(USA_institution))

#the following results in the cluster ids who women, who match with an individual man who moved to the same institute in the USA. matches are preferred if they are the same discipline, specialty, origin institution and origin country
result3 <- women %>% 
  left_join(men, by = c("USA_institution"), suffix = c("_women", "_men")) %>% 
  filter(!is.na(origin_institution_men)) %>% 
  group_by(cluster_id_men) %>% 
  mutate(n_matches_men = n()) %>% 
  ungroup() %>% 
  mutate(do_disciplines_match = if_else(discipline_women == discipline_men, 1, 0),
         do_specialties_match = if_else(specialty_women == specialty_men, 1, 0),
         do_origininstitutions_match = if_else(origin_institution_women == origin_institution_men, 1, 0,),
         do_origincountries_match = if_else(origin_country_women == origin_country_men, 1, 0)) %>% 
  arrange(n_matches_men,desc(do_origincountries_match), desc(do_origininstitutions_match), desc(do_disciplines_match), desc(do_specialties_match)) %>% 
  distinct(cluster_id_men, .keep_all = T) %>% 
  distinct(cluster_id_women, .keep_all = T)

new_female_matched_dataset <- result3 %>% select(pair_id = pair_id_women) %>% distinct(pair_id) %>% left_join(female_matched_dataset, by = "pair_id")
new_male_matched_dataset <- result3 %>% select(pair_id = pair_id_men) %>% distinct(pair_id) %>% left_join(male_matched_dataset, by = "pair_id")

# then doing the diff-in-diff just for the men and women that moved to the same place.

##### WOMEN  #####
# Publications
new_female_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                     gname = "moving_year_plus1",
                                     idname = "cluster_id",
                                     tname = "career_year_plus_1",
                                     xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                     data = new_female_matched_dataset,
                                     est_method = "dr",
                                     control_group = "nevertreated",
                                     anticipation = 1,
                                     allow_unbalanced_panel = T) 

new_female_did_model_pfull_dynamic_short <- aggte(new_female_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(new_female_did_model_pfull_dynamic_short)
new_female_p_full_did_plot <- ggdid(new_female_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
new_female_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                   gname = "moving_year_plus1",
                                   idname = "cluster_id",
                                   tname = "career_year_plus_1",
                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                   data = new_female_matched_dataset,
                                   est_method = "dr",
                                   control_group = "nevertreated",
                                   anticipation = 1,
                                   allow_unbalanced_panel = T) 

new_female_did_model_ncs_dynamic_short <- aggte(new_female_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(new_female_did_model_ncs_dynamic_short)
new_female_ncs_did_plot <- ggdid(new_female_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

new_female_did_model_njs <- att_gt(yname = "njs_full_mean",
                                   gname = "moving_year_plus1",
                                   idname = "cluster_id",
                                   tname = "career_year_plus_1",
                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                   data = new_female_matched_dataset,
                                   est_method = "dr",
                                   control_group = "nevertreated",
                                   anticipation = 1,
                                   allow_unbalanced_panel = T) 

new_female_did_model_njs_dynamic_short <- aggte(new_female_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(new_female_did_model_njs_dynamic_short)
new_female_njs_did_plot <- ggdid(new_female_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
new_female_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                           gname = "moving_year_plus1",
                                           idname = "cluster_id",
                                           tname = "career_year_plus_1",
                                           xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                           data = new_female_matched_dataset,
                                           est_method = "dr",
                                           control_group = "nevertreated",
                                           anticipation = 1,
                                           allow_unbalanced_panel = T) 

new_female_did_model_topjournals_dynamic_short <- aggte(new_female_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(new_female_did_model_topjournals_dynamic_short)
new_female_topjournals_did_plot <- ggdid(new_female_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
new_female_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                        gname = "moving_year_plus1",
                                        idname = "cluster_id",
                                        tname = "career_year_plus_1",
                                        xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                        data = new_female_matched_dataset,
                                        est_method = "dr",
                                        control_group = "nevertreated",
                                        anticipation = 1,
                                        allow_unbalanced_panel = T) 

new_female_did_model_topcited_dynamic_short <- aggte(new_female_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(new_female_did_model_topcited_dynamic_short)
new_female_topcited_did_plot <- ggdid(new_female_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

##### MEN ######

# Publications
new_male_did_model_pfull <- att_gt(yname = "p_full_yearsum",
                                   gname = "moving_year_plus1",
                                   idname = "cluster_id",
                                   tname = "career_year_plus_1",
                                   xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                   data = new_male_matched_dataset,
                                   est_method = "dr",
                                   control_group = "nevertreated",
                                   anticipation = 1,
                                   allow_unbalanced_panel = T) 

new_male_did_model_pfull_dynamic_short <- aggte(new_male_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(new_male_did_model_pfull_dynamic_short)
new_male_p_full_did_plot <- ggdid(new_male_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
new_male_did_model_ncs <- att_gt(yname = "ncs_full_mean",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                 data = new_male_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) 

new_male_did_model_ncs_dynamic_short <- aggte(new_male_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(new_male_did_model_ncs_dynamic_short)
new_male_ncs_did_plot <- ggdid(new_male_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

new_male_did_model_njs <- att_gt(yname = "njs_full_mean",
                                 gname = "moving_year_plus1",
                                 idname = "cluster_id",
                                 tname = "career_year_plus_1",
                                 xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                 data = new_male_matched_dataset,
                                 est_method = "dr",
                                 control_group = "nevertreated",
                                 anticipation = 1,
                                 allow_unbalanced_panel = T) 

new_male_did_model_njs_dynamic_short <- aggte(new_male_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(new_male_did_model_njs_dynamic_short)
new_male_njs_did_plot <- ggdid(new_male_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
new_male_did_model_topjournals <- att_gt(yname = "njs_full_over2_yearsum",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = new_male_matched_dataset,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) 

new_male_did_model_topjournals_dynamic_short <- aggte(new_male_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(new_male_did_model_topjournals_dynamic_short)
new_male_topjournals_did_plot <- ggdid(new_male_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
new_male_did_model_topcited <- att_gt(yname = "p_top_prop10_full_yearsum",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = new_male_matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) 

new_male_did_model_topcited_dynamic_short <- aggte(new_male_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(new_male_did_model_topcited_dynamic_short)
new_male_topcited_did_plot <- ggdid(new_male_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")


#####------- making a forest plot -------- #####
new_gender_ATTs <-  c("Publications", "Female", new_female_did_model_pfull_dynamic_short$overall.att, new_female_did_model_pfull_dynamic_short$overall.att-1.96*new_female_did_model_pfull_dynamic_short$overall.se,new_female_did_model_pfull_dynamic_short$overall.att+1.96*new_female_did_model_pfull_dynamic_short$overall.se) %>% 
  rbind(c("Publications", "Male", new_male_did_model_pfull_dynamic_short$overall.att, new_male_did_model_pfull_dynamic_short$overall.att-1.96*new_male_did_model_pfull_dynamic_short$overall.se,new_male_did_model_pfull_dynamic_short$overall.att+1.96*new_male_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Female", new_female_did_model_ncs_dynamic_short$overall.att, new_female_did_model_ncs_dynamic_short$overall.att-1.96*new_female_did_model_ncs_dynamic_short$overall.se,new_female_did_model_ncs_dynamic_short$overall.att+1.96*new_female_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Male", new_male_did_model_ncs_dynamic_short$overall.att, new_male_did_model_ncs_dynamic_short$overall.att-1.96*new_male_did_model_ncs_dynamic_short$overall.se,new_male_did_model_ncs_dynamic_short$overall.att+1.96*new_male_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Female", new_female_did_model_njs_dynamic_short$overall.att, new_female_did_model_njs_dynamic_short$overall.att-1.96*new_female_did_model_njs_dynamic_short$overall.se,new_female_did_model_njs_dynamic_short$overall.att+1.96*new_female_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Male", new_male_did_model_njs_dynamic_short$overall.att, new_male_did_model_njs_dynamic_short$overall.att-1.96*new_male_did_model_njs_dynamic_short$overall.se,new_male_did_model_njs_dynamic_short$overall.att+1.96*new_male_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Female", new_female_did_model_topjournals_dynamic_short$overall.att, new_female_did_model_topjournals_dynamic_short$overall.att-1.96*new_female_did_model_topjournals_dynamic_short$overall.se,new_female_did_model_topjournals_dynamic_short$overall.att+1.96*new_female_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Male", new_male_did_model_topjournals_dynamic_short$overall.att, new_male_did_model_topjournals_dynamic_short$overall.att-1.96*new_male_did_model_topjournals_dynamic_short$overall.se,new_male_did_model_topjournals_dynamic_short$overall.att+1.96*new_male_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Female", new_female_did_model_topcited_dynamic_short$overall.att, new_female_did_model_topcited_dynamic_short$overall.att-1.96*new_female_did_model_topcited_dynamic_short$overall.se,new_female_did_model_topcited_dynamic_short$overall.att+1.96*new_female_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Male", new_male_did_model_topcited_dynamic_short$overall.att, new_male_did_model_topcited_dynamic_short$overall.att-1.96*new_male_did_model_topcited_dynamic_short$overall.se,new_male_did_model_topcited_dynamic_short$overall.att+1.96*new_male_did_model_topcited_dynamic_short$overall.se)) %>% 
  as_tibble()

names(new_gender_ATTs) <- c("Measure","Gender","Estimate", "95% CI low", "95% CI high")

new_gender_ATTs <- new_gender_ATTs %>% 
  mutate(across("Estimate":"95% CI high", as.numeric),
         Measure = factor(Measure, levels = rev(c("Publications", "Citation Score", "Journal Score", "Top Journals", "Top Cited"))),
         Gender = factor(Gender, levels = rev(c("Female", "Male"))))



new_p2 <- ggplot(new_gender_ATTs, aes(x=Measure, y=Estimate, ymin=`95% CI low`, ymax=`95% CI high`,col=Gender,fill=Gender)) + 
  #specify position here
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Performance measure") +
  scale_y_continuous(name="Increase in performance (ATT)") +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = T))
new_p2

ggsave(new_p2, filename = "plots/DID_gender_matchedondestination.pdf", device = "pdf", width =6, height = 5)

#### ROBUSTNESS CHECK 4. CHECKING WHETHER THERE ARE SIMILAR EFFECTS IF LOOKING AT FRACTIONALISED MEASURES #####

##### WOMEN  #####
# Publications
frac_female_did_model_pfull <- att_gt(yname = "p_frac_yearsum",
                                      gname = "moving_year_plus1",
                                      idname = "cluster_id",
                                      tname = "career_year_plus_1",
                                      xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                      data = female_matched_dataset,
                                      est_method = "dr",
                                      control_group = "nevertreated",
                                      anticipation = 1,
                                      allow_unbalanced_panel = T) 

frac_female_did_model_pfull_dynamic_short <- aggte(frac_female_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_female_did_model_pfull_dynamic_short)
frac_female_p_full_did_plot <- ggdid(frac_female_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
frac_female_did_model_ncs <- att_gt(yname = "ncs_frac_mean",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = female_matched_dataset,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) 

frac_female_did_model_ncs_dynamic_short <- aggte(frac_female_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_female_did_model_ncs_dynamic_short)
frac_female_ncs_did_plot <- ggdid(frac_female_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

frac_female_did_model_njs <- att_gt(yname = "njs_frac_mean",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = female_matched_dataset,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) 

frac_female_did_model_njs_dynamic_short <- aggte(frac_female_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_female_did_model_njs_dynamic_short)
frac_female_njs_did_plot <- ggdid(frac_female_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
frac_female_did_model_topjournals <- att_gt(yname = "njs_full_over2_frac_yearsum",
                                            gname = "moving_year_plus1",
                                            idname = "cluster_id",
                                            tname = "career_year_plus_1",
                                            xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                            data = female_matched_dataset,
                                            est_method = "dr",
                                            control_group = "nevertreated",
                                            anticipation = 1,
                                            allow_unbalanced_panel = T) 

frac_female_did_model_topjournals_dynamic_short <- aggte(frac_female_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_female_did_model_topjournals_dynamic_short)
frac_female_topjournals_did_plot <- ggdid(frac_female_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
frac_female_did_model_topcited <- att_gt(yname = "p_top_prop10_frac_yearsum",
                                         gname = "moving_year_plus1",
                                         idname = "cluster_id",
                                         tname = "career_year_plus_1",
                                         xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                         data = female_matched_dataset,
                                         est_method = "dr",
                                         control_group = "nevertreated",
                                         anticipation = 1,
                                         allow_unbalanced_panel = T) 

frac_female_did_model_topcited_dynamic_short <- aggte(frac_female_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_female_did_model_topcited_dynamic_short)
frac_female_topcited_did_plot <- ggdid(frac_female_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

##### MEN ######

# Publications
frac_male_did_model_pfull <- att_gt(yname = "p_frac_yearsum",
                                    gname = "moving_year_plus1",
                                    idname = "cluster_id",
                                    tname = "career_year_plus_1",
                                    xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                    data = male_matched_dataset,
                                    est_method = "dr",
                                    control_group = "nevertreated",
                                    anticipation = 1,
                                    allow_unbalanced_panel = T) 

frac_male_did_model_pfull_dynamic_short <- aggte(frac_male_did_model_pfull, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_male_did_model_pfull_dynamic_short)
frac_male_p_full_did_plot <- ggdid(frac_male_did_model_pfull_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised citation score
frac_male_did_model_ncs <- att_gt(yname = "ncs_frac_mean",
                                  gname = "moving_year_plus1",
                                  idname = "cluster_id",
                                  tname = "career_year_plus_1",
                                  xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                  data = male_matched_dataset,
                                  est_method = "dr",
                                  control_group = "nevertreated",
                                  anticipation = 1,
                                  allow_unbalanced_panel = T) 

frac_male_did_model_ncs_dynamic_short <- aggte(frac_male_did_model_ncs, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_male_did_model_ncs_dynamic_short)
frac_male_ncs_did_plot <- ggdid(frac_male_did_model_ncs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Normalised journal score

frac_male_did_model_njs <- att_gt(yname = "njs_frac_mean",
                                  gname = "moving_year_plus1",
                                  idname = "cluster_id",
                                  tname = "career_year_plus_1",
                                  xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                  data = male_matched_dataset,
                                  est_method = "dr",
                                  control_group = "nevertreated",
                                  anticipation = 1,
                                  allow_unbalanced_panel = T) 

frac_male_did_model_njs_dynamic_short <- aggte(frac_male_did_model_njs, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_male_did_model_njs_dynamic_short)
frac_male_njs_did_plot <- ggdid(frac_male_did_model_njs_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top journal papers
frac_male_did_model_topjournals <- att_gt(yname = "njs_full_over2_frac_yearsum",
                                          gname = "moving_year_plus1",
                                          idname = "cluster_id",
                                          tname = "career_year_plus_1",
                                          xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                          data = male_matched_dataset,
                                          est_method = "dr",
                                          control_group = "nevertreated",
                                          anticipation = 1,
                                          allow_unbalanced_panel = T) 

frac_male_did_model_topjournals_dynamic_short <- aggte(frac_male_did_model_topjournals, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_male_did_model_topjournals_dynamic_short)
frac_male_topjournals_did_plot <- ggdid(frac_male_did_model_topjournals_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")

#Top cited papers
frac_male_did_model_topcited <- att_gt(yname = "p_top_prop10_frac_yearsum",
                                       gname = "moving_year_plus1",
                                       idname = "cluster_id",
                                       tname = "career_year_plus_1",
                                       xformla = ~1, #this package doesn't provide functionality to assess impact of covariates on outcomes. 
                                       data = male_matched_dataset,
                                       est_method = "dr",
                                       control_group = "nevertreated",
                                       anticipation = 1,
                                       allow_unbalanced_panel = T) 

frac_male_did_model_topcited_dynamic_short <- aggte(frac_male_did_model_topcited, type = "dynamic", min_e = -2, max_e = 2)
summary(frac_male_did_model_topcited_dynamic_short)
frac_male_topcited_did_plot <- ggdid(frac_male_did_model_topcited_dynamic_short, xlab = "Years from move", ylab = "Treatment effect", title = "Publications (yearly sum)")


#####------- making a forest plot -------- #####
frac_gender_ATTs <-  c("Publications", "Female", frac_female_did_model_pfull_dynamic_short$overall.att, frac_female_did_model_pfull_dynamic_short$overall.att-1.96*frac_female_did_model_pfull_dynamic_short$overall.se,frac_female_did_model_pfull_dynamic_short$overall.att+1.96*frac_female_did_model_pfull_dynamic_short$overall.se) %>% 
  rbind(c("Publications", "Male", frac_male_did_model_pfull_dynamic_short$overall.att, frac_male_did_model_pfull_dynamic_short$overall.att-1.96*frac_male_did_model_pfull_dynamic_short$overall.se,frac_male_did_model_pfull_dynamic_short$overall.att+1.96*frac_male_did_model_pfull_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Female", frac_female_did_model_ncs_dynamic_short$overall.att, frac_female_did_model_ncs_dynamic_short$overall.att-1.96*frac_female_did_model_ncs_dynamic_short$overall.se,frac_female_did_model_ncs_dynamic_short$overall.att+1.96*frac_female_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Citation Score", "Male", frac_male_did_model_ncs_dynamic_short$overall.att, frac_male_did_model_ncs_dynamic_short$overall.att-1.96*frac_male_did_model_ncs_dynamic_short$overall.se,frac_male_did_model_ncs_dynamic_short$overall.att+1.96*frac_male_did_model_ncs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Female", frac_female_did_model_njs_dynamic_short$overall.att, frac_female_did_model_njs_dynamic_short$overall.att-1.96*frac_female_did_model_njs_dynamic_short$overall.se,frac_female_did_model_njs_dynamic_short$overall.att+1.96*frac_female_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Journal Score", "Male", frac_male_did_model_njs_dynamic_short$overall.att, frac_male_did_model_njs_dynamic_short$overall.att-1.96*frac_male_did_model_njs_dynamic_short$overall.se,frac_male_did_model_njs_dynamic_short$overall.att+1.96*frac_male_did_model_njs_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Female", frac_female_did_model_topjournals_dynamic_short$overall.att, frac_female_did_model_topjournals_dynamic_short$overall.att-1.96*frac_female_did_model_topjournals_dynamic_short$overall.se,frac_female_did_model_topjournals_dynamic_short$overall.att+1.96*frac_female_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Journals", "Male", frac_male_did_model_topjournals_dynamic_short$overall.att, frac_male_did_model_topjournals_dynamic_short$overall.att-1.96*frac_male_did_model_topjournals_dynamic_short$overall.se,frac_male_did_model_topjournals_dynamic_short$overall.att+1.96*frac_male_did_model_topjournals_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Female", frac_female_did_model_topcited_dynamic_short$overall.att, frac_female_did_model_topcited_dynamic_short$overall.att-1.96*frac_female_did_model_topcited_dynamic_short$overall.se,frac_female_did_model_topcited_dynamic_short$overall.att+1.96*frac_female_did_model_topcited_dynamic_short$overall.se)) %>% 
  rbind(c("Top Cited", "Male", frac_male_did_model_topcited_dynamic_short$overall.att, frac_male_did_model_topcited_dynamic_short$overall.att-1.96*frac_male_did_model_topcited_dynamic_short$overall.se,frac_male_did_model_topcited_dynamic_short$overall.att+1.96*frac_male_did_model_topcited_dynamic_short$overall.se)) %>% 
  as_tibble()

names(frac_gender_ATTs) <- c("Measure","Gender","Estimate", "95% CI low", "95% CI high")

frac_gender_ATTs <- frac_gender_ATTs %>% 
  mutate(across("Estimate":"95% CI high", as.numeric),
         Measure = factor(Measure, levels = rev(c("Publications", "Citation Score", "Journal Score", "Top Journals", "Top Cited"))),
         Gender = factor(Gender, levels = rev(c("Female", "Male"))))

frac_p2 <- ggplot(frac_gender_ATTs, aes(x=Measure, y=Estimate, ymin=`95% CI low`, ymax=`95% CI high`,col=Gender,fill=Gender)) + 
  #specify position here
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Performance measure  \n (fractionalised)") +
  scale_y_continuous(name="Increase in performance (ATT)") +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = T))
frac_p2

ggsave(frac_p2, filename = "plots/DID_gender_fractionalised.pdf", device = "pdf", width =6, height = 5)

########## getting package citations ########

toBibtex(citation("did"))
write.bib(c("base","did", "dplyr", "ggplot2","ggpubr", "readr", "stringr", "scales"), 'packagereference.bib')