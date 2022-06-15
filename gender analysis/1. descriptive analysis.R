#importing data and packages
pacman::p_load(did, dplyr, ggplot2, ggpubr, tidyr, readr,stringr, scales, bibtex)
load("eligible_researchers.Rdata")
european_country_list <- read_csv("country_list.csv", col_names =T)
set.seed(5030)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) #function i got from: https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r?rq=1

#### Descriptive analysis ####
n_researchers <- final_complete_dataset %>% filter(!is.na(gender)) %>% distinct(cluster_id, .keep_all = T) %>% group_by(gender) %>% tally() %>% select(gender, n_total = n)
n_countries <- final_complete_dataset %>% filter(!is.na(gender)) %>% distinct(cluster_id, .keep_all = T) %>% group_by(origin_country) %>% tally()
percentage_gender_inferable <- 1-mean(is.na(final_complete_dataset %>% distinct(cluster_id, .keep_all = T) %>% pull(gender)))


# How many movers per each country
n_countries_by_gender <- final_complete_dataset %>% filter(!is.na(gender)) %>% distinct(cluster_id, .keep_all = T) %>% group_by(origin_country, gender) %>% tally() %>%  ungroup() %>% spread(gender, n) %>% left_join(european_country_list, by = c("origin_country" = "country")) %>% mutate(region = str_to_title(region))
n_anywhere_movers_by_country <- final_complete_dataset %>% filter(!is.na(gender)) %>% filter(pub_country != origin_country) %>% distinct(cluster_id, .keep_all = T) %>%  group_by(origin_country, gender) %>% tally() %>%  ungroup() %>% spread(gender, n) 
movers_and_nonmovers_by_country <- n_countries_by_gender %>% left_join(n_anywhere_movers_by_country, by = "origin_country", suffix = c("_all", "_movers")) %>% select(origin_country, region, F_all, F_movers, M_all, M_movers)
write.csv(movers_and_nonmovers_by_country, "tables/researchercountrys_by_gender.csv")

############################################################################
################## ANALYSIS OF FREQUENCES OF MOVERS #######################
############################################################################
# no data will be included to run these analyses #

### How many women move anywhere? ###
n_anywhere_movers <- final_complete_dataset %>% filter(!is.na(gender)) %>% filter(pub_country != origin_country) %>% distinct(cluster_id, .keep_all = T) %>% group_by(gender) %>% tally() %>% select(gender, n_anywhere_movers = n)
n_researchers %>% left_join(n_anywhere_movers, by = "gender")

moving_anywhere <- tidy(prop.test(x=c(12213,19062), n=c(78091, 88923), conf.level=0.95)) #15.6% of women move compared to 21.4% of men.

moving_anywhere_OR <- tidy(fisher.test(matrix(c(12213, 19062, 78091-12213, 88923-19062), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "not movers")))))

gender_difference_likelihood_of_moving <- n_researchers %>% 
  left_join(n_anywhere_movers, by = "gender") %>% 
  mutate(prop = c(moving_anywhere$estimate2,moving_anywhere$estimate1), 
         prop_diff = prop - lead(prop),
         prop_diff_conf_low = if_else(!is.na(prop_diff), abs(moving_anywhere$conf.high), NA_real_),
         prop_diff_conf_high = if_else(!is.na(prop_diff), abs(moving_anywhere$conf.low), NA_real_),
         odds_ratio = if_else(!is.na(prop_diff), moving_anywhere_OR$estimate, NA_real_),
         conf.low = if_else(!is.na(prop_diff), abs(moving_anywhere_OR$conf.high), NA_real_),
         conf.high = if_else(!is.na(prop_diff), abs(moving_anywhere_OR$conf.low), NA_real_),
         prop = percent(prop, accuracy = 0.01),
         prop_diff = percent(prop_diff, accuracy = 0.01),
         prop_diff_conf_low = percent(prop_diff_conf_low, accuracy = 0.01),
         prop_diff_conf_high = percent(prop_diff_conf_high, accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate(across(where(is.double), specify_decimal, 2))

write.csv(gender_difference_likelihood_of_moving, "tables/gender_difference_likelihood.csv")

#### How many women are at destination for a minimum of two years #####
n_2yearmin_movers <- final_complete_dataset %>% filter(!is.na(gender)) %>% filter(pub_country != origin_country) %>% group_by(cluster_id, pub_country) %>% summarise(origin_country = first(origin_country), gender = first(gender), n = n(), earliest_year =min(pub_year), last_year = max(pub_year)) %>% ungroup() %>% mutate(duration_years = last_year-earliest_year) %>%  filter(duration_years >=2) %>% arrange(cluster_id,desc(n)) %>% distinct(cluster_id,.keep_all = T) %>% group_by(gender) %>% tally() %>% select(gender, n_movers = n)
n_researchers %>% left_join(n_2yearmin_movers, by = "gender")

7560+12916 #total number of researchers abroad for two years is 20476
((7560+12916)/(78091+88923))*100 #percentage of total researchers that are abroad for two years is 12.26005%

prop.test(x=c(7560,12916), n=c(78091, 88923),
          conf.level=0.95) #9.68% of women move longterm to another country compared to 14.53% of men

0.14524926-0.09681013 #difference between genders in absolute percentage that move abroad is: 4.843913%
fisher.test(matrix(c(7560, 12916, 78091-7560, 88923-12916), ncol = 2,dimnames = list(c("F", "M"), c("Long-term", "not movers")))) #odds ratio 0.6307709

# difference in proportions of those who moved abroad compared to those who moved for at least 2 years #

prop.test(x=c(7560,12916), n=c(12213, 19062),
          conf.level=0.95) #61.90125% of women movers stay for at least 2 years, compared to 67.75784% of men
0.6775784-0.6190125 #absolute difference in percentage is: 5.85659%

fisher.test(matrix(c(7560, 12916, 12213-7560, 19062-12916), ncol = 2,dimnames = list(c("F", "M"),c("Long-term", "short-term")))) #odds ratio = 0.7731509

#### are there any regional differences in gender differences in moving ####
n_researchers_by_region <- final_complete_dataset %>% left_join(european_country_list, by = c("origin_country" = "country")) %>% filter(!is.na(gender)) %>% distinct(cluster_id, .keep_all = T) %>% group_by(gender, region) %>% tally() %>% select(gender, region, n_total = n) %>% ungroup()
n_anywhere_movers_byregion <- final_complete_dataset %>% left_join(european_country_list, by = c("origin_country" = "country")) %>% filter(!is.na(gender)) %>% filter(pub_country != origin_country) %>% distinct(cluster_id, .keep_all = T) %>% group_by(gender, region) %>% tally() %>% select(gender, region, n_anywhere_movers = n) %>%  ungroup()
n_researchers_by_region %>% left_join(n_anywhere_movers_byregion, by = c("gender", "region"))

eastern_prop <- tidy(prop.test(x=c(1245,1959), n=c(13422, 15523),
          conf.level=0.95)) #Eastern: women 9.3%, men = 12.6%
northern_prop <- tidy(prop.test(x=c(2897,4453), n=c(16568, 18695),
          conf.level=0.95)) #Northern: women 17.5, men = 23.8%
southern_prop <- tidy(prop.test(x=c(3431,4271), n=c(20432, 18876),
          conf.level=0.95)) #Southern: 16.8%, men = 22.6%
western_prop <- tidy(prop.test(x=c(4640,8379), n=c(27669, 35829),
                               conf.level=0.95)) #Western: women 16.7%, men = 23.3%

eastern_prop_OR <- tidy(fisher.test(matrix(c(1245, 1959, 13422-1245, 15523-1959), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))
northern_prop_OR <- tidy(fisher.test(matrix(c(2897, 4453, 16568-2897, 18695-4453), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))
southern_prop_OR <- tidy(fisher.test(matrix(c(3431, 4271, 20432-3431, 18876-4271), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))
western_prop_OR <- tidy(fisher.test(matrix(c(4640, 8379, 27669-4640, 35829-8379), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

gender_difference_byregions <- n_researchers_by_region %>% 
  left_join(n_anywhere_movers_byregion, by = c("gender", "region")) %>% 
  arrange(region, gender) %>% 
  mutate(prop = c(eastern_prop$estimate2, eastern_prop$estimate1, northern_prop$estimate2, northern_prop$estimate1,southern_prop$estimate2, southern_prop$estimate1,western_prop$estimate2, western_prop$estimate1)) %>% 
  group_by(region) %>% 
  mutate(prop_diff = prop - lead(prop),
         prop_diff_conf_low = if_else(!is.na(prop_diff) & region == "eastern",abs(eastern_prop$conf.high), 
                            if_else(!is.na(prop_diff) & region == "northern",abs(northern_prop$conf.high), 
                                    if_else(!is.na(prop_diff) & region == "southern",abs(southern_prop$conf.high), 
                                            if_else(!is.na(prop_diff) & region == "western",abs(western_prop$conf.high), NA_real_)))),
         prop_diff_conf_high = if_else(!is.na(prop_diff) & region == "eastern",abs(eastern_prop$conf.low), 
                                      if_else(!is.na(prop_diff) & region == "northern",abs(northern_prop$conf.low), 
                                              if_else(!is.na(prop_diff) & region == "southern",abs(southern_prop$conf.low), 
                                                      if_else(!is.na(prop_diff) & region == "western",abs(western_prop$conf.low), NA_real_)))),
         odds_ratio = if_else(!is.na(prop_diff) & region == "eastern",abs(eastern_prop_OR$estimate), 
                              if_else(!is.na(prop_diff) & region == "northern",abs(northern_prop_OR$estimate), 
                                      if_else(!is.na(prop_diff) & region == "southern",abs(southern_prop_OR$estimate), 
                                              if_else(!is.na(prop_diff) & region == "western",abs(western_prop_OR$estimate), NA_real_)))),
         conf.low = if_else(!is.na(prop_diff) & region == "eastern",abs(eastern_prop_OR$conf.high), 
                            if_else(!is.na(prop_diff) & region == "northern",abs(northern_prop_OR$conf.high), 
                                    if_else(!is.na(prop_diff) & region == "southern",abs(southern_prop_OR$conf.high), 
                                            if_else(!is.na(prop_diff) & region == "western",abs(western_prop_OR$conf.high), NA_real_)))),
         conf.high = if_else(!is.na(prop_diff) & region == "eastern", abs(eastern_prop_OR$conf.low), 
                             if_else(!is.na(prop_diff) & region == "northern", abs(northern_prop_OR$conf.low), 
                                     if_else(!is.na(prop_diff) & region == "southern", abs(southern_prop_OR$conf.low), 
                                             if_else(!is.na(prop_diff) & region == "western", abs(western_prop_OR$conf.low), NA_real_)))),
         prop = percent(prop, accuracy = 0.01),
         prop_diff = percent(prop_diff, accuracy = 0.01),
         prop_diff_conf_low = percent(prop_diff_conf_low, accuracy = 0.01),
         prop_diff_conf_high = percent(prop_diff_conf_high, accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate(across(where(is.double), specify_decimal, 2))

write.csv(gender_difference_byregions, "tables/gender_by_region_likelihood.csv")

# are there any disipline differences in gender differences in moving
n_researchers_by_disicipline <- final_complete_dataset %>% filter(!is.na(gender)) %>% distinct(cluster_id, .keep_all = T) %>% group_by(gender, discipline) %>% tally() %>% select(gender, discipline, n_total = n) %>% ungroup()
n_anywhere_movers_bydiscipline <- final_complete_dataset  %>% filter(!is.na(gender)) %>% filter(pub_country != origin_country) %>% distinct(cluster_id, .keep_all = T) %>% group_by(gender, discipline) %>% tally() %>% select(gender, discipline, n_anywhere_movers = n) %>% ungroup()
n_researchers_by_disicipline %>% left_join(n_anywhere_movers_bydiscipline, by = c("gender", "discipline"))

biology_prop <- tidy(prop.test(x=c(1203,1370), n=c(7001, 6428),conf.level=0.95)) #Biology: women = 17% ,  men = 21%
biology_prop_OR <- tidy(fisher.test(matrix(c(1203, 1370, 7001-1203, 6428-1370), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

biomedical_prop <- tidy(prop.test(x=c(3070,3574), n=c(16179, 12203),conf.level=0.95)) #Biomedical Research: women = 19% ,  men = 29%
biomedical_prop_OR <- tidy(fisher.test(matrix(c(3070, 3574, 16179-3070, 12203-3574), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

chemistry_prop <- tidy(prop.test(x=c(1327,2343), n=c(7138, 9310),conf.level=0.95)) #Chemistry: women = 18.5% ,  men = 25.1%
chemistry_prop_OR <- tidy(fisher.test(matrix(c(1327, 2343, 7138-1327, 9310-2343), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

clinmedicine_prop <- tidy(prop.test(x=c(3662,4113), n=c(32405, 27385),conf.level=0.95)) #Clinical Medicine: women = 11.3% ,  men = 15%
clinmedicine_prop_OR <- tidy(fisher.test(matrix(c(3662, 4113, 32405-3662, 27385-4113), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

earthspace_prop <- tidy(prop.test(x=c(1124,1883), n=c(4484, 6448),conf.level=0.95)) #Earth and Space: women = 25% ,  men = 29.2%
earthspace_prop_OR <- tidy(fisher.test(matrix(c(1124, 1883, 4484-1124, 6448-1883), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

engtech_prop <- tidy(prop.test(x=c(689,2449), n=c(4696, 15149),conf.level=0.95)) #Engineering and Technology: women = 14.6% ,  men = 16.1%
engtech_prop_OR <- tidy(fisher.test(matrix(c(689, 2449, 4696-689, 15149-2449), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

health_prop <- tidy(prop.test(x=c(160,92), n=c(1600, 724),conf.level=0.95)) #Health: women = 10% ,  men = 12.7% (not significant)
health_prop_OR <- tidy(fisher.test(matrix(c(160, 92, 1600-160, 724-92), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

maths_prop <- tidy(prop.test(x=c(153,556), n=c(719, 1681),
          conf.level=0.95)) #Mathematics: women = 21.2% ,  men = 33%
maths_prop_OR <- tidy(fisher.test(matrix(c(153, 556, 719-153, 1681-556), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

physics_prop <- tidy(prop.test(x=c(521,2446), n=c(2239, 8544),conf.level=0.95)) #Physics: women = 23.3% ,  men = 26.8%
physics_prop_OR <- tidy(fisher.test(matrix(c(521, 2446, 2239-521, 8544-2446), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

psychology_prop <- tidy(prop.test(x=c(304,236), n=c(1630, 1051),conf.level=0.95)) #Psychology: women = 18.6% ,  men = 22.5%
psychology_prop_OR <- tidy(fisher.test(matrix(c(304, 236, 1630-304, 1051-236), ncol = 2,dimnames = list(c("F", "M"),c("Movers", "non-movers")))))

gender_difference_bydiscipline <- n_researchers_by_disicipline %>% 
  left_join(n_anywhere_movers_bydiscipline, by = c("gender", "discipline")) %>% 
  arrange(discipline, gender) %>% 
  mutate(prop = c(biology_prop$estimate2, biology_prop$estimate1, 
                  biomedical_prop$estimate2, biomedical_prop$estimate1,
                  chemistry_prop$estimate2, chemistry_prop$estimate1,
                  clinmedicine_prop$estimate2, clinmedicine_prop$estimate1,
                  earthspace_prop$estimate2, earthspace_prop$estimate1,
                  engtech_prop$estimate2, engtech_prop$estimate1, 
                  health_prop$estimate2, health_prop$estimate1,
                  maths_prop$estimate2, maths_prop$estimate1,
                  physics_prop$estimate2, physics_prop$estimate1,
                  psychology_prop$estimate2, psychology_prop$estimate1)) %>% 
  group_by(discipline) %>% 
  mutate(prop_diff = prop - lead(prop),
         prop_diff_conf_low = if_else(!is.na(prop_diff) & discipline == "Biology",abs(biology_prop$conf.high), 
                            if_else(!is.na(prop_diff) & discipline == "Biomedical Research",abs(biomedical_prop$conf.high), 
                                    if_else(!is.na(prop_diff) & discipline == "Chemistry",abs(chemistry_prop$conf.high), 
                                            if_else(!is.na(prop_diff) & discipline == "Clinical Medicine",abs(clinmedicine_prop$conf.high), 
                                                    if_else(!is.na(prop_diff) & discipline == "Earth and Space",abs(earthspace_prop$conf.high), 
                                                            if_else(!is.na(prop_diff) & discipline == "Engineering and Technology",abs(engtech_prop$conf.high), 
                                                                    if_else(!is.na(prop_diff) & discipline == "Health",-abs(health_prop$conf.high), 
                                                                            if_else(!is.na(prop_diff) & discipline == "Mathematics",abs(maths_prop$conf.high), 
                                                                                    if_else(!is.na(prop_diff) & discipline == "Physics",abs(physics_prop$conf.high), 
                                                                                            if_else(!is.na(prop_diff) & discipline == "Psychology",abs(psychology_prop$conf.high), NA_real_)))))))))),
         prop_diff_conf_high = if_else(!is.na(prop_diff) & discipline == "Biology",abs(biology_prop$conf.low), 
                            if_else(!is.na(prop_diff) & discipline == "Biomedical Research",abs(biomedical_prop$conf.low), 
                                    if_else(!is.na(prop_diff) & discipline == "Chemistry",abs(chemistry_prop$conf.low), 
                                            if_else(!is.na(prop_diff) & discipline == "Clinical Medicine",abs(clinmedicine_prop$conf.low), 
                                                    if_else(!is.na(prop_diff) & discipline == "Earth and Space",abs(earthspace_prop$conf.low), 
                                                            if_else(!is.na(prop_diff) & discipline == "Engineering and Technology",abs(engtech_prop$conf.low), 
                                                                    if_else(!is.na(prop_diff) & discipline == "Health",abs(health_prop$conf.low), 
                                                                            if_else(!is.na(prop_diff) & discipline == "Mathematics",abs(maths_prop$conf.low), 
                                                                                    if_else(!is.na(prop_diff) & discipline == "Physics",abs(physics_prop$conf.low), 
                                                                                            if_else(!is.na(prop_diff) & discipline == "Psychology",abs(psychology_prop$conf.low), NA_real_)))))))))),
         odds_ratio = if_else(!is.na(prop_diff) & discipline == "Biology",abs(biology_prop_OR$estimate), 
                              if_else(!is.na(prop_diff) & discipline == "Biomedical Research",abs(biomedical_prop_OR$estimate), 
                                      if_else(!is.na(prop_diff) & discipline == "Chemistry",abs(chemistry_prop_OR$estimate), 
                                              if_else(!is.na(prop_diff) & discipline == "Clinical Medicine",abs(clinmedicine_prop_OR$estimate), 
                                                      if_else(!is.na(prop_diff) & discipline == "Earth and Space",abs(earthspace_prop_OR$estimate), 
                                                              if_else(!is.na(prop_diff) & discipline == "Engineering and Technology",abs(engtech_prop_OR$estimate), 
                                                                      if_else(!is.na(prop_diff) & discipline == "Health",health_prop_OR$estimate, 
                                                                              if_else(!is.na(prop_diff) & discipline == "Mathematics",abs(maths_prop_OR$estimate), 
                                                                                      if_else(!is.na(prop_diff) & discipline == "Physics",abs(physics_prop_OR$estimate), 
                                                                                              if_else(!is.na(prop_diff) & discipline == "Psychology",abs(psychology_prop_OR$estimate), NA_real_)))))))))),
         conf.low = if_else(!is.na(prop_diff) & discipline == "Biology",abs(biology_prop_OR$conf.low), 
                            if_else(!is.na(prop_diff) & discipline == "Biomedical Research",abs(biomedical_prop_OR$conf.low), 
                                    if_else(!is.na(prop_diff) & discipline == "Chemistry",abs(chemistry_prop_OR$conf.low), 
                                            if_else(!is.na(prop_diff) & discipline == "Clinical Medicine",abs(clinmedicine_prop_OR$conf.low), 
                                                    if_else(!is.na(prop_diff) & discipline == "Earth and Space",abs(earthspace_prop_OR$conf.low), 
                                                            if_else(!is.na(prop_diff) & discipline == "Engineering and Technology",abs(engtech_prop_OR$conf.low), 
                                                                    if_else(!is.na(prop_diff) & discipline == "Health",health_prop_OR$conf.low, 
                                                                            if_else(!is.na(prop_diff) & discipline == "Mathematics",abs(maths_prop_OR$conf.low), 
                                                                                    if_else(!is.na(prop_diff) & discipline == "Physics",abs(physics_prop_OR$conf.low), 
                                                                                            if_else(!is.na(prop_diff) & discipline == "Psychology",abs(psychology_prop_OR$conf.low), NA_real_)))))))))),
         conf.high = if_else(!is.na(prop_diff) & discipline == "Biology",abs(biology_prop_OR$conf.high), 
                             if_else(!is.na(prop_diff) & discipline == "Biomedical Research",abs(biomedical_prop_OR$conf.high), 
                                     if_else(!is.na(prop_diff) & discipline == "Chemistry",abs(chemistry_prop_OR$conf.high), 
                                             if_else(!is.na(prop_diff) & discipline == "Clinical Medicine",abs(clinmedicine_prop_OR$conf.high), 
                                                     if_else(!is.na(prop_diff) & discipline == "Earth and Space",abs(earthspace_prop_OR$conf.high), 
                                                             if_else(!is.na(prop_diff) & discipline == "Engineering and Technology",abs(engtech_prop_OR$conf.high), 
                                                                     if_else(!is.na(prop_diff) & discipline == "Health",abs(health_prop_OR$conf.high), 
                                                                             if_else(!is.na(prop_diff) & discipline == "Mathematics",abs(maths_prop_OR$conf.high), 
                                                                                     if_else(!is.na(prop_diff) & discipline == "Physics",abs(physics_prop_OR$conf.high), 
                                                                                             if_else(!is.na(prop_diff) & discipline == "Psychology",abs(psychology_prop_OR$conf.high), NA_real_)))))))))),
         prop = percent(prop, accuracy = 0.01),
         prop_diff = percent(prop_diff, accuracy = 0.01),
         prop_diff_conf_low = percent(prop_diff_conf_low, accuracy = 0.01),
         prop_diff_conf_high = percent(prop_diff_conf_high, accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate(across(where(is.double), specify_decimal, 2))

write.csv(gender_difference_bydiscipline, "tables/gender_by_discipline_likelihood.csv")

# How many Women vs men move to the USA? #
n_usa_movers <- final_complete_dataset %>% filter(!is.na(gender)) %>% filter(pub_country == "United States") %>% distinct(cluster_id, .keep_all = T) %>% group_by(gender) %>% tally() %>% select(gender, n_movers = n)
n_researchers %>% left_join(n_usa_movers, by = "gender")

((3246+5433)/(78091+88923))*100 #What is proportion of total researchers that move to the USA: 5.19657%

prop.test(x=c(3246,5433), n=c(78091, 88923),
          conf.level=0.95) #4.2% of women get a USA affilation compared to 6.1% of men

0.06109780-0.04156689 #absolute difference in percentage that moves to the USA: 1.953091

fisher.test(matrix(c(3246, 5433, 78091-3246, 88923-5433), ncol = 2,dimnames = list(c("F", "M"),c("Movers to USA", "non-movers")))) #odds ratio 0.664742

#### Are there any career age related differences? ####
n_anywhere_movers_by_age <- final_complete_dataset %>% filter(!is.na(gender)) %>% filter(pub_country != origin_country) %>% arrange(cluster_id, career_year) %>%  distinct(cluster_id, .keep_all = T) %>% group_by(gender, career_year) %>% tally() %>% select(gender, career_year, n_anywhere_movers = n) %>% ungroup()
n_researchers %>% left_join(n_anywhere_movers_by_age, by = c("gender"))

plot_multi_histogram <- function(df, feature, label_column, means) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black", bins = 11, binwidth = 1) +
    geom_density(alpha=0.6, bw = 0.7) +
    geom_vline(xintercept=means, color=c("#f9b282","#a6d8f0"), linetype="solid", size=1) +
    labs(x="Career Year", y = "Density")
  plt + guides(fill=guide_legend(title=label_column)) +
    theme_minimal() + scale_fill_manual( values = c("#f9b282","#a6d8f0")) + 
    scale_x_continuous(breaks = seq(1, 12, by = 2))
} # code taken from : https://stackoverflow.com/a/53680101

uncounted_data <- uncount(n_anywhere_movers_by_age) %>% mutate(Gender = if_else(gender == "M", "Male","Female"), career_year = career_year+1) %>% select(Gender, career_year)
age_differences_plot <- plot_multi_histogram(uncounted_data, 'career_year', 'Gender', c(mean(uncounted_data[which(uncounted_data$Gender == "Female"),]$career_year),mean(uncounted_data[which(uncounted_data$Gender == "Male"),]$career_year)))
age_differences_plot
ggsave(age_differences_plot, filename = "plots/age_gender.pdf", device = "pdf", width =6, height = 5)


