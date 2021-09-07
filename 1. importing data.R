pacman::p_load(tidyverse, readxl)

###########################################################
################## IMPORTING RAW DATA #####################
###########################################################

#This little bit of code helps to turn names of months into months as numeric
months <- seq(1,12,0.5) #i make a sequence of numeric months, but also with 0.5 decimals in between to use when the month was written as for example "Jan-Feb"
names(months) <- c("JAN","JAN-FEB", "FEB", "FEB-MAR", "MAR","MAR-APR", "APR","APR-MAY",#now I'm matching the numeric months with the names
                   "MAY","MAY-JUN", "JUN","JUN-JUL", "JUL","JUL-AUG", "AUG","AUG-SEP", #note that there are other text months in the data that I couldn't easily convert to numeric. For example: "Jan-July".
                   "SEP","SEP-OCT", "OCT", "OCT-NOV", "NOV","NOV-DEC", "DEC") 

#######################
### loading in data ###
#######################

##this dataset was for finding eligible authors from WoS. However it has some useful info (author names, gender, gender accuracy), so we extract that 
researcher_info <- read_delim("raw_data/wos/univ-1176-elegible-researchers2.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  select(cluster_id, full_name, first_name, gender, gender_accuracy = accuracy)

## this dataset is the list of publications per author
raw_publication_list <- 
  read_delim("raw_data/wos/univ-1176-pubs-left-join.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% #this is the list of publications for each of the authors we "extracted" from web of science
  rbind(read_delim("raw_data/wos_extra/new-cluster-ids-pub-left-join.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% mutate(lr_univ_id = NA, lr_region = NA, lr_country_name = NA, lr_univ_name = NA, lr_city = NA)) %>% 
  mutate(months_numeric = unname(months[pub_month])) %>% 
  group_by(cluster_id, pub_year) %>% 
  mutate(months_numeric_null_is_min_of_year = if_else(is.na(months_numeric) == F,  months_numeric, min(months_numeric, na.rm=T)), #new var: 1. "months_numeric" - A variable that gives month as a numeric value
         months_numeric_null_is_min_of_year = if_else(is.infinite(months_numeric_null_is_min_of_year) == T, 1, months_numeric_null_is_min_of_year)) %>% # new var: 2. "months_numeric_null_is_min_of_year" - A variable that gives month as a numeric value, where missing months (NULL) are given the same value as the earliest other article published in that given year for that author. Failing that it is given the value 1.
  ungroup() 

intermediatestep_publication_order <- #here I calculate the order of publications per author. I add it to the above dataset in the next step
  raw_publication_list %>% 
  distinct(cluster_id, ut, .keep_all= T) %>% 
  arrange(cluster_id, pub_year, months_numeric_null_is_min_of_year, ut) %>% 
  distinct(cluster_id, ut, pub_year, months_numeric_null_is_min_of_year) %>% #publication order based on pub_year and months_numeric_null_is_min_of_year. If two articles have the same, then it will be decided by UT (Accession Number)
  group_by(cluster_id) %>% 
  mutate(order_of_publishing = row_number())

#creating the main dataset - contains: author info + all publications per author + "order_of_publishing" + first_year + last_year + career year

publication_list_all <- raw_publication_list %>% 
  left_join(intermediatestep_publication_order, by = c("cluster_id", "ut", "pub_year", "months_numeric_null_is_min_of_year")) %>% #adding order_of_publishing
  left_join(researcher_info, by ="cluster_id") %>% #adding in our basic researcher data
  select(-author_seq, -affiliation_seq, -months_numeric) %>%  #removing just a couple of unused variables %>% 
  arrange(cluster_id, pub_year, months_numeric_null_is_min_of_year) %>% 
  group_by(cluster_id) %>% 
  mutate(first_year = min(pub_year), #this replaces the variable given by jesper since his version included conference abstracts
         last_year = max(pub_year),
         career_year = pub_year-first_year) %>% #new variable: career year (0 = first year)
  ungroup() %>% 
  distinct(cluster_id, ut, pub_org, .keep_all = T) #I noticed there were some duplicate rows, so this is my way of removing them

#Next, i import: articles metadata + citations
publication_info <- read_delim("raw_data/wos/univ-1176-pub-vars-left-join.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>%  
  rbind(read_delim("raw_data/wos_extra/new-cluster-ids-pub-vars-left-join.txt", "\t", escape_double = FALSE, trim_ws = TRUE)) %>% distinct(ut, .keep_all = T) #had to select a single discipline/specialty for those that had multiple. First in dataframe is chosen.
citation_3year_original <- read_delim("raw_data/wos/merge-indicator-cit3yr.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% select(-p_full,-p_frac, -cs_full,-cs_frac, -ncs_full,-ncs_frac, -js_full,-js_frac, -njs_full, -njs_frac) #we have updated values for bibliometric indicators so i have removed them here.
#citation_all_info <- read_delim("data/wos/merged-indicator-citvar.txt", "\t", escape_double = FALSE, trim_ws = TRUE) #we're not using the full citations, so i commented this out
updated_3_year_njs_full_data <- read_delim("raw_data/wos/njs_indicators.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% select(-pub_year, -source_title)
updated_3_year_njs_frac_data <- read_delim("raw_data/wos/njs_indicators_frac.txt", "\t", escape_double = FALSE, trim_ws = TRUE) 
citation_3year_step1 <- citation_3year_original %>% left_join(updated_3_year_njs_full_data, by ="ut") %>% left_join(updated_3_year_njs_frac_data, by ="ut")

extra_clusterid_3year_info <- read_delim("raw_data/wos_extra/extra_cluster_ids_merged_indicator_cit3yr.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% select(-p_full,-p_frac, -cs_full,-cs_frac, -ncs_full,-ncs_frac, -js_full,-js_frac, -njs_full, -njs_frac)
extra_clusterid_njs_indicators <- read_delim("raw_data/wos_extra/new-cluster-ids-njs_indicators.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% select(everything(),"source_title" = journal)

citation_3year_info <- citation_3year_step1 %>% 
  rbind(extra_clusterid_3year_info %>% left_join(extra_clusterid_njs_indicators) %>% select(names(citation_3year_info)))
  
  
#Next, I import the wos institution names that i matched to their grid profiles using the dimensions api. The main dataset "institute_grids_ids" contains also what "type of institute"
wos_matched_to_grid <- read_csv("raw_data/misc/wos_matched_to_grid_final.csv")
grid_institute_types <- read_csv("raw_data/misc/grid_institute_types.csv")
institute_grid_ids <- wos_matched_to_grid %>% left_join(grid_institute_types, by = "grid_id") %>% select(grid_id, wos_institute_name, type)

#Next, I import the university rankings. these were matched to the institute names in wos by first matching them to grid.
qs_ranking <- read_csv("raw_data/misc/qs_ranking_and_grid_finalfinal.csv") %>% select(university, year_released, overall_rank, overall_score,reputation_rank,reputation_score, grid_id)
LR_full_name <- read_excel("raw_data/misc/LR-full-name.xlsx") %>% select(lr_univ_id = cwts_organization_id, University = full_name, wos_name)
CWTS_Leiden_Ranking_2020 <- read_excel("raw_data/misc/CWTS Leiden Ranking 2020.xlsx", sheet = "Results") %>% filter(Field == "All sciences")

leiden_ranking <- CWTS_Leiden_Ranking_2020 %>% left_join(LR_full_name, by ="University")

#Next, i import a list of countries that count as being "Europe"
european_country_list <- read_csv("raw_data/misc/country_list.csv", col_names =T)

#Finally, i make a single RData file so in future i don't have to run this script agains
save(researcher_info,
     publication_list_all,
     publication_info,
     citation_3year_info,
     european_country_list,
     institute_grid_ids,
     qs_ranking,
     leiden_ranking,
     file = "wos_data.RData")
