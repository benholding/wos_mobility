library(tidyverse)

eu_univ_1176_eligible_researchers <- read_delim("~/Desktop/WoS Data from Jesper/univ-1176-elegible-researchers2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# # once we had all authors from wos of science that matched our initial critieria, we identified our final author sample, where it could only be a type 1 or 2 research article, their first article, and they needed at least one affilation to a leiden ranked institution
months <- seq(1,12,0.5)
names(months) <- c("JAN","JAN-FEB", "FEB", "FEB-MAR", "MAR","MAR-APR", "APR","APR-MAY",
                   "MAY","MAY-JUN", "JUN","JUN-JUL", "JUL","JUL-AUG", "AUG","AUG-SEP",
                   "SEP","SEP-OCT", "OCT", "OCT-NOV", "NOV","NOV-DEC", "DEC")

# original <- eu_univ_1176_eligible_researchers %>%
#   filter(pub_doc_type_id == 1 | pub_doc_type_id == 2) %>% #only including type 1 or type 2 articles
#   mutate(months_numeric = unname(months[pub_month])) %>%
#   group_by(cluster_id) %>%
#   mutate(is_min_year = if_else(pub_year == min(pub_year),1,0), #determining first year per cluster_id
#          is_min_month = if_else(is_min_year == 0, 0,
#                                 if_else(pub_month == min(pub_month, na.rm=T),1,0))) %>% #assessing which row represents the earliest month in the first year per cluster_id
#   filter(is_min_month == 1 | is.na(is_min_month), #only keeping in rows where it is the first year and either 1. it is the earliest month or 2. we had missing data for month
#          pub_org == lr_univ_id) #only keeping rows where pub_org is the same as lr_univ_id. i.e. affilation is in leiden ranking
# 
# unique_cluster_ids_original <- original %>% distinct(cluster_id) # getting distinct cluster IDS

# new <- eu_univ_1176_eligible_researchers %>%
#   filter(pub_doc_type_id == 1 | pub_doc_type_id == 2) %>% #only including type 1 or type 2 articles
#   mutate(months_numeric = unname(months[pub_month])) %>%
#   group_by(cluster_id) %>%
#   mutate(is_min_year = if_else(pub_year == min(pub_year),1,0)) %>% #determining first year per cluster_id
#   group_by(cluster_id, pub_year) %>% 
#   mutate(is_min_month = if_else(is_min_year == 0, 0,
#                                 if_else(months_numeric == min(months_numeric, na.rm=T),1,0))) %>% #assessing which row represents the earliest month in the first year per cluster_id
#   ungroup() %>% 
#   filter(is_min_month == 1 | is.na(is_min_month), #only keeping in rows where it is the first year and either 1. it is the earliest month or 2. we had missing data for month
#          pub_org == lr_univ_id) #only keeping rows where pub_org is the same as lr_univ_id. i.e. affiliation is in Leiden ranking
# 
# unique_cluster_ids_new <- new %>% distinct(cluster_id) # getting distinct cluster IDS
# 
# table(unique_cluster_ids_new$cluster_id %in% unique_cluster_ids_original$cluster_id) # there are 76403 that we should have that are missing.
# 
# write_csv(unique_cluster_ids_new, "data/unique_cluster_ids_18march2021.csv") #writing the dataframe into a csv

##############################################################################
#### then once we took all publications for our eligible researchers: ####
##############################################################################

author_info_per_publication <- read_delim("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/univ-1176-pubs-left-join.txt", 
                                          "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(months_numeric = unname(months[pub_month])) %>% 
  group_by(cluster_id) %>%
  mutate(is_first_year = if_else(pub_year == min(pub_year),1,0)) %>%
  group_by(cluster_id, pub_year) %>% 
  mutate(is_earliest_timepoint = if_else(is_first_year == 0, 0,
                                if_else(months_numeric == min(months_numeric, na.rm=T),1,0))) %>% 
  ungroup() 
  
publication_info <- read_delim("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/univ-1176-pub-vars-left-join.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)


citation_all_info <- read_csv("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/merge-indicator-citvar.csv")


citation_3year_info <- read_delim("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/merge-indicator-cit3yr.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)

# #quick sanity check
# table(unique(publication_info$ut) %in% unique(citation_all_info$ut)) #shows that all uts in publication_info are in citation_all_info
# x <- publication_info %>% distinct(ut, .keep_all=T) %>% left_join(citation_all_info, by ="ut")
# table(is.na(x$p_full)) # shows that there aren't any of the "NULL" rows that were in the citation_all_info df now in the combined dataset. seems good.
