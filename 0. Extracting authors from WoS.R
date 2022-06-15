# the first step in this project was to get data from our WoS database. 
#Here after being sent the names of researchers publishing within our timeframe (2008-2010) I tried to get the IDs of the researchers that we wanted full publications for
## criteria include:
## 1. only type 1 or type 2 research articles as first articles
## 2. The publication was in the earliest year, and either the earliest month or month was NA
# Once I had the IDs we then extracted full publication profiles for our authors

library(tidyverse)

eu_univ_1176_eligible_researchers <- read_delim("~/Desktop/WoS Data from Jesper/univ-1176-elegible-researchers2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# # once we had all authors from wos of science that matched our initial criteria, we identified our final author sample, where it could only be a type 1 or 2 research article and be their first article
months <- seq(1,12,0.5)
names(months) <- c("JAN","JAN-FEB", "FEB", "FEB-MAR", "MAR","MAR-APR", "APR","APR-MAY",
                   "MAY","MAY-JUN", "JUN","JUN-JUL", "JUL","JUL-AUG", "AUG","AUG-SEP",
                   "SEP","SEP-OCT", "OCT", "OCT-NOV", "NOV","NOV-DEC", "DEC")

eligible_researchers_all <- eu_univ_1176_eligible_researchers %>%
  filter(pub_doc_type_id == 1 | pub_doc_type_id == 2) %>% #only including type 1 or type 2 articles
  mutate(months_numeric = unname(months[pub_month])) %>%
  group_by(cluster_id) %>%
  mutate(is_min_year = if_else(pub_year == min(pub_year),1,0)) %>% #determining first year per cluster_id
  group_by(cluster_id, pub_year) %>%
  mutate(is_min_month = if_else(is_min_year == 0, 0,
                                if_else(months_numeric == min(months_numeric, na.rm=T),1,0))) %>% #assessing which row represents the earliest month in the first year per cluster_id
  ungroup() %>%
  filter(is_min_month == 1 | is.na(is_min_month)) #only keeping in rows where it is the first year and either 1. it is the earliest month or 2. we had missing data for month

eligible_researchers_all_excluding_NAs <- eligible_researchers_all %>% 
  filter(!is.na(pub_org_name)) #only asking for cluster_ids where at their first measureable publication (i.e. first year, and earliest month) they had a pub_org_name

unique_cluster_ids_eligible_researchers_all_excluding_NAs <- eligible_researchers_all_excluding_NAs %>% distinct(cluster_id) # getting distinct cluster IDs
missing_researchers <- !c(unique_cluster_ids_eligible_researchers_all_excluding_NAs$cluster_id %in% unique_cluster_ids_eligible_researchers_only_leidenranked$cluster_id)

unique_cluster_ids_eligible_researchers_all_excluding_NAs[missing_researchers,] %>% write.csv("new_cluster_ids_to_extract.csv")