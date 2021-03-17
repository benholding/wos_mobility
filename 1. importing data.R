library(tidyverse)

eu_univ_1176_eligible_researchers <- read_delim("~/Desktop/WoS Data from Jesper/univ-1176-elegible-researchers2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# once we had all authors from wos of science that matched our initial critieria, we identified our final author sample, where it could only be a type 1 or 2 research article, their first article, and they needed at least one affilation to a leiden ranked institution
months <- seq(1,12,0.5)
names(months) <- c("JAN","JAN-FEB", "FEB", "FEB-MAR", "MAR","MAR-APR", "APR","APR-MAY", 
                   "MAY","MAY-JUN", "JUN","JUN-JUL", "JUL","JUL-AUG", "AUG","AUG-SEP", 
                   "SEP","SEP-OCT", "OCT", "OCT-NOV", "NOV","NOV-DEC", "DEC")

for_jesper <- eu_univ_1176_eligible_researchers %>% 
  filter(pub_doc_type_id == 1 | pub_doc_type_id == 2) %>% #only including type 1 or type 2 articles
  mutate(months_numeric = unname(months[pub_month])) %>% 
  group_by(cluster_id) %>% 
  mutate(is_min_year = if_else(pub_year == min(pub_year),1,0), #determining first year per cluster_id
         is_min_month = if_else(is_min_year == 0, 0, 
                                if_else(pub_month == min(pub_month, na.rm=T),1,0))) %>% #assessing which row represents the earliest month in the first year per cluster_id
  filter(is_min_month == 1 | is.na(is_min_month), #only keeping in rows where it is the first year and either 1. it is the earliest month or 2. we had missing data for month
         pub_org == lr_univ_id) #only keeping rows where pub_org is the same as lr_univ_id. i.e. affilation is in leiden ranking

unique_cluster_ids <- for_jesper %>% distinct(cluster_id) # getting distinct cluster IDS

write_csv(unique_cluster_ids, "data/unique_cluster_ids_12march2021.csv") #writing the dataframe into a csv

##############################################################################
#### then once we took all publications for our eligible researchers: ####
##############################################################################

eu_univ_1176_pub_vars <- read_delim("~/Desktop/WoS Data from Jesper/eu-univ-1176-pub-vars-left-join2.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)

eu_univ_1176_pubs <- read_delim("~/Desktop/WoS Data from Jesper/eu-univ-1176-pubs-left-join2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

eu_merged_indicator_citvar <- read_delim("~/Desktop/WoS Data from Jesper/eu-merged-indicator-citvar.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)

eu_merged_indicator_cit3yr <- read_delim("~/Desktop/WoS Data from Jesper/eu-merged-indicator-cit3yr.txt", "\t", escape_double = FALSE, trim_ws = TRUE)