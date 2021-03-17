#script for jesper
library(tidyverse)

#importing data
eu_univ_1176_eligible_researchers <- read_delim("~/Desktop/WoS Data from Jesper/eu_univ_1176_eligible_researchers.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
eu_univ_1176_pub_vars <- read_delim("~/Desktop/WoS Data from Jesper/eu_univ_1176_pub_vars.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
eu_univ_1176_pubs <- read_delim("~/Desktop/WoS Data from Jesper/eu_univ_1176_pubs.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
eu_merged_indicator_citvar <- read_delim("~/Desktop/WoS Data from Jesper/eu-merged-indicator-citvar.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)
eu_merged_indicator_cit3yr <- read_delim("~/Desktop/WoS Data from Jesper/eu-merged-indicator-cit3yr.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#ISSUE ONE - first_year not equal to first pub_year
#first publication info for all authors
#NB: check the first_year vs the pub_year, they after often not the same!
x <- eu_univ_1176_eligible_researchers %>% 
  distinct(cluster_id, .keep_all = T) %>% #some authors have double affiliations but it's not important here, so i just keep one instance of each cluster_id
  left_join(eu_univ_1176_pubs, by = "cluster_id") %>% #left join the pub information to the eligibe researcher dataset
  left_join(eu_univ_1176_pub_vars %>% distinct(ut, .keep_all=T), by = "ut") %>%  #left join the publication_variables dataset (NB I extract only distinct uts, because of ISSUE TWO)
  arrange(cluster_id, pub_year) %>% #arrange by cluster id so that publications are in chronological order
  group_by(cluster_id) %>%
  filter(row_number()==1) #keep only the first publication per author #keep only the rows where first year is NOT equal to first pub_year

#ISSUE TWO - duplicates within the eu_univ_1176_pub_vars dataset
eu_univ_1176_pub_vars %>% 
  group_by(ut) %>% 
  add_tally(name = "n_rows") %>% #counts the number of rows per group (which is set to ut)
  ungroup() %>% 
  arrange(ut) %>% 
  filter(n_rows > 1) %>% #only showing the uts with >1 row
  print(n=20)
