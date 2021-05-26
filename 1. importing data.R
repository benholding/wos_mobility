library(tidyverse)

#This little bit of code helps to turn names of months into months as numeric
months <- seq(1,12,0.5) #i make a sequence of numeric months, but also with 0.5 decimals in between to use when the month was written as for example "Jan-Feb"
names(months) <- c("JAN","JAN-FEB", "FEB", "FEB-MAR", "MAR","MAR-APR", "APR","APR-MAY",#now I'm matching the numeric months with the names
                   "MAY","MAY-JUN", "JUN","JUN-JUL", "JUL","JUL-AUG", "AUG","AUG-SEP", #note that there are other text months in the data that I couldn't easily convert to numeric. For example: "Jan-July".
                   "SEP","SEP-OCT", "OCT", "OCT-NOV", "NOV","NOV-DEC", "DEC") 

#######################
### loading in data ###
#######################

#this first one is just to get author names, gender, and first/last year
researcher_info <- read_delim("~/Desktop/WoS Data from Jesper/univ-1176-elegible-researchers2.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% #this dataset was for finding eligible authors from WoS. However it has some useful info, so we extract that
  distinct(cluster_id, .keep_all = T) %>% 
  select(cluster_id, full_name, first_name, first_year, last_year, gender, gender_accuracy = accuracy)

#this is the list of publications per authors that we took from WoS. 
#i add a few extra variables:
## 1. "months_numeric" - A variable that gives month as a numeric value
## 2. "months_numeric_null_is_min_of_year" - A variable that gives month as a numeric value, where missing months (NULL) are given the same value as the earliest other article published in that given year for that author
## 3. "is_first_year" - A variable reporting whether this article is from an authors first year
## 4. "is_earliest_timepoint" - A variable that shows whether this article (UT) was published at the earliest timepoint (year, month) recorded
raw_publication_list <- 
  read_delim("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/univ-1176-pubs-left-join.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(months_numeric = unname(months[pub_month])) %>% 
  group_by(cluster_id) %>%
  mutate(is_first_year = if_else(pub_year == min(pub_year),1,0)) %>%
  group_by(cluster_id, pub_year) %>% 
  mutate(months_numeric_null_is_min_of_year = if_else(is.na(months_numeric) == F,  months_numeric, min(months_numeric, na.rm=T)),
         months_numeric_null_is_min_of_year = if_else(is.infinite(months_numeric_null_is_min_of_year) == T, 1, months_numeric_null_is_min_of_year)) %>% 
  ungroup() 

intermediatestep_publication_order <- #here I calculate the order of publications per author. I add it to the above dataset in the next step
  raw_publication_list %>% 
  distinct(cluster_id, ut, .keep_all= T) %>% 
  arrange(cluster_id, pub_year, months_numeric_null_is_min_of_year, ut) %>% 
  distinct(cluster_id, ut, pub_year, months_numeric_null_is_min_of_year) %>% #publication order based on pub_year and months_numeric_null_is_min_of_year. If two articles have the same, then it will be decided by UT (Accession Number)
  group_by(cluster_id) %>% 
  mutate(order_of_publishing = row_number())

#this is the main dataset.
#compared to the raw_publication_list i have added:
## 1. "order_of_publishing" - publication order based on pub_year and months_numeric_null_is_min_of_year. If two articles have the same, then it will be decided by UT (Accession Number)
## 2. "author_affilations_count_per_ut" - number of author affiliations for a given paper (i.e. how many lines did an author have with the same UT)

publication_list_all <- raw_publication_list %>% 
  left_join(intermediatestep_publication_order, by = c("cluster_id", "ut", "pub_year", "months_numeric_null_is_min_of_year")) %>% 
  group_by(cluster_id, ut) %>% 
  mutate(author_affilations_count_per_ut = n()) %>% #here for each combination of cluster_id and ut, i count the number of rows to assess the number of affiliations per given author for this paper
  left_join(researcher_info, by ="cluster_id") %>% #adding in our basic researcher data
  ungroup() %>% 
  select(-affiliation_seq, -months_numeric) %>%  #removing just a couple of unused variables %>% 
  arrange(cluster_id, pub_year, months_numeric_null_is_min_of_year) %>% 
  group_by(cluster_id) %>% 
  mutate(career_year = pub_year-first_year,
         career_length_months_at_this_pub = ((career_year*12)+months_numeric_null_is_min_of_year)-first(months_numeric_null_is_min_of_year)) %>% 
  distinct(cluster_id, ut, pub_org, .keep_all = T) %>% #I noticed there were some duplicate rows, so this is my way of removing them
  ungroup()

#Here we have the datasets that contain detailed information about the individual articles. I.e. discipline, citations etc
publication_info <- read_delim("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/univ-1176-pub-vars-left-join.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
citation_3year_info <- read_delim("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/merge-indicator-cit3yr.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
citation_all_info <- read_delim("/Users/ben/Desktop/WoS\ Data\ from\ Jesper/merged-indicator-citvar.txt", "\t", escape_double = FALSE, trim_ws = TRUE)



# #quick sanity check
# table(unique(publication_info$ut) %in% unique(citation_all_info$ut)) #shows that all uts in publication_info are in citation_all_info
# x <- publication_info %>% distinct(ut, .keep_all=T) %>% left_join(citation_all_info, by ="ut")
# table(is.na(x$p_full)) # shows that there aren't any of the "NULL" rows that were in the citation_all_info df now in the combined dataset. seems good.
