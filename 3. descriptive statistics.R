source("2. data cleaning and maniputation.R") #importing data and packages

##########################################
## TO BE COMPLETED ##
##########################################

#### basic demographics about the whole dataset ####

length(unique(final_complete_dataset$ut)) #1,670,813 articles
length(unique(final_complete_dataset$cluster_id)) #178,621 researchers

#### about the matched dataset

matched_dataset
dim(matched_dataset) #54000 years of data
dim(matched_dataset %>% filter(career_over == F))
n_distinct(matched_dataset$cluster_id) #4500 resarchers


#### about the final diff-in-diff dataset

diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>%  count(discipline) #do this to see how many matches per discipline
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(origin_country) %>% print(n=100) #do this to see how many matches per discipline

diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>%filter(condition_numeric == 1) %>% count(gender) #do this to see how many matches per discipline
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(origin_type) #i manually went through the missing - 14 = education, 1 = archive, 25 = facility, 12 = government, 14 = healthcare, 6 = nonprofit
diffindiff_data %>% distinct(cluster_id, .keep_all = T) %>% filter(condition_numeric == 1) %>% count(USA_type) # i manually went through the missing - 1 = education, 1 =archive, 1 = facility, 1= gov, 2 = health, 4 = nonprofit

