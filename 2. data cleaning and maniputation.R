source("1. importing data.R") #importing data and packages-.
#identifying movers

#STEP ONE#
#identifying the three key datasets...
#1. eu_univ_1176_eligible_researchers - SHOULD BE (but actually isn't, because some authors had >1 affilation at scientific birth) one row for each author, showing name, city, country, gender etc
#the way to solve the duplication problem is either to take researchers with...

one_institution_authors <- eu_univ_1176_eligible_researchers %>% #one institution# (this is the one I'm using generally)
  group_by(cluster_id) %>% 
  mutate(nrow = n()) %>% 
  filter(nrow == 1) %>% 
  select( -nrow)

# one_city_authors <- eu_univ_1176_eligible_researchers %>% #one city#
#   group_by(cluster_id) %>% 
#   mutate(nrow = n(),
#          n_distinct_cities =n_distinct(city)) %>% 
#   filter(n_distinct_cities == 1)

# one_countries_authors <- eu_univ_1176_eligible_researchers %>% #one country#
#   group_by(cluster_id) %>% 
#   mutate(nrow = n(),
#          n_distinct_countries =n_distinct(country)) %>% 
#   filter(n_distinct_countries == 1)

#2. eu_univ_1176_pubs - articles per author, containing place in author list, FIRST INSTITUTION/COUNTRY and current institution/country
#There are issues here - some authors have many home_org_names, but i think when joining to one_institution_authors, this will be solved (since all authors with >1 institution at scientific birth will be removed)
#note 10/02/2021: There may still be problems, since their first publication isn't the same as what decides if they are in the eu_univ_1176_eligible_researchers dataset!

#3. eu_univ_1176_pub_vars - for each author it gives publication year, doi, collaboration type
#I noticed there are some duplicated (>40,000 rows of duplicates) in this dataset, so i only use the first row of distinct ut.
#I think this is because some publications have multiple "journal specialities", leading to multiple rows of the same article. Therefore it should be okay to just use distinct ut (article id) to remove the duplicates. Note 10/02/2021: This was confirmed by Jesper
#we also need to only include authors with first article between 2008-2010

#STEP TWO# 
#Joining the datasets together...

publications <- eu_univ_1176_pubs %>% 
  left_join(eu_univ_1176_pub_vars %>% distinct(ut, .keep_all=T), by = "ut") %>% #joining with eu_univ_1176_pub_vars (only those with a distinct ut)
  group_by(cluster_id) %>% 
  mutate(min_publicationyear_perauthor = min(pub_year)) %>% 
  ungroup()

author_publications <- one_institution_authors %>% 
  left_join(publications, by = "cluster_id") %>% 
  filter(between(min_publicationyear_perauthor, 2008, 2010)) %>%  #note that this removes ALL researchers IF they had a publication where there was an NA for pub_year. 
  ungroup()
  
#STEP THREE#
#Investigating our new dataset...
length(unique(author_publications$ut)) #1,398,148 articles'
length(unique(author_publications$cluster_id)) #157,998 researchers

#first I look at how many affiliations each unique author per paper has
author_publications_with_bygroup_rowcounts <- author_publications %>% group_by(cluster_id, ut) %>% mutate(n_affiliations_per_paperauthor=n(), n_pubcountries_per_paperauthor = n_distinct(pub_country)) %>% ungroup()
table(author_publications_with_bygroup_rowcounts %>% distinct(ut, .keep_all = T) %>% pull(n_affiliations_per_paperauthor)) #how many affilations is too many? 

#second, I look at how many countries authors are affilated with
table(author_publications_with_bygroup_rowcounts%>% distinct(ut, .keep_all = T) %>% pull(n_pubcountries_per_paperauthor)) #Can we trust papers where authors have 3,4 or 5 countries of affilation?

#to get a list of authors where their first paper (or more specifically papers within the first year that they had a publication) had more than one affilation. You can use this to filter them out if needed
author_publications_with_bygroup_rowcounts %>% filter(n_affiliations_per_paperauthor > 1 & pub_year == min_publicationyear_perauthor) %>% pull(cluster_id)

#STEP THREE#
#adjusting our main dataset based on our findings in step 3
#i'm going to only include authors with a maximum of 3 country affilations EVER, this should also reduce the problem with cases whereby authors have been incorrectly been given all affilations in, for example, a large consortium paper

author_publications_3countrylimit <- author_publications %>% group_by(cluster_id, ut) %>% mutate(n_affiliations_per_paperauthor=n(), n_affiliationcountries_per_paperauthor = n_distinct(pub_country)) %>% group_by(cluster_id) %>% mutate(total_number_of_distinct_pub_countries = n_distinct(pub_country)) %>% ungroup() %>% filter(total_number_of_distinct_pub_countries <= 3)

#and now I will check the distribution of affilations
table(author_publications_3countrylimit %>% distinct(ut, .keep_all = T) %>% pull(n_affiliations_per_paperauthor)) #some authors still have up to 12 affilations. This seems unlikely. Let's have a look at what's going on...

author_publications_3countrylimit %>% filter(n_affiliations_per_paperauthor > 10) %>% arrange(cluster_id,ut, desc(n_affiliations_per_paperauthor)) %>% select(cluster_id,ut,author_name,doi,home_org_name, home_country,pub_org_name, pub_country, n_affiliationcountries_per_paperauthor,n_affiliations_per_paperauthor) %>% print(n=40)     
#i looked at the doi of the first person, yamada, r, and she is indeed affilated to two countries. HOWEVER, she was only, in the paper, affilated to FOUR institutions not 11 as in our dataset
#it generally seems like the french end up with a lot of extra affiliations for some reason
#i also looked at the doi of the third person, hatoum, ij, and she was indeed affilated both to the US and France, but only had 3 affilations
#Bottom line: French affilations do not seem accurate, but based on this small investigation, affiliation countries are accurate

#STEP FOUR#
#let's check if I can identify movers

author_publications_3countrylimit %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% distinct(cluster_id, .keep_all=T) %>% select(cluster_id, author_name, wos_name, country, first_year,n_distinct_pubcountries_total) %>% count(n_distinct_pubcountries_total)
#according to this, we have roughly 53,000 researchers that have had at least one NEW affilation to a different country added

author_publications_3countrylimit %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% filter(n_distinct_pubcountries_total>1) %>% arrange(cluster_id, pub_year) %>%  View()
#so after doing the code above, i see a few things:
# 1. we don't have authors first publication!!! For example, if you run this code: 
eu_univ_1176_pubs %>% filter(cluster_id == 124) %>% left_join(eu_univ_1176_pub_vars, by = "ut") %>% left_join(one_institution_authors, by = "cluster_id")
#it shows that this author has a "first_year" of 2007, bit the earlist publication we have is 2008. This makes me nervous - how many publiations are we missing? just the first one or even more?? same with, for example, author 1345
#2. it looks like we have a good timeline of author movements!

#To make it easier to interpret author movements, I will make a dataset that just contains the author movements showing unique country affilations per author (and which year the new affilation arrived)
author_publications_3countrylimit %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% filter(n_distinct_pubcountries_total>1) %>% arrange(cluster_id, pub_year) %>% distinct(cluster_id, pub_country, .keep_all = T) %>% select(cluster_id, pub_country, pub_year) %>% group_by(cluster_id) %>%  mutate(move_n = row_number()-1, years_since_previousmove = pub_year-lag(pub_year)) %>% print(n = 50) 
#some authors seem to be moving to TWO countries in the same year. This doesn't seem likely. What should we do about these cases? Exclude?

#STEP FIVE#
#understanding more about the movements of the researchers
countries_moved_to <- author_publications_3countrylimit %>% group_by(cluster_id) %>% mutate(n_distinct_pubcountries_total =n_distinct(pub_country)) %>% ungroup() %>% filter(n_distinct_pubcountries_total>1) %>% arrange(cluster_id, pub_year) %>% distinct(cluster_id, pub_country, .keep_all = T) %>% select(cluster_id, pub_country, pub_year) %>% group_by(cluster_id) %>%  mutate(move_n = row_number()-1, years_since_previousmove = pub_year-lag(pub_year)) %>% ungroup() %>%   count(pub_country)

#just for fun, I made a map with where people moved
library(rworldmap)

mapped_data <- joinCountryData2Map(countries_moved_to, joinCode = "NAME", 
                                   nameJoinColumn = "pub_country")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data, nameColumnToPlot = "n",colourPalette = "diverging",catMethod ="pretty",numCats=200)

