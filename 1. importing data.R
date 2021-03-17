library(tidyverse)

eu_univ_1176_eligible_researchers <- read_delim("~/Desktop/WoS Data from Jesper/univ-1176-elegible-researchers2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

eu_univ_1176_pub_vars <- read_delim("~/Desktop/WoS Data from Jesper/eu-univ-1176-pub-vars-left-join2.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)

eu_univ_1176_pubs <- read_delim("~/Desktop/WoS Data from Jesper/eu-univ-1176-pubs-left-join2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

eu_merged_indicator_citvar <- read_delim("~/Desktop/WoS Data from Jesper/eu-merged-indicator-citvar.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)

eu_merged_indicator_cit3yr <- read_delim("~/Desktop/WoS Data from Jesper/eu-merged-indicator-cit3yr.txt", "\t", escape_double = FALSE, trim_ws = TRUE)