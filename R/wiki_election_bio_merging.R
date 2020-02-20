library(tidyverse)
library(readr)

######################################################
######################################################
#Merging Wiki Biographical and Election Data
######################################################
######################################################

house <- read_csv(url("https://raw.githubusercontent.com/codymg/wiki_congress/master/data/wiki_elections_house.csv?token=AOS7CTQQYCJ4KZUYA3WISNK6JYHO6")) %>%
  select(-X1)

senate <- read_csv(url("https://raw.githubusercontent.com/codymg/wiki_congress/master/data/wiki_elections_senate.csv?token=AOS7CTW4K4TP7XTE4UJBUKS6JYIBQ")) %>%
  select(-X1)

final_pol <- read_csv(url("https://raw.githubusercontent.com/codymg/wiki_congress/master/data/pol_bios.csv?token=AOS7CTRSA2HCFOWH5CK5AEK6JYIES")) %>%
  select(-X1)

final_opp <- read_csv(url("https://raw.githubusercontent.com/codymg/wiki_congress/master/data/opp_bios.csv?token=AOS7CTTCFDC2HYN4BRY6S7C6JYJTC")) %>%
  select(-X1)

wiki_df <- bind_rows(house, senate)

wiki_data <- left_join(wiki_df, final_pol, by = c("Politician" = "wiki_name"))

wiki_data <- left_join(wiki_data, final_opp, by = c("Opp" = "Opp_wiki_name")) %>%
  mutate(Opp = str_replace_all(.$Opp, "\n", "")) %>%
  mutate(Opp = trimws(.$Opp, "both"))

