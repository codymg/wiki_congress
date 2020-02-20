library(rvest)
library(tidyverse)
library(SnowballC)
library(tidytext)
library(stopwords)
library(textstem)
library(corpus)
library(ggwordcloud)
library(ggthemes)


###############################################################

#House Elections

###############################################################

url_home <- paste0("https://en.wikipedia.org/wiki/")

election <- paste0("_United_States_House_of_Representatives_elections")

#creates list of all tables in the page (each state has its own list of district and returns are in there).

wiki_scrape <- function(year){
  wiki_dat <- lapply(paste0(paste0(url_home, year), election),
                     function(url){ 
                       url %>% read_html() %>% 
                         html_table(fill = TRUE)
                     }) 
  holding_bin <- rep(list(NA), length(wiki_dat[[1]]))
  
  for (i in 1:length(holding_bin)){
    data = data.frame(wiki_dat[[1]][[i]])
    data$year = year
    data2 <- tryCatch(data %>% 
                        setNames(make.names(names(.), unique = TRUE)) %>% #gives unique names
                        select(District, Candidates), 
                      error = function(e)
                        paste("ERROR:", conditionMessage(e), "\n"))
    holding_bin[[i]] <- data2
  }
  final_df <- do.call(rbind, holding_bin) %>% unique() %>% 
    mutate(year = year)
}

dat_2018 <- wiki_scrape(2018)

final_df <- dat_2018 %>% 
  filter(!is.na(Candidates)) %>%
  filter(District != "ERROR: object 'Candidates' not found \n") %>%
  filter(District != "ERROR: object 'District' not found \n")

#bindrows and clean data frame

test <- final_df %>%
  separate(., Candidates, into = c("Politician", "Opp"), sep = "%", extra = "merge") %>%
  mutate(Politician_Vote_Share = str_extract(Politician, pattern = "\\s\\d+.\\d+|d{2}"),
         Politician_Party = str_extract(Politician, pattern = "\\(.+\\)"),
         Opp_Vote_Share = str_extract(Opp, pattern = "\\s\\d+.\\d+|d{2}|\\s\\d+.\\d+(?=%)"),
         Unopposed = ifelse(str_detect(string = Politician, pattern = "\\s+Unopposed|unopposed|Unopposed") == TRUE, 1, 0),
         Wiki_Missing = ifelse(str_detect(string = Politician, pattern = "\\[Data unknown/missing.\\]|\\[data unknown/missing.\\]") == TRUE, 1, 0)) %>% #for two rounds it records round 2
  mutate(Opp = str_replace_all(Opp, pattern = "(%).*", "%"),
         Opp_Party = str_extract(Opp, pattern = "\\(.+\\)"))

test <- test %>% 
  mutate(Opp = str_remove_all(.$Opp, pattern = "\\[.+\\]")) %>%
  separate(., Opp, into = c("Opp", "Others"), sep = "%|\\d+", extra = "merge") %>%
  select(-Others)


test$State <- str_replace_all(string = test$District, pattern = "\\s+\\d+|\\s+at-large|\\s+At-large", repl = "") #eliminates districts

test$District <- str_extract(test$District, pattern = "\\s+\\d+|\\s+at-large") #only keeps district numbers

test$Politician <- str_replace_all(string = test$Politician, pattern = "\\s+\\(\\w+\\)|Y\\s+|\u221a\\s+|\\s+\\(.+\\)|\\s+Unopposed|unopposed|Unopposed", repl = "") #replaces party and Y and any information in ()

test$Opp <- str_replace_all(string = test$Opp, pattern = "\\s+\\(\\w+\\)|\\s+\\(.+\\)", repl = "") #replaces party or any information in ()

test$Politician <- str_replace_all(string = test$Politician, pattern = "\\[\\d+\\]|\\[\\d+\\].+|\\[\\w+\\]|\\[\\w+\\].+|\\[citation needed\\]", repl = "") #replaces footnotes of any kind

test$Opp <- str_replace_all(string = test$Opp, pattern = "\\[\\d+\\]|\\[\\d+\\].+|\\[\\w+\\]|\\[\\w+\\].+", repl = "") #replaces footnotes of any kind

test$Politician <- str_replace_all(string = test$Politician, pattern = "\\s\\d+.\\d+%|\\s\\d+.\\d+", repl = "") #replaces vote shares

test$Opp <- str_replace_all(string = test$Opp, pattern = "\\s\\d+.\\d+%|\\s\\d+.\\d+", repl = "") #replaces vote shares

test$Politician_Vote_Share <- as.numeric(test$Politician_Vote_Share) #changing to numeric

test$Opp_Vote_Share <- as.numeric(test$Opp_Vote_Share) #changing to numeric

test$Politician_Party <- str_remove_all(test$Politician_Party, pattern = "\\(|\\)")

test$Politician_Party <- str_replace_all(test$Politician_Party, pattern = "Republican|R", replacement = "200")

test$Politician_Party <- str_replace_all(test$Politician_Party, pattern = "Democratic|D", replacement = "100")

test$Opp_Party <- str_remove_all(test$Opp_Party, pattern = "\\(|\\)")

test$Opp_Party <- str_replace_all(test$Opp_Party, pattern = "Republican|R", replacement = "200")

test$Opp_Party <- str_replace_all(test$Opp_Party, pattern = "Democratic|D", replacement = "100")

test <- test %>%
  select(State, District, Politician, Politician_Party, Politician_Vote_Share, Unopposed, Opp, Opp_Party, Opp_Vote_Share, year, Wiki_Missing) %>% #re-ordering variables for easier comprehension
  mutate(Close_Election = ifelse(Politician_Vote_Share - Opp_Vote_Share <= 2, 1, 0)) %>% #creating binary variable to identify close elections (less than 2 percent difference)
  group_by(State, District) %>%
  arrange(desc(State, District))  #try to put in opposite order  

#fixing two odd states (X,)

test$State[1] <- "Maine"

test$District[1] <- "1"

test$State[2] <- "Maine"

test$District[2] <- "2"

test2 <- test %>% 
  ungroup() %>%
  distinct(Politician_Party, .keep_all = TRUE)

table(is.na(test$Politician_Vote_Share))

problems <- test %>%
  filter(is.na(Politician_Vote_Share), Unopposed == 0)


problems <- problems %>%
  mutate(P_Votes = stringi::stri_extract_first_regex(Politician, "\\d{2}"),
         O_Votes = stringi::stri_extract_first_regex(Opp, "\\d{2}"))

problems$P_Votes <- ifelse(problems$P_Votes == "character(0)", NA, as.numeric(problems$P_Votes))

problems$O_Votes <- ifelse(problems$O_Votes == "character(0)", NA, as.numeric(problems$O_Votes))

table(is.na(problems$O_Votes))

problems <- problems %>%
  select(-Politician_Vote_Share, -Opp_Vote_Share) %>%
  mutate(Close_Election = ifelse(P_Votes - O_Votes <= 2, 1, 0))

problems <- problems %>%
  rename(Politician_Vote_Share = P_Votes, Opp_Vote_Share = O_Votes)

dat <- anti_join(test, problems, by = c("State" = "State", "District" = "District", "year" = "year", "Politician" = "Politician")) %>%
  select(year, State, District, Politician, Politician_Party, Politician_Vote_Share, Opp, Opp_Party, Opp_Vote_Share, Unopposed, Wiki_Missing, Close_Election)

dat$Politician <- str_replace_all(string = dat$Politician, pattern = "\\d{2}|%|\\[Data unknown/missing.\\]|\\[data unknown/missing.\\]|\n", repl = "") %>%
  trimws(., "both") #replaces vote shares, \n, and white spaces

dat$Opp <- str_replace_all(string = dat$Opp, pattern = "\\d{2}|%|\n", repl = "") %>%
  trimws(., "both") #replaces vote shares, \n, and white spaces

dat$District <- str_remove_all(string = dat$District, pattern = "\\s+")

dat$Leg_Body <- 2

head(dat)
