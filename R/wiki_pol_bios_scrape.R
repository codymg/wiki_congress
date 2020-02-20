library(tidyverse)
library(rvest)
library(tidytext)
library(purrr)
library(purrrlyr)
library(tidyr)

#an example url is https://en.wikipedia.org/wiki/John_McCain

getpolitician <- function(politician) { #politician is a constructed Wikipedia URL
  
  politician_name <- politician %>% read_html() %>%
    html_node(xpath = "//h1[@id='firstHeading']") %>% #selecting header name
    html_text(trim = TRUE)
  
  
  vcard <- politician %>% read_html() %>% #obtaining biographical data
    html_nodes(".vcard") %>%
    html_text(trim = TRUE)
  
  url_dat <- politician #retaining url used to obtain data
  
  all_politician <- data.frame(politician_name = politician_name, #storing data into dataframe
                               vcard = vcard,
                               url = url_dat) 
  
}

WikiPoliticians <- function(url_dat) { #url_dat comes from the get_Politician function
  data <- data.frame()
  i = 1
  for(i in 1:length(url_dat)) {
    link <- paste0("https://en.wikipedia.org/wiki/", url_dat[i])
    all_politician <- getpolitician(link)
    data <- plyr::rbind.fill(data, all_politician)
    print(i)
  }
  data
}

#you can use the Wikipedia name "John_McCain" stored as url_dat as an example

url_dat <- "John_McCain"

df <- purrr::map(url_dat, purrr::possibly(WikiPoliticians, NULL)) %>%
purrr::compact() %>%
dplyr::bind_rows() %>%
distinct(politician_name, .keep_all = TRUE)

df

wiki_pol_raw <- df %>%
  mutate(wiki_name = str_remove_all(.$url, "https://en.wikipedia.org/wiki/")) %>%
  mutate(wiki_name = str_replace_all(.$wiki_name, "_", " "))

final_pol <- wiki_pol_raw %>%
  rename(politician = politician_name, bio = vcard) %>%
  separate(bio, c("bio", "personal_details"), sep = "\nPersonal details|Personal details") %>%
  mutate(Date = str_extract_all(personal_details, "\\(\\d{4}-\\d{2}-\\d{2}\\)")) %>%
  separate(Date, c("birthday", "death_date"), sep = ",") %>%
  mutate(age = stringi::stri_extract_first_regex(personal_details, "\\(age\\s+\\d{1,3}\\)|\\(aged\\s+\\d{1,3}\\)"),
         military_service = ifelse(str_detect(personal_details, "Military service|military service|Military Service") == TRUE, 1, 0),
         birthday = str_remove_all(birthday, pattern = 'c|"|\\(|\\)'),
         death_date = str_remove_all(death_date, pattern = 'c|"|\\(|\\)')) %>%
  mutate(age = str_extract_all(age, "\\d{1,3}", "")) %>%
  mutate(age = na_if(age, "character0")) %>%
  dplyr::select(politician, wiki_name, birthday, death_date, age, bio, military_service, personal_details)

final_pol