library(dplyr)
library(tidyverse)
library(rvest)
library(tidyverse)
library(purrr)
library(purrrlyr)
library(janitor)
library(tidyr)

###########################################################################################
###########################################################################################
#Isreali Knesset Members
###########################################################################################
###########################################################################################

url_home <- paste0("https://en.wikipedia.org/wiki/List_of_members_of_the_")

knesset <- paste0("_Knesset")

knesset_numbers <- c("first", "second", "third",
                     "fourth", "fifth", "sixth",
                     "seventh", "eighth", "ninth",
                     "tenth", "eleventh", "twelfth",
                     "thirteenth", "fourteenth", "fifteenth",
                     "sixteenth", "seventeenth", "eighteenth",
                     "nineteenth", "twentieth", "twenty-first",
                     "twenty-second")

knesset_numbers <- as.character(knesset_numbers)

#creates list of all tables in the page (each state has its own list of district and returns are in there).
    

wiki <- lapply(paste0(paste0(url_home, knesset_numbers[1:22]), knesset),
                   function(url){
                     tryCatch(
                       url %>% read_html() %>% 
                         html_table(fill = TRUE) %>%
                         bind_rows(),
                       error = function(e)
                         as.list(paste("ERROR:", conditionMessage(e), "\n")))
                   })


df <- lapply(wiki, data.frame, stringsAsFactors = FALSE) %>%
  rbindlist(., use.names = TRUE, fill = TRUE) %>% 
  dplyr::select(.,-contains("ERROR")) %>%
  dplyr::select(.,-contains("NA.")) %>%
  dplyr::filter(!is.na(Party)) %>%
  dplyr::select_if(names(.) %in% c('Party', 'Member', 'Name', 'Date', 'Replacement', 'MK', 'Replacing', 'Replaced', 'Notes')) %>%
  mutate(Party = str_remove_all(.$Party, "\\(\\d+\\)|\\[[^\\]]*\\]"),
         Notes = str_remove_all(.$Notes, "\\(\\d+\\)|\\[[^\\]]*\\]"))


write_rds(df, "israeli_knesset_members.rds")

write_csv(df, "israeli_knesset_members.csv")


###########################################################################################
###########################################################################################
#Isreali Knesset Member Bios
###########################################################################################
###########################################################################################


getPolitician <- function(politician) {
  
  politician_name <- politician %>% read_html() %>%
    html_node(xpath = "//h1[@id='firstHeading']") %>%
    html_text(trim=TRUE)
  
  
  vcard <- politician %>% read_html() %>%
    html_nodes(".vcard") %>%
    html_text(trim = TRUE)
  
  url_dat <- politician 
  
  all_politician <- data.frame(politician_name = politician_name,
                               vcard = vcard,
                               url = url_dat)
  
}

WikiPoliticians <- function(url_dat) {
  data <- data.frame()
  i = 1
  for(i in 1:length(url_dat)) {
    link <- paste0("https://en.wikipedia.org/wiki/", url_dat[i])
    all_politician <- getPolitician(link)
    data <- plyr::rbind.fill(data, all_politician)
    print(i)
  }
  data
}

names(df)

df <- readRDS("~/israeli_knesset_members.rds")

dat <- df %>% mutate(Member = str_replace_all(.$Member, "\\s", "_"),
                     MK = str_replace_all(.$MK, "\\s", "_"),
                     Replaced = str_replace_all(.$Replaced, "\\s", "_"),
                     Name = str_replace_all(.$Name, "\\s", "_"),
                     Replacement = str_replace_all(.$Replacement, "\\s", "_"),
                     Replacing = str_replace_all(.$Replacing, "\\s", "_"))

members <- dat %>%
  dplyr::select(Member) %>%
  distinct(.) %>%
  rename(politician = Member)

mks <- dat %>%
  dplyr::select(MK) %>%
  distinct(.)%>%
  rename(politician = MK)

names <- dat %>%
  dplyr::select(Name) %>%
  distinct(.)%>%
  rename(politician = Name)

replacement <- dat %>%
  dplyr::select(Replacement) %>%
  distinct(.)%>%
  rename(politician = Replacement)

url_dat <- members %>%
  bind_rows(mks, names, replacement)

dta <- purrr::map(url_dat$politician, purrr::possibly(WikiPoliticians, NULL)) %>%
  purrr::compact() %>%
  dplyr::bind_rows() %>%
  distinct(politician_name, .keep_all = TRUE)

write_rds(dta, "knesset_bios_raw.rds")


dta <- dta %>% 
  mutate(wiki_name = str_remove_all(.$url, "https://en.wikipedia.org/wiki/")) %>%
  mutate(wiki_name = str_replace_all(.$wiki_name, "_", " "))

final_df <- dta %>%
  rename(politician = politician_name, bio = vcard) %>%
  mutate(Date = str_extract_all(bio, "\\(\\d{4}-\\d{2}-\\d{2}\\)")) %>%
  separate(Date, c("birthday", "death_date"), sep = ",") %>%
  mutate(age = stringi::stri_extract_first_regex(bio, "\\(age\\s+\\d{1,3}\\)|\\(aged\\s+\\d{1,3}\\)"),
         military_service = ifelse(str_detect(bio, "Military service|military service|Military Service") == TRUE, 1, 0),
         birthday = str_remove_all(birthday, pattern = 'c|"|\\(|\\)'),
         death_date = str_remove_all(death_date, pattern = 'c|"|\\(|\\)')) %>%
  mutate(age = str_extract_all(age, "\\d{1,3}", "")) %>%
  dplyr::select(politician, wiki_name, birthday, death_date, age, bio, military_service) %>%
  mutate_if(is.character, list(~na_if(., "harater0")))


write_rds(final_df, "wiki_knesset_bios.rds")

write.csv(final_df, "wiki_knesset_bios.csv")

