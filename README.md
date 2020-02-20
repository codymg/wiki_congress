# wiki_congress

This repository contains data on U.S. Congressional Elections from 1918-2018. It also includes biographical data on U.S. politicians in both the U.S. House and Senate. While not as complete, there is similar biographical information on second-place election candidates in U.S. Congressional races. All data found in this repository was obtained from Wikipedia. This repository also contains the code used to obtain both the available election data and biographical data from Wikipedia.

## Getting Started

The following instructions will provide you with a basic understanding of how some of the data was collected and should allow you to collect similar biographical information for a wide variety of individuals that have Wikipedia pages.

### Packages

Below are the following R packages that will be required to collect the data

```
library(tidyverse)
library(rvest)
library(tidytext)
library(purrr)
library(purrrlyr)
library(tidyr)
```

## Data Collection

This code uses mostly the `dplyr` and `rvest` packages to scrap the desired biographical data

### Identifying Individuals and Scraping Data

Firstly, we need a list of URLs from Wikipedia that will be used to direct us to the appropriate URLs on Wikipedia. In this case, the list of names used for this data collection exercise was created from the U.S. Congressional Election data available on Wikipedia which can be found in the wiki_election_data file. The following code obtains the Wikipedia header name, the politician's biographical data on wikipedia, and finally URL that you provide (which may be different than the header name). While it seems slightly redundant, for merging purposes, having both the header name and name used in the URL is important.

```
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
```

The following function is used to store all the data into a dataframe that is obtained by the *get_politician* function above.

```
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
```

Now that we have functions that get the requested name and biographical infromation from Wikipedia and store it in a data frame we use a combination of `purrr` and `dplyr` to send the request(s) to Wikipedia and then store the obtained raw data into a dataframe.

```
#you can use the Wikipedia name "John_McCain" stored as url_dat as an example

url_dat <- "John_McCain"

df <- purrr::map(url_dat, purrr::possibly(WikiPoliticians, NULL)) %>%
  purrr::compact() %>%
  dplyr::bind_rows() %>%
  distinct(politician_name, .keep_all = TRUE)
  
df
```

### Cleaning Biographical Data

Below is an exmaple of how such biographical information can be cleaned and prepared for analysis. 

```
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
```

## Authors

* **Cody Giddings**

## Public domain

The project is in the public domain within the United States, and copyright and related rights in the work worldwide are waived through the [CC0 1.0 Universal public domain dedication][CC0].

All contributions to this project will be released under the CC0 dedication. By submitting a pull request, you are agreeing to comply with this waiver of copyright interest.

[CC0]: http://creativecommons.org/publicdomain/zero/1.0/
