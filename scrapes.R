#
# Useful scrapes to get word lists
#
if(!require(tidyverse)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(rvest)


# scrape a list of english contractions from wikipedia
contractions_html <- rvest::read_html("https://en.wikipedia.org/w/index.php?title=Wikipedia:List_of_English_contractions&oldid=1069197848")
contractions_table_nodes <- contractions_html %>% html_nodes("table")
contractions_table <- contractions_table_nodes[[2]] %>% html_table()
contractions_table %>%
  select(Contraction) %>%
  mutate(Contraction = str_replace(Contraction, fixed(" (informal)"), "")) %>%
  mutate(Contraction = str_replace(Contraction, fixed(" (formal)"), "")) %>%
  mutate(Contraction = str_trim(Contraction)) %>%
  arrange(Contraction) %>%
  pull(Contraction) %>%
  write_lines(file = "data/en/contractions.txt")


# create a list of profanities
# source: https://github.com/coffee-and-fun/google-profanity-words
read_csv(
  file = "https://raw.githubusercontent.com/coffee-and-fun/google-profanity-words/b43e903ae2ee14b4101c639426a80e346641b936/data/list.txt",
  col_names = c("word"),
  col_types = cols(word = col_character())
) %>%
  arrange(word) %>%
  pull(word) %>%
  write_lines(file = "data/en/profanities.txt")

# create a list of 1st and 2nd person pronouns
# source: https://www.ef.edu/english-resources/english-grammar/pronouns/
pronouns <- tibble(
  word  = c(
    "i",
    "me",
    "my",
    "mine",
    "myself",
    "you",
    "your",
    "yours",
    "yourself",
    "yourselves",
    "we",
    "us",
    "our",
    "ours",
    "ourselves"
  )
) %>%
  arrange(word) %>%
  pull(word) %>%
  write_lines(file = "data/en/pronouns.txt")
