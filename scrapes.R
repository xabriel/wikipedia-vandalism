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
rm(contractions_html, contractions_table_nodes, contractions_table)

# scrape a list of english superlatives
superlatives_html <- rvest::read_html("https://www.easypacelearning.com/all-lessons/grammar/1436-comparative-superlative-adjectives-list-from-a-to-z")
superlatives_table_nodes <- superlatives_html %>% html_nodes("table")
superlatives_table <- superlatives_table_nodes[[1]] %>% html_table(header = TRUE)
superlatives_table %>%
  select(Superlative) %>%
  mutate(Superlative = str_trim(Superlative)) %>%
  separate_rows(Superlative, sep = "/") %>%
  arrange(Superlative) %>%
  pull(Superlative) %>%
  write_lines(file = "data/en/superlatives.txt")
rm(superlatives_html, superlatives_table_nodes, superlatives_table)

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
tibble(
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

# create a list of wikisyntax and allowed html tags
# source:
# https://en.wikipedia.org/wiki/Help:HTML_in_wikitext
# https://en.wikipedia.org/wiki/Help:Wikitext
# 
tibble(
  word  = c(
    # html pairs
    "b",
    "bdi",
    "del",
    "i",
    "ins",
    "u",
    "font",
    "big",
    "small",
    "sub",
    "sup",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "cite",
    "code",
    "em",
    "s",
    "strike",
    "strong",
    "tt",
    "var",
    "div",
    "center",
    "blockquote",
    "ol",
    "ul",
    "dl",
    "table",
    "caption",
    "pre",
    "ruby",
    "rb",
    "rp",
    "rt",
    "rtc",
    "p",
    "span",
    "abbr",
    "dfn",
    "kbd",
    "samp",
    "data",
    "time",
    "mark",
    # html singles
    "br",
    "wbr",
    "hr",
    "li",
    "dt",
    "dd",
    "meta",
    "link",
    # html nestable
    "table",
    "tr",
    "td",
    "th",
    "div",
    "blockquote",
    "ol",
    "ul",
    "li",
    "dl",
    "dt",
    "dd",
    "font",
    "big",
    "small",
    "sub",
    "sup",
    "span",
    "var",
    "kbd",
    "samp",
    "em",
    "strong",
    "q",
    "ruby",
    "bdo",
    # html global attr
    "class",
    "dir",
    "ltr",
    "rtl",
    "auto",
    "id",
    "lang",
    "style",
    # html5 microdata
    "itemid",
    "itemprop",
    "itemref",
    "itemscope",
    "itemtype",
    # parser extension tags
    "categorytree",
    "ce",
    "charinsert",
    "chem",
    "gallery",
    "graph",
    "hiero",
    "imagemap",
    "indicator",
    "inputbox",
    "langconvert",
    "mapframe",
    "maplink",
    "math",
    "nowiki",
    "poem",
    "pre",
    "ref",
    "references",
    "score",
    "section",
    "source",
    "syntaxhighlight",
    "templatedata",
    "templatestyles",
    "timeline",
    # wikitext
    "=",
    "==",
    "===",
    "====",
    "=====",
    "----",
    "__FORCETOC__",
    "__TOC__",
    "__NOTOC__",
    ":",
    "::",
    ":::",
    "::::",
    ":::::",
    "::::::",
    "Outdent",
    "*",
    "**",
    "***",
    "****",
    "#",
    "##",
    "###",
    "####"
    # TODO: much more, but let's try with this list
  )
) %>%
  distinct(word) %>%
  arrange(word) %>%
  pull(word) %>%
  write_lines(file = "data/global/wikisyntax.txt")
