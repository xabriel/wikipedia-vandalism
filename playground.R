##
# In this project we intend to build a vandalism detection system for Wikipedia edits.
##

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

#
# SETUP
#

# We utilize "PAN Wikipedia Vandalism Corpus 2010": https://webis.de/data/pan-wvc-10
# download corpus if needed. note the zip file is 438 MBs, and it expands to 1.4 GBs.
if (!dir.exists("data")) {
  dir.create("data")
  corpusFileName <- "data/pan-wikipedia-vandalism-corpus-2010.zip"
  corpusURI <-
    "https://zenodo.org/record/3341488/files/pan-wikipedia-vandalism-corpus-2010.zip?download=1"

  download.file(corpusURI, corpusFileName)
  unzip(corpusFileName, exdir = "data")

  file.remove(corpusFileName)
  rm(corpusFileName, corpusURI)
}

# read edits, merge with golden classificaton of vandalism / regular
parentPath <- "data/pan-wikipedia-vandalism-corpus-2010/"

edits <- read_csv(file = paste(parentPath, "edits.csv", sep = ""),
                  quote = "\"",
                  col_names = TRUE,
                  col_types = cols(
                    editid = col_integer(),
                    editor = col_character(),
                    oldrevisionid = col_integer(),
                    newrevisionid = col_integer(),
                    diffurl = col_character(),
                    edittime = col_datetime(format = ""),
                    editcomment = col_character(),
                    articleid = col_integer(),
                    articletitle = col_character()
                  ))

annotations <- read_csv(file = paste(parentPath, "gold-annotations.csv", sep = ""),
                        quote = "\"",
                        col_names = TRUE,
                        col_types = cols(
                          editid = col_integer(),
                          class = col_character(),
                          annotators = col_integer(),
                          totalannotators = col_integer()
                        )) %>%
  mutate(class = factor(class, levels = c("vandalism", "regular"))) # make sure vandalism == 1

edits <- edits %>%
  left_join(annotations %>% select(editid, class), by = "editid")

rm(annotations)

revisionPaths <- list.files(path = paste(parentPath, "article-revisions", sep = ""),
                            full.names = TRUE,
                            recursive = TRUE)

revisions <- map_dfr(revisionPaths, function(path) {
  list(revisionid = as.integer(str_remove(basename(path), ".txt")),
       revisionpath = path,
       text = read_file(path))
})

rm(revisionPaths, parentPath)

# git diff system call with all the arguments needed to make the output easily parsable
# the key arg is --word-diff=porcelain, which separates edits into additions (+) and deletions (-)
gitdiff <- Vectorize(
  function(old, new) {
    system2(command = "git",
            args = c("diff", "--minimal", "--no-prefix", "--no-index", "--word-diff=porcelain",
                     "--unified=0", old, new),
            stdout = TRUE)
  })

# let's calculate the diffs
start <- proc.time()

# this takes ~10mins
diffs <- edits %>%
  select(oldrevisionid, newrevisionid, class) %>%
  left_join(
    revisions %>%
      select(revisionid, revisionpath) %>%
      rename(oldrevisionpath = revisionpath),
    by = c("oldrevisionid" = "revisionid")) %>%
  left_join(
    revisions %>%
      select(revisionid, revisionpath) %>%
      rename(newrevisionpath = revisionpath),
    by = c("newrevisionid" = "revisionid")) %>%
  mutate(diff = gitdiff(oldrevisionpath, newrevisionpath)) %>%
  select(oldrevisionid, newrevisionid, diff) %>%
  mutate(additions =
           lapply(diff, function(d) {
             s = d[str_starts(d, fixed("+")) & !str_starts(d, fixed("+++"))]
             str_sub(s, start = 2)
             })) %>%
  mutate(deletions =
           lapply(diff, function(d) {
             s = d[str_starts(d, fixed("-")) & !str_starts(d, fixed("---"))]
             str_sub(s, start = 2)
             })) %>%
  select(oldrevisionid, newrevisionid, additions, deletions)

time <- proc.time() - start

rm(revisions, start, time)

edits <- edits %>%
  left_join(diffs, by = c("oldrevisionid", "newrevisionid")) %>%
  select(-diffurl)

rm(diffs, gitdiff)

#
# split into train, test, and validation sets
#

# validation set will be 50% of source data, to be comparable to [Potthast 2010]
# Note that our classes are imbalanced:
edits %>% pull(class) %>% table()
# but createDataPartition takes care of stratifying splits properly.
set.seed(123, sample.kind="Rounding")
validation_index <- createDataPartition(y = edits %>% pull(class), times = 1, p = 0.5, list = FALSE)
validation <- edits[validation_index, ]
remaining <- edits[-validation_index, ]

# test set will be 10% of the remaining data
set.seed(123, sample.kind="Rounding")
test_index <- createDataPartition(y = remaining %>% pull(class), times = 1, p = 0.1, list = FALSE)
test <- remaining[test_index, ]
train <- remaining[-test_index, ]

rm(remaining, validation_index, test_index)
#
# save here as pristine.
#

# given a list of strings, concatenate them and find the number of
# unique chars.
num_unique_chars <- function(s) {
  chars <- str_split(s, pattern = "")
  length(unique(chars[[1]]))
}


edits[1:2,] %>%
  select(editid, additions) %>%
  # Ratio of upper case chars to lower case chars (all chars).
  mutate(upper_case_ratio = upper_case_ratio(additions))
#
# let's start calculating features
# 

# character level features:
edits[1:10,] %>%
  select(editid, additions) %>%
  mutate(additions_as_string = map_chr(additions, str_c, collapse = "\n")) %>%
  # Ratio of upper case chars to lower case chars (all chars)
  mutate(
    upper_case_ratio =
      (str_count(additions_as_string, "[A-Z]") + 1) / (str_count(additions_as_string, "[a-z]") + 1)
  ) %>%
  # Ratio of digits to all letters.
  mutate(
    digits_ratio =
      (str_count(additions_as_string, "\\d") + 1) / (str_count(additions_as_string, ".") + 1)
  ) %>%
  # Ratio of special chars to all chars.
  mutate(
    special_chars_ratio =
      (str_count(additions_as_string, "[^A-Za-z0-9]") + 1) / (str_count(additions_as_string, ".") + 1)
  ) %>%
  # Length of all inserted lines to the (1 / number of different chars)
  mutate(
    char_diversity =
      str_length(additions_as_string) ^ (1 / num_unique_chars(additions_as_string))
  ) %>%
  # longest word
  # FIXME: this still fails
  mutate(
    all_words = map_chr(additions_as_string, str_extract_all, "\\w+"),
    all_word_lengths = map(all_words, str_length),
    max_word_length = map_int(all_word_lengths, max)
  ) %>%
  select(-additions, -additions_as_string, -all_words, -all_word_lengths)

# figure out most common word additions on vandalism:

common_regular_words <- train %>%
  mutate(cat_additions = paste(additions, sep = " ")) %>%
  select(editid, class, cat_additions) %>%
  unnest_tokens(
    output = "word",
    input = "cat_additions",
    drop = TRUE,
    token = "words",
    to_lower = TRUE,
    format = "text"
  ) %>%
  anti_join(stop_words) %>%
  # no http words, no wiki words
  filter(!word %in% c("ref", "http", "span", "br", "align", "cite", "style", "center", "url", "character", "text", "special:contributions", "title")) %>%
  # no numbers
  filter(!str_detect(word, "^\\d+$")) %>%
  group_by(class, word) %>%
  summarise(n = n()) %>%
  filter(class == "regular") %>%
  arrange(desc(n))

common_vandalism_words <- train %>%
  mutate(cat_additions = paste(additions, sep = " ")) %>%
  select(editid, class, cat_additions) %>%
  unnest_tokens(
    output = "word",
    input = "cat_additions",
    drop = TRUE,
    token = "words",
    to_lower = TRUE,
    format = "text"
  ) %>%
  anti_join(stop_words) %>%
  # no http words, no wiki words
  filter(!word %in% c("ref", "http", "span", "br", "align", "cite", "style", "center", "url", "character", "text", "special:contributions", "title")) %>%
  # no numbers
  filter(!str_detect(word, "^\\d+$")) %>%
  group_by(class, word) %>%
  summarise(n = n()) %>%
  filter(class == "vandalism") %>%
  arrange(desc(n)) %>%
  slice_head(n = 100)

library(tidytext)  


# pull random true positive:
edits %>% filter(class == "vandalism") %>% last()
revisions %>% filter(revisionid == tp) %>% pull(text)


# playground for diff:
install.packages("diffobj")
first <- ""
second <- "abcdefghijk lmno"





# maybe use a text corpus package like tm?


# quickly running OOM with lm and rf....
# WE NEED features, not raw data!
lm <- caret::train(
  form = class ~ editor + editcomment,
  data = play,
  method = "lm"
)


ip_address_regex <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"
# of vandalism, how many are anonymous?
# FALSE  TRUE 
# 313  2081
edits %>%
  # filter(class == "vandalism") %>%
  mutate(is_anonymous = str_detect(editor, ip_address_regex)) %>%
  select(editor, is_anonymous) %>% pull(is_anonymous) %>% table()

# of vandalism, how many have no comment?
# FALSE  TRUE 
#  1530   864 
edits %>%
  filter(class == "vandalism") %>%
  mutate(is_comment_null = editcomment == "null") %>%
  select(editcomment, is_comment_null) %>%
  pull(is_comment_null) %>% table()

rm(first, second, parentPath, revisionCount, ip_address_regex,d)  

# whenever we get the diffs, let's try:
# naive bayes (used in spam filtering)