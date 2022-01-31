##
# In this project we intend to build a vandalism detection system for Wikipedia edits.
##

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
# library(data.table)

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

# save here as pristine?

# define a function that does the diff
nativediff <- Vectorize(
  function(old, new) {
    d <- diffobj::diffChr(target = old,
                          current = new,
                          mode = "unified", # output in one line
                          context = 0, # just the actual additions, deletions
                          format = "raw", # plain text
                          word.diff = FALSE, #  TODO: false to make it faster, this may affect outcome!
                          pager = "off", # should not matter, but just in case
                          guides = FALSE, # don't modify diff with cues
                          ignore.white.space = FALSE)
    # paste(as.character(d), collapse = "\n")
    list(as.character(d))
  })

# let's calculate the diffs
start <- proc.time()

diffs <- edits %>%
  sample_frac(0.1) %>%
  select(oldrevisionid, newrevisionid, class) %>%
  left_join(
    revisions %>% select(revisionid, text) %>% rename(oldtext = text),
    by = c("oldrevisionid" = "revisionid")) %>%
  left_join(
    revisions %>% select(revisionid, text) %>% rename(newtext = text),
    by = c("newrevisionid" = "revisionid")) %>%
  mutate(diff = nativediff(oldtext, newtext)) %>%
  select(oldrevisionid, newrevisionid, diff) %>%
  mutate(additions =
           lapply(diff, function(d) { d[str_starts(d, ">") & !str_starts(d, "> new")] })) %>%
  mutate(deletions =
           lapply(diff, function(d) { d[str_starts(d, "<") & !str_starts(d, "< old")] }))

time <- proc.time() - start

# git diff --minimal --no-prefix a.txt b.txt
# 327585467     327607921
# FIXME: still need to test this out
gitdiff <- function(old, new) {
  system2(command = "git",
          args = c("diff", "--minimal", "--no-prefix", old, new),
          stdout = TRUE)
}

# pull random true positive:
edits %>% filter(class == "vandalism") %>% last()
revisions %>% filter(revisionid == tp) %>% pull(text)

# TODO:
# it looks like we need to calculate the diff to move forward
# we can do this locally by:
# 1) keeping full path around.
# 2) iterate over revisions, for each, calculate diff using git diff
# ( or maybe using https://github.com/brodieG/diffobj ?)
# 3) save it as part of edits

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