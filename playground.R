##
# In this project we intend to build a vandalism detection system for Wikipedia edits.
##

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")
if(!require(yardstick)) install.packages("yardstick", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(doParallel)
library(MLmetrics)
library(yardstick)

# load up helper functions
source("functions.R")

#
# SETUP
#

# We utilize "PAN Wikipedia Vandalism Corpus 2010": https://webis.de/data/pan-wvc-10
# download corpus if needed. note the zip file is 438 MBs, and it expands to 1.4 GBs.
if (!dir.exists("data/pan-wikipedia-vandalism-corpus-2010")) {
  dir.create("data/pan-wikipedia-vandalism-corpus-2010")
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
  list(
    revisionid = as.integer(str_remove(basename(path), ".txt")),
    revisionpath = path,
    revisiontext = read_file(path)
  )
}) %>% mutate(revisionsize = str_length(revisiontext))

rm(revisionPaths, parentPath)

# let's calculate the diffs
start <- proc.time()

# this takes ~10mins
diffs <- edits %>%
  select(editid, oldrevisionid, newrevisionid) %>%
  left_join(
    revisions %>%
      select(revisionid, revisionpath, revisionsize) %>%
      rename(oldrevisionpath = revisionpath, oldrevisionsize = revisionsize),
    by = c("oldrevisionid" = "revisionid")) %>%
  left_join(
    revisions %>%
      select(revisionid, revisionpath, revisionsize) %>%
      rename(newrevisionpath = revisionpath, newrevisionsize = revisionsize),
    by = c("newrevisionid" = "revisionid")) %>%
  mutate(diff = git_diff(oldrevisionpath, newrevisionpath)) %>%
  select(editid, oldrevisionid, newrevisionid, oldrevisionsize, newrevisionsize, diff) %>%
  mutate(additions =
           lapply(diff, function(d) {
             s = d[str_starts(d, fixed("+")) & !str_starts(d, fixed("+++"))]
             str_sub(s, start = 2)
             })) %>%
  mutate(deletions =
           lapply(diff, function(d) {
             s = d[str_starts(d, fixed("-")) & !str_starts(d, fixed("---"))]
             str_sub(s, start = 2)
             }))

time <- proc.time() - start
print(time)

rm(start, time)

edits <- edits %>%
  select(-diffurl) %>%
  left_join(diffs %>% select(-oldrevisionid, -newrevisionid),
            by = c("editid"))
  
rm(diffs, revisions)

#
# save here as "pristine-edits-with-diffs.RData".
#


#
# let's start calculating features
# 

# character level features:
character_features <- edits %>%
  select(editid, additions) %>%
  mutate(additions_as_string = map_chr(additions, str_c, collapse = "\n")) %>%
  # Ratio of upper case chars to lower case chars (all chars)
  mutate(
    upper_to_lower_ratio =
      (str_count(additions_as_string, "[A-Z]") + 1) / (str_count(additions_as_string, "[a-z]") + 1)
  ) %>%
  # Ratio of upper case chars to all chars
  mutate(
    upper_to_all_ratio =
      (str_count(additions_as_string, "[A-Z]") + 1) / (str_count(additions_as_string, ".") + 1)
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
  # achievable compression ratio of all chars
  # gzip chosen for its speed (https://cran.r-project.org/web/packages/brotli/vignettes/benchmarks.html)
  mutate(
    compression_ratio =
      (map_int(additions_as_string, function(s) { length(memCompress(s, type = "gzip")) }) + 1) /
      (map_int(additions_as_string, function(s) { length(as.raw(s)) }) + 1)
  ) %>%
  select(-additions, -additions_as_string)

# load word dictionaries
profanities <- read_lines(file = "data/en/profanities.txt")
pronouns <- read_lines(file = "data/en/pronouns.txt")
contractions <- read_lines(file = "data/en/contractions.txt")
superlatives <- read_lines(file = "data/en/superlatives.txt")
wikisyntax <- read_lines(file = "data/global/wikisyntax.txt")

# This tokenizer regex is wikisyntax aware.
# This helps as vandals do not typically use wikisyntax.
# matches:
# "----" section break
# indentation markers with ":", "*", "#" or "=".
# link delimiters with "{{", "}}", "[[" or "]]"
# any other regular word ( which would also include other wikisyntax )
wiki_regex <- "----|:{1,6}|\\*{1,4}|#{1,4}|={1,5}|\\{\\{|\\}\\}|\\[\\[|]]|\\w+"

word_features <- edits %>%
  select(editid, additions) %>%
  mutate(additions_as_string = map_chr(additions, str_c, collapse = "\n")) %>%
  mutate(
    word_list = map(str_match_all(additions_as_string, wiki_regex), as.vector),
    word_list_lower = map(word_list, str_to_lower),
  ) %>% select(editid, word_list_lower) %>%
  unnest_longer(col = word_list_lower, values_to = "word") %>%
  mutate(
    word = replace_na(word, ""),
    is_profanity = map_lgl(word, function(w) {
      bin_search(profanities, w) > 0
    }),
    is_pronoun = map_lgl(word, function(w) {
      bin_search(pronouns, w) > 0
    }),
    is_superlative = map_lgl(word, function(w) {
      bin_search(superlatives, w) > 0
    }),
    is_contraction = map_lgl(word, function(w) {
      bin_search(contractions, w) > 0
    }),
    is_wikisyntax = map_lgl(word, function(w) {
      bin_search(wikisyntax, w) > 0
    }),
    word_length = str_length(word)
    # TODO: add top-k vandal words
  ) %>% group_by(editid) %>%
  summarise(
    profanity_count = sum(is_profanity),
    pronoun_count = sum(is_pronoun),
    superlative_count = sum(is_superlative),
    contraction_count = sum(is_contraction),
    wikisyntax_count = sum(is_wikisyntax),
    longest_word = max(word_length)
  )

# edit comment features
comment_features <- edits %>%
  select(editid, editcomment) %>%
  mutate(
    has_comment = !editcomment == "null" | is.na(editcomment),
    comment_length = if_else(has_comment, str_length(editcomment), 0L),
    is_revert =
      str_starts(editcomment, fixed("Revert")) | # user revert
      str_starts(editcomment, fixed("revert")) | # user revert
      str_starts(editcomment, fixed("[[Help:Reverting|Reverted]] ")) | # user revert
      str_starts(editcomment, fixed("[[WP:RBK|Reverted]]")) | # bot revert
      str_starts(editcomment, fixed("[[WP:UNDO|Undid]]")), # bot undo ( aka late revert )
    is_bot = str_starts(editcomment, "\\[\\[WP:"),
    # tokenize each comment
    word_lists = map(str_match_all(editcomment, "\\w+"), as.vector),
    word_lists_lower = map(word_lists, str_to_lower),
    # for each token, lowercase, and check if there is a hit on profanity list
    profanity_lists = map(word_lists_lower,
                      function(word_list_lower) {
                        map_lgl(word_list_lower, function(word) { bin_search(profanities, word) > 0 } )
                      }),
    # OR all word checks. if any TRUE, comment has profanity
    has_profanity = map_lgl(profanity_lists, function(list) { reduce(list, `|`, .init = FALSE) })
  ) %>%
  select(-editcomment, -word_lists, -word_lists_lower, -profanity_lists)

size_features <- edits %>%
  select(editid, oldrevisionsize, newrevisionsize, additions, deletions) %>%
  mutate(
    size_delta = newrevisionsize - oldrevisionsize,
    size_ratio = (newrevisionsize + 1) / (oldrevisionsize + 1),
    num_additions = map_int(additions, length),
    num_deletions = map_int(deletions, length)
  ) %>%
  select(-oldrevisionsize, -newrevisionsize, -additions, -deletions)

ip_address_regex <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"

editor_features <- edits %>%
  select(editid, editor, edittime) %>%
  mutate(is_anonymous = str_detect(editor, ip_address_regex)) %>%
  # TODO: for anonymous, we can geolocate by id, and check local time
  select(editid, is_anonymous)

# prepare features as matrix to feed to algos
all_features <-
  character_features %>%
  left_join(word_features, by = "editid") %>%
  left_join(comment_features, by = "editid") %>%
  left_join(size_features, by = "editid") %>%
  left_join(editor_features, by = "editid") %>%
  select(-editid) %>%
  as.matrix()

golden_class <- edits %>% pull(class)

#
# split into train, test, and validation sets
#

# validation set will be 50% of source data, to be comparable to [Potthast 2010]
# Note that our classes are imbalanced:
edits %>% pull(class) %>% table()
# but createDataPartition takes care of stratifying splits properly.
set.seed(123, sample.kind="Rounding")
validation_index <- createDataPartition(y = edits %>% pull(class), times = 1, p = 0.5, list = FALSE)
validation <- all_features[validation_index, ]
validation_class <- golden_class[validation_index]
validation_edits <- edits[validation_index,]
train <- all_features[-validation_index, ]
train_class <- golden_class[-validation_index]

# check that we have good features
nearZeroVar(train, names = T)
# this yields:
# [1] "profanity_count"   "pronoun_count"     "superlative_count" "contraction_count" "is_bot"
# [6] "has_profanity"
# BUT, note that our classes are very skewed, with vandalism being only ~7%.

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(
  x = train,
  y = train_class,
  method = "knn",
  tuneGrid = data.frame(k = c(3, 5, 7)),
  trControl = control
)
ggplot(train_knn)


y_hat_knn <- predict(train_knn, validation, type="raw")
cm <- confusionMatrix(y_hat_knn, validation_class)
cm
varImp(train_knn)
# knn is ok, with 140 false positives.



library(doParallel)
cores <- coalesce(detectCores() - 1, 1)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# use summaryFunction = prSummary to fit based on PR-AUC
# FIXME: prSummary failed, try twoClassSummary ?
# twoClass failed as well, check for NA's!!
# 
# We dont seem to have NAs....
# 
control <- trainControl(method = "cv",
                        number = 10,
                        summaryFunction = prSummary,
                        allowParallel = TRUE)
grid <- data.frame(mtry = c(1, 2, 3, 4, 5, 10, 25, 50, 100))

set.seed(123, sample.kind="Rounding")
train_rf <-  train(
  x = train,
  y = train_class,
  method = "rf",
  ntree = 1000, # try with 1000
  trControl = control,
  tuneGrid = grid,
  metric = "F", # use F1 since PR-AUC is returning NAs :(
  maximize = TRUE
)

stopCluster(cl)

ggplot(train_rf)


y_hat_rf <- predict(train_rf, validation, type="raw")
cm_rf <- confusionMatrix(y_hat_rf, validation_class)
cm_rf
varImp(train_rf)

y_hat_rf_prob <- predict(train_rf, validation, type="prob")

# PR-AUC of 0.5795794 gets us second only to [MolaVelasco]'s 0.66522.
prauc <- MLmetrics::PRAUC(y_hat_rf_prob["vandalism"], if_else(validation_class == "vandalism", 1L, 0L))
# ROC-AUC of 0.9164979 gets us also secon only to [MolaVelasco]'s 0.92236
rocauc <- MLmetrics::AUC(y_hat_rf_prob["vandalism"], if_else(validation_class == "vandalism", 1L, 0L))

results_rf <- tibble(
  index = 1:length(validation_class),
  truth = validation_class,
  vandalism = y_hat_rf_prob %>% pull(vandalism),
  regular = y_hat_rf_prob %>% pull(regular),
  predicted = y_hat_rf
)

# ROC AUC curve
results_rf %>%
  roc_curve(truth, vandalism) %>%
  autoplot()

# yardstick says PR-AUC is 0.625.
results_rf %>%
  pr_auc(truth, vandalism)

results_rf %>%
  pr_curve(truth, vandalism) %>%
  autoplot()

# let's take a look at the FP and FN, see if we can get insights on why we are failing
false_negatives_rf <- results_rf %>%
  filter(truth != predicted) %>%
  filter(truth == "vandalism")

false_positives_rf <- results_rf %>%
  filter(truth != predicted) %>%
  filter(truth == "regular")

View(false_positives_rf)

View(validation_edits[false_positives_rf$index,])



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
  # TODO: fix this up and include it in word_features
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
revisions %>% filter(revisionid == tp) %>% pull(revisiontext)




# maybe use a text corpus package like tm?


# quickly running OOM with lm and rf....
# WE NEED features, not raw data!
lm <- caret::train(
  form = class ~ editor + editcomment,
  data = play,
  method = "lm"
)



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