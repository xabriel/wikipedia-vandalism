##
# In this project we intend to build a binary vandalism classifierfor Wikipedia edits.
##

#
# Dependencies
#
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(multidplyr)) install.packages("multidplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")
if(!require(yardstick)) install.packages("yardstick", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(multidplyr)
library(caret)
library(doParallel)
library(MLmetrics)
library(yardstick)
library(tidytext)
library(naivebayes)
library(nnet)

# load up helper functions
source("functions.R")

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

#
# Load data
# Below we read both edits.csv and gold-annotations.csv and join them.
#
parentPath <- "data/pan-wikipedia-vandalism-corpus-2010/"

edits <- read_csv(
  file = paste(parentPath, "edits.csv", sep = ""),
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
  )
)

annotations <-
  read_csv(
    file = paste(parentPath, "gold-annotations.csv", sep = ""),
    quote = "\"",
    col_names = TRUE,
    col_types = cols(
      editid = col_integer(),
      class = col_character(),
      annotators = col_integer(),
      totalannotators = col_integer()
    )
  ) %>%
  mutate(class = factor(class, levels = c("vandalism", "regular"))) # make sure vandalism == 1

edits <- edits %>%
  left_join(annotations %>% select(editid, class), by = "editid")

rm(annotations)

revisionPaths <-
  list.files(
    path = paste(parentPath, "article-revisions", sep = ""),
    full.names = TRUE,
    recursive = TRUE
  )

revisions <- map_dfr(revisionPaths, function(path) {
  list(
    revisionid = as.integer(str_remove(basename(path), ".txt")),
    revisionpath = path,
    revisionsize = str_length(read_file(path))
  )
})

rm(revisionPaths, parentPath)

# use multidplyr to parallelize dplyr actions
cores <- coalesce(detectCores() - 1, 1)
cluster <- multidplyr::new_cluster(cores)
multidplyr::cluster_copy(cluster, 'git_diff')
multidplyr::cluster_library(cluster, 'tidyverse')
edits <- edits %>% partition(cluster)

# let's calculate the diffs
start <- proc.time()

# this takes ~10mins serial, ~2mins with multidplyr and 7 cores
diffs <- edits %>%
  select(editid, oldrevisionid, newrevisionid) %>%
  left_join(
    revisions %>%
      select(revisionid, revisionpath, revisionsize) %>%
      rename(oldrevisionpath = revisionpath, oldrevisionsize = revisionsize),
    by = c("oldrevisionid" = "revisionid"),
    copy = TRUE
  ) %>%
  left_join(
    revisions %>%
      select(revisionid, revisionpath, revisionsize) %>%
      rename(newrevisionpath = revisionpath, newrevisionsize = revisionsize),
    by = c("newrevisionid" = "revisionid"),
    copy = TRUE
  ) %>%
  mutate(diff = git_diff(oldrevisionpath, newrevisionpath)) %>%
  select(editid,
         oldrevisionid,
         newrevisionid,
         oldrevisionsize,
         newrevisionsize,
         diff) %>%
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
      (map_int(additions_as_string, function(s) { length(as.raw(s)) }) + 1) /
      (map_int(additions_as_string, function(s) { length(memCompress(s, type = "gzip")) }) + 1)
  ) %>%
  select(-additions, -additions_as_string)

# next set of "word features" will use a list of common vandalism words, so let's do
# data set split now as to not overtrain.
# 
# validation set will be 50% of source data, to be comparable to [Potthast 2010].
# 
# Note that our classes are imbalanced:
edits %>% pull(class) %>% table()
# but createDataPartition takes care of stratifying splits properly.
set.seed(123, sample.kind="Rounding")
validation_index <- createDataPartition(y = edits %>% pull(class), times = 1, p = 0.5, list = FALSE)
validation_edits <- edits[validation_index,]
train_edits <- edits[-validation_index,]

# load word dictionaries
profanities <- read_lines(file = "data/en/profanities.txt")
pronouns <- read_lines(file = "data/en/pronouns.txt")
contractions <- read_lines(file = "data/en/contractions.txt")
superlatives <- read_lines(file = "data/en/superlatives.txt")
wikisyntax <- read_lines(file = "data/global/wikisyntax.txt")

# This tokenizer regex is wikisyntax aware.
# matches:
# "----" section break
# indentation markers with ":", "*", "#" or "=".
# link delimiters with "{{", "}}", "[[" or "]]"
# any other regular word ( which would also include other wikisyntax )
wiki_regex <- "----|:{1,6}|\\*{1,4}|#{1,4}|={1,5}|\\{\\{|\\}\\}|\\[\\[|]]|\\w+"


# figure out most common words, but use only train_edits as to not overtrain
common_words <- train_edits %>%
  mutate(additions_as_string = map_chr(additions, str_c, collapse = "\n")) %>%
  mutate(
    word_list = map(str_match_all(additions_as_string, wiki_regex), as.vector),
    word_list_lower = map(word_list, str_to_lower),
  ) %>% select(editid, class, word_list_lower) %>%
  unnest_longer(col = word_list_lower, values_to = "word") %>%
  anti_join(stop_words) %>%
  filter(!word %in% wikisyntax) %>%
  filter(!str_detect(word, "^\\d+$")) %>% # no numbers
  group_by(class, word) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

common_vandalism_words <- common_words %>%
  filter(class == "vandalism") %>%
  slice_head(n = 200)

common_regular_words <- common_words %>%
  filter(class == "regular") %>%
  slice_head(n = 200)

intersection_words <- common_vandalism_words %>%
  inner_join(common_regular_words, by = "word")

common_vandalism_words <- common_vandalism_words %>%
  anti_join(intersection_words, by = "word") %>%
  arrange(word) %>%
  pull(word)

common_regular_words <- common_regular_words %>%
  anti_join(intersection_words, by = "word") %>%
  arrange(word) %>%
  pull(word)

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
    is_common_vandalism = map_lgl(word, function(w) {
      bin_search(common_vandalism_words, w) > 0
    }),
    is_common_regular = map_lgl(word, function(w) {
      bin_search(common_regular_words, w) > 0
    }),
    word_length = str_length(word)
  ) %>% group_by(editid) %>%
  summarise(
    profanity_count = sum(is_profanity),
    pronoun_count = sum(is_pronoun),
    superlative_count = sum(is_superlative),
    contraction_count = sum(is_contraction),
    wikisyntax_count = sum(is_wikisyntax),
    common_vandalism_count = sum(is_common_vandalism),
    common_regular_count = sum(is_common_regular),
    longest_word = max(word_length)
  )

# edit comment features
comment_features <- edits %>%
  select(editid, editcomment) %>%
  mutate(
    comment_exists = !editcomment == "null" | is.na(editcomment),
    comment_length = if_else(comment_exists, str_length(editcomment), 0L),
    comment_is_revert =
      str_starts(editcomment, fixed("Revert")) | # user revert
      str_starts(editcomment, fixed("revert")) | # user revert
      str_starts(editcomment, fixed("[[Help:Reverting|Reverted]] ")) | # user revert
      str_starts(editcomment, fixed("[[WP:RBK|Reverted]]")) | # bot revert
      str_starts(editcomment, fixed("[[WP:UNDO|Undid]]")), # bot undo ( aka late revert )
    comment_is_bot = str_starts(editcomment, "\\[\\[WP:"),
    # tokenize each comment
    word_lists = map(str_match_all(editcomment, "\\w+"), as.vector),
    word_lists_lower = map(word_lists, str_to_lower),
    # for each token, lowercase, and check if there is a hit on profanity list
    profanity_lists = map(word_lists_lower,
                      function(word_list_lower) {
                        map_lgl(word_list_lower, function(word) { bin_search(profanities, word) > 0 } )
                      }),
    # OR all word checks. if any TRUE, comment has profanity
    comment_has_profanity = map_lgl(profanity_lists, function(list) { reduce(list, `|`, .init = FALSE) })
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

#
# save pristine here
#


# let's do some descriptive analytics

edits %>%
  select(editid, class) %>%
  left_join(character_features, by = "editid") %>%
  left_join(word_features, by = "editid") %>%
  left_join(comment_features, by = "editid") %>%
  left_join(size_features, by = "editid") %>%
  left_join(editor_features, by = "editid") %>%
  mutate_at(3:24, scales::rescale, to=c(1,2)) %>% # data is not normal, so scale to fit [0,1]
  pivot_longer(cols = 3:24, names_to = "feature_name", values_to = "feature_value") %>%
  ggplot(aes(x = feature_value, color = class)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~feature_name)

edits %>%
  select(editid, class) %>%
  left_join(character_features, by = "editid") %>%
  ggplot(aes(sample = char_diversity, color = class)) +
  stat_qq() +
  stat_qq_line()

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

validation_features <- all_features[validation_index, ]
validation_class <- golden_class[validation_index]
validation_edits <- edits[validation_index,]
train_features <- all_features[-validation_index, ]
train_class <- golden_class[-validation_index]
train_edits <- edits[-validation_index,]


# check that we have good features
nearZeroVar(train_features, names = T)
# this yields:
# [1] "profanity_count"       "pronoun_count"         "superlative_count"     "contraction_count"
# [5] "comment_is_bot"        "comment_has_profanity"
# BUT, note that our classes are very skewed, with vandalism being only ~7%.



ggplot(train_knn)


y_hat_knn <- predict(train_knn, validation_features, type="raw")
cm <- confusionMatrix(y_hat_knn, validation_class)
cm
varImp(train_knn)
# knn is ok, with 140 false positives.



library(doParallel)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

control <- trainControl(method = "cv",
                        number = 10,
                        summaryFunction = prSummary,
                        classProbs = TRUE, # this makes AUC calculation work
                        allowParallel = TRUE)

#
# kNN
#
set.seed(123, sample.kind="Rounding")
train_knn <- train(
  x = train_features,
  y = train_class,
  method = "knn",
  trControl = control,
  tuneGrid = data.frame(k = c(3, 5, 7, 9, 11)),
  metric = "AUC", # PR-AUC
  maximize = TRUE
)

#
# Random Forest
#
set.seed(123, sample.kind="Rounding")
train_rf <-  train(
  x = train_features,
  y = train_class,
  method = "rf",
  ntree = 1000, # try with 1000
  trControl = control,
  tuneGrid = data.frame(mtry = c(1, 2, 3, 4, 5, 10, 25, 50, 100)),
  metric = "AUC", # PR-AUC
  maximize = TRUE
)

#
# Naive Bayes
#
#
  
set.seed(123, sample.kind="Rounding")
train_bayes <-  train(
  x = train_features,
  y = train_class,
  method = "naive_bayes",
  trControl = control,
  tuneGrid = expand.grid(laplace = 0, usekernel = TRUE, adjust = FALSE),
  metric = "AUC", # PR-AUC
  maximize = TRUE
)

#
# Neural Network
# 
set.seed(123, sample.kind="Rounding")
train_neural <-  train(
  x = train_features,
  y = train_class,
  method = "nnet",
  trControl = control,
  tuneGrid = expand.grid(size = c(1, 2, 3 , 4 ,5, seq(10,100, 9)), decay = seq(0, 1, 0.1)),
  metric = "AUC", # PR-AUC
  maximize = TRUE,
  maxit = 300 # max iterations
)


# let's also try oblique random forests

stopCluster(cl)

ggplot(train_rf)


y_hat_rf <- predict(train_rf, validation_features, type="raw")
cm_rf <- confusionMatrix(y_hat_rf, validation_class)
cm_rf
varImp(train_rf)

y_hat_rf_prob <- predict(train_rf, validation_features, type="prob")

# PR-AUC of 0.6230123 gets us second only to [MolaVelasco]'s 0.66522.
prauc <- MLmetrics::PRAUC(y_hat_rf_prob["vandalism"], if_else(validation_class == "vandalism", 1L, 0L))
# ROC-AUC of 0.920313 gets us also second only to [MolaVelasco]'s 0.92236
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

# yardstick says PR-AUC is 0.624.
results_rf %>%
  pr_auc(truth, vandalism)

results_rf %>%
  pr_curve(truth, vandalism) %>%
  autoplot()