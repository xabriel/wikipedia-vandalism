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
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(kknn)) install.packages("kknn", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(multidplyr)
library(caret)
library(doParallel)
library(MLmetrics)
library(yardstick)
library(tidytext)
library(nnet)
library(kknn)
library(wordcloud)

# load up helper functions
source("functions.R")

# We utilize "PAN Wikipedia Vandalism Corpus 2010": https://webis.de/data/pan-wvc-10
# download corpus if needed. note the zip file is 438 MBs, and it expands to 1.4 GBs.
if (!dir.exists("data/pan-wikipedia-vandalism-corpus-2010")) {
  dir.create("data/pan-wikipedia-vandalism-corpus-2010")
  corpusFileName <- "data/pan-wikipedia-vandalism-corpus-2010.zip"
  corpusURI <-
    "https://zenodo.org/record/3341488/files/pan-wikipedia-vandalism-corpus-2010.zip?download=1"
  
  download.file(url = corpusURI,
                destfile = corpusFileName,
                method = "curl", # use curl since auto mode fails intermittently
                timeout = 300) # 5 minutes
  unzip(corpusFileName, exdir = "data")
  
  file.remove(corpusFileName)
  rm(corpusFileName, corpusURI)
}




## ----load-edits, include=FALSE, cache=TRUE------------------------------------------------------------------
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
  # make sure vandalism == 1
  mutate(class = factor(class, levels = c("vandalism", "regular")))

edits <- edits %>%
  left_join(annotations %>% select(editid, class), by = "editid")

rm(annotations)


## ----setup-multidplyr, include=FALSE, cache=TRUE------------------------------------------------------------
# use multidplyr to parallelize dplyr actions
cores <- coalesce(detectCores() - 1, 1)
cluster <- multidplyr::new_cluster(cores)
multidplyr::cluster_copy(cluster, 'git_diff')
multidplyr::cluster_library(cluster, 'tidyverse')
edits <- edits %>% partition(cluster)


## ----calculate-diffs, cache=TRUE, warning=FALSE-------------------------------------------------------------
parentPath <- "data/pan-wikipedia-vandalism-corpus-2010/"

# recursively find files in the article-revisions/ directory
revisionPaths <-
  list.files(
    path = paste(parentPath, "article-revisions", sep = ""),
    full.names = TRUE,
    recursive = TRUE
  )

# create a data frame with revision information
revisions <- map_dfr(revisionPaths, function(path) {
  list(
    revisionid = as.integer(str_remove(basename(path), ".txt")),
    revisionpath = path,
    revisionsize = file.size(path)
  )
})

# calculate diff for each edit
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
  # diff is a list of chars
  mutate(diff = git_diff(oldrevisionpath, newrevisionpath)) %>%
  select(editid,
         oldrevisionid,
         newrevisionid,
         oldrevisionsize,
         newrevisionsize,
         diff) %>%
  # additions are diff lines that start with '+'
  mutate(additions =
           lapply(diff, function(d) {
             s = d[str_starts(d, fixed("+")) & !str_starts(d, fixed("+++"))]
             str_sub(s, start = 2)
           })) %>%
  # deletions are diff lines that start with '-'
  mutate(deletions =
           lapply(diff, function(d) {
             s = d[str_starts(d, fixed("-")) & !str_starts(d, fixed("---"))]
             str_sub(s, start = 2)
           }))

edits <- edits %>%
  select(-diffurl) %>%
  left_join(diffs %>% select(-oldrevisionid,-newrevisionid),
            by = c("editid"))


## ---- include=FALSE, cache=TRUE-----------------------------------------------------------------------------
rm(revisionPaths, parentPath)
rm(diffs, revisions)


## ----imbalance, cache=TRUE----------------------------------------------------------------------------------
edits %>%
  collect() %>%
  group_by(class) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=class, y= n)) +
  geom_bar(stat="identity") +
  ggtitle("Distribution of edits per class")


## ----profanity, cache=TRUE----------------------------------------------------------------------------------
# load list of profanities
profanities <- read_lines(file = "data/en/profanities.txt")
# a regex that is aware of wikisyntax
wiki_regex <- "----|:{1,6}|\\*{1,4}|#{1,4}|={1,5}|\\{\\{|\\}\\}|\\[\\[|]]|\\w+"
# setup for parallelism
multidplyr::cluster_copy(cluster, 'bin_search')
multidplyr::cluster_copy(cluster, 'profanities')

# extract words from the diff additions, and check whether each one of them
# against a profanity word list. Then calculate the profanity rate for each class.
words_by_class <- edits %>%
  collect() %>%
  select(class, additions) %>%
  mutate(
    additions_as_string = map_chr(additions, str_c, collapse = "\n"),
    word_list = map(str_match_all(additions_as_string, wiki_regex), as.vector),
    word_list_lower = map(word_list, str_to_lower),
  ) %>%
  select(class, word_list_lower) %>%
  unnest_longer(col = word_list_lower, values_to = "word") %>%
  partition(cluster) %>%
  mutate(word = replace_na(word, ""),
         is_profanity = map_lgl(word, function(w) {
           bin_search(profanities, w) > 0
         })) %>%
  collect() %>%
  group_by(class)

words_by_class %>%
  summarise(profanity_rate = sum(is_profanity) / n()) %>%
  ggplot(aes(x = class, y = profanity_rate)) +
  geom_bar(stat="identity") +
  ggtitle("Rate of profanity use per class")


## ----wordcloud, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE, fig.align='center'---------------------
word_counts <- words_by_class %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "^\\d+$")) %>% # no numbers
  group_by(class, word) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  pivot_wider(names_from = class, values_from = n, values_fill = 0L) %>%
  # normalize vandalism words hits since it is 7.38%
  mutate(vandalism = as.integer(vandalism * 1/.0738))

word_counts_matrix <- as.matrix(word_counts[,-1])
rownames(word_counts_matrix) <- word_counts$word

comparison.cloud(
  word_counts_matrix,
  random.order=FALSE,
  title.size = 2L,
  match.colors = TRUE
)


## ----vandals-anonymous, cache=TRUE--------------------------------------------------------------------------
# a regex that matches IP addresses
ip_address_regex <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"

edits %>%
  collect() %>%
  select(editor, class) %>%
  mutate(is_anonymous = str_detect(editor, ip_address_regex)) %>%
  group_by(class) %>%
  summarise(anonymity_rate = sum(is_anonymous) / n()) %>%
  ggplot(aes(x = class, y = anonymity_rate)) +
  geom_bar(stat="identity") +
  ggtitle("Rate of anonymity per class")


## ----data-split, include=FALSE, cache=TRUE------------------------------------------------------------------
# validation set will be 50% of source data, to be comparable to [Potthast 2010].

# data is imbalanced, but createDataPartition takes care of stratifying splits properly.
set.seed(123, sample.kind = "Rounding")
validation_index <-
  createDataPartition(
    y = edits %>% pull(class),
    times = 1,
    p = 0.5,
    list = FALSE
  )
validation_edits <- edits %>% as_tibble() %>% .[validation_index, ]
train_edits <- edits %>% as_tibble() %>% .[-validation_index, ]


## ----features, include=FALSE, cache=TRUE--------------------------------------------------------------------
# copy function num_unique_chars into cluster
multidplyr::cluster_copy(cluster, 'num_unique_chars')

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
  select(-additions, -additions_as_string) %>%
  collect()

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

multidplyr::cluster_copy(cluster, 'profanities')
multidplyr::cluster_copy(cluster, 'pronouns')
multidplyr::cluster_copy(cluster, 'contractions')
multidplyr::cluster_copy(cluster, 'superlatives')
multidplyr::cluster_copy(cluster, 'wikisyntax')
multidplyr::cluster_copy(cluster, 'common_vandalism_words')
multidplyr::cluster_copy(cluster, 'common_regular_words')

word_features <- edits %>%
  collect() %>% # multidplyr doesn't like unnest_longer(), so let's do this section locally
  select(editid, additions) %>%
  mutate(
    additions_as_string = map_chr(additions, str_c, collapse = "\n"),
    word_list = map(str_match_all(additions_as_string, wiki_regex), as.vector),
    word_list_lower = map(word_list, str_to_lower),
  ) %>%
  select(editid, word_list_lower) %>%
  unnest_longer(col = word_list_lower, values_to = "word") %>%
  partition(cluster) %>% # now leverage multidplyr
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
  ) %>%
  collect() %>%
  # since some lists may be emtpy, this group_by may return less rows than there are edits
  # so we need to compensate for this later when joining.
  group_by(editid) %>%
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
  select(-editcomment, -word_lists, -word_lists_lower, -profanity_lists) %>%
  collect()

size_features <- edits %>%
  select(editid, oldrevisionsize, newrevisionsize, additions, deletions) %>%
  mutate(
    size_delta = newrevisionsize - oldrevisionsize,
    size_ratio = (newrevisionsize + 1) / (oldrevisionsize + 1),
    num_additions = map_int(additions, length),
    num_deletions = map_int(deletions, length)
  ) %>%
  select(-oldrevisionsize,-newrevisionsize,-additions,-deletions) %>%
  collect()

ip_address_regex <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"
multidplyr::cluster_copy(cluster, 'ip_address_regex')

editor_features <- edits %>%
  select(editid, editor, edittime) %>%
  mutate(is_anonymous = str_detect(editor, ip_address_regex)) %>%
  select(editid, is_anonymous) %>%
  collect()

# prepare features as matrix to feed to algos
all_features <-
  character_features %>%
  left_join(word_features, by = "editid") %>%
  # fill in with 0s whatever word_features may be missing
  mutate_all(~replace(., is.na(.), 0L)) %>%
  left_join(comment_features, by = "editid") %>%
  left_join(size_features, by = "editid") %>%
  left_join(editor_features, by = "editid") %>%
  arrange(editid) %>% # make sure we align rows with class
  select(-editid) %>%
  as.matrix()

#
# split into train, test, and validation sets
#
golden_class <- edits %>% collect() %>% arrange(editid) %>% pull(class)
validation_features <- all_features[validation_index, ]
validation_class <- golden_class[validation_index]
train_features <- all_features[-validation_index, ]
train_class <- golden_class[-validation_index]


## ----rm-multiplyr-cluster, include=FALSE, cache=TRUE--------------------------------------------------------
# we are now done with multidplyr parallelism
rm(cluster)


## ----ml-setup, include=FALSE, cache=TRUE--------------------------------------------------------------------

# setup parallel cluster for caret
library(doParallel)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = prSummary, # this summary function includes PR-AUC
  classProbs = TRUE, # this makes AUC calculation work
  allowParallel = TRUE
)


## ----knn, cache=TRUE, include=FALSE-------------------------------------------------------------------------
#
# kNN
#
set.seed(123, sample.kind = "Rounding")
train_knn <- train(
  x = train_features,
  y = train_class,
  method = "knn",
  trControl = control,
  tuneGrid = data.frame(k = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)),
  metric = "AUC", # PR-AUC
  maximize = TRUE
)


## ----knn-normalized, cache=TRUE, include=FALSE--------------------------------------------------------------
#
# kNN, with features normalized to [0,1]
#
set.seed(123, sample.kind = "Rounding")
train_knn_normalized <- train(
  x = train_features,
  y = train_class,
  method = "knn",
  trControl = control,
  tuneGrid = data.frame(k = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)),
  metric = "AUC", # PR-AUC
  maximize = TRUE,
  preProcess = "range" # normalize all features to [0,1]
)


## ----knn-normalized-weigthed, cache=TRUE, include=FALSE-----------------------------------------------------
#
# kNN, with features normalized to [0,1], and with weigthed neighboors
#
set.seed(123, sample.kind = "Rounding")
train_knn_normalized_weigthed <- train(
  x = train_features,
  y = train_class,
  method = "kknn",
  trControl = control,
  tuneGrid = expand.grid(
    kmax = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21),
    distance = c(1, 2), # try manhattan and euclidean distance
    kernel = "optimal"
  ),
  metric = "AUC", # PR-AUC
  maximize = TRUE,
  preProcess = "range" # normalize all features to [0,1]
)


## ----neural-network, message=FALSE, warning=FALSE, cache=TRUE, include=FALSE--------------------------------
#
# Neural Network
#
set.seed(123, sample.kind = "Rounding")
train_neural <-  train(
  x = train_features,
  y = train_class,
  method = "nnet",
  trControl = control,
  tuneGrid = expand.grid(
    size = c(1, 2, 3, 4, 5, 10, 15, 20, 25),
    decay = seq(0, 1, 0.1)
  ),
  metric = "AUC", # PR-AUC
  maximize = TRUE,
  maxit = 300 # max iterations
)


## ----random-forest-150, include=FALSE, cache=TRUE-----------------------------------------------------------
#
# Random Forest 150
#
set.seed(123, sample.kind = "Rounding")
train_rf_150 <-  train(
  x = train_features,
  y = train_class,
  method = "rf",
  ntree = 150,
  trControl = control,
  tuneGrid = data.frame(mtry = c(1, 2, 3, 4, 5, 10, 25, 50, 100)),
  metric = "AUC", # PR-AUC
  maximize = TRUE,
  preProcess = "range" # normalize all features to [0,1]
)


## ----random-forest-500, include=FALSE, cache=TRUE-----------------------------------------------------------
#
# Random Forest 500
#
set.seed(123, sample.kind = "Rounding")
train_rf_500 <-  train(
  x = train_features,
  y = train_class,
  method = "rf",
  ntree = 500,
  trControl = control,
  tuneGrid = data.frame(mtry = c(1, 2, 3, 4, 5, 10, 25, 50, 100)),
  metric = "AUC", # PR-AUC
  maximize = TRUE,
  preProcess = "range" # normalize all features to [0,1]
)


## ----random-forest-1000, include=FALSE, cache=TRUE----------------------------------------------------------
#
# Random Forest 1000
#
set.seed(123, sample.kind = "Rounding")
train_rf_1000 <-  train(
  x = train_features,
  y = train_class,
  method = "rf",
  ntree = 1000,
  trControl = control,
  tuneGrid = data.frame(mtry = c(1, 2, 3, 4, 5, 10, 25, 50, 100)),
  metric = "AUC", # PR-AUC
  maximize = TRUE,
  preProcess = "range" # normalize all features to [0,1]
)


## ----clean-up-caret, include=FALSE, cache=TRUE--------------------------------------------------------------
# we are done with caret cluster
stopCluster(cl)


## ----validation-predictions, cache=TRUE, include=FALSE------------------------------------------------------
pred_knn <-
  predict(train_knn, newdata = validation_features, type="prob")
pred_knn_normalized <-
  predict(train_knn_normalized, newdata = validation_features, type="prob")
pred_knn_normalized_weigthed <-
  predict(train_knn_normalized_weigthed, newdata = validation_features, type="prob")

pred_neural <- predict(train_neural, newdata = validation_features, type="prob")

pred_rf_150 <- predict(train_rf_150, newdata = validation_features, type="prob")
pred_rf_500 <- predict(train_rf_500, newdata = validation_features, type="prob")
pred_rf_1000 <- predict(train_rf_1000, newdata = validation_features, type="prob")

set.seed(123, sample.kind = "Rounding")
pred_random <- sample(x = c(0,1), size = nrow(validation_features), replace = TRUE, prob = c(0.5,0.5))

# dataframe with truth and 'positive' class (vandalism) probability
predictions = tibble(
  truth = validation_class,
  knn = pred_knn$vandalism,
  knn_normalized = pred_knn_normalized$vandalism,
  knn_normalized_weigthed = pred_knn_normalized_weigthed$vandalism,
  nn = pred_neural$vandalism,
  rf_150 = pred_rf_150$vandalism,
  rf_500 = pred_rf_500$vandalism,
  rf_1000 = pred_rf_1000$vandalism,
  random = pred_random
)


## ----validation-aocs, echo=FALSE, cache=TRUE----------------------------------------------------------------
pr_aocs <- bind_rows(
  pr_auc(predictions, truth = "truth", knn) %>%
    add_column(model = "kNN"),
  pr_auc(predictions, truth = "truth", knn_normalized) %>%
    add_column(model = "Normalized kNN"),
  pr_auc(predictions, truth = "truth", knn_normalized_weigthed) %>%
    add_column(model = "Weigthed Normalized kNN"),
  pr_auc(predictions, truth = "truth", nn) %>%
    add_column(model = "Neural Network"),
  pr_auc(predictions, truth = "truth", rf_150) %>%
    add_column(model = "Random Forest with 150 trees"),
  pr_auc(predictions, truth = "truth", rf_500) %>%
    add_column(model = "Random Forest with 500 trees"),
  pr_auc(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  pr_auc(predictions, truth = "truth", random) %>%
    add_column(model = "Random Classifier")
)

roc_aocs <- bind_rows(
  roc_auc(predictions, truth = "truth", knn) %>%
    add_column(model = "kNN"),
  roc_auc(predictions, truth = "truth", knn_normalized) %>%
    add_column(model = "Normalized kNN"),
  roc_auc(predictions, truth = "truth", knn_normalized_weigthed) %>%
    add_column(model = "Weigthed Normalized kNN"),
  roc_auc(predictions, truth = "truth", nn) %>%
    add_column(model = "Neural Network"),
  roc_auc(predictions, truth = "truth", rf_150) %>%
    add_column(model = "Random Forest with 150 trees"),
  roc_auc(predictions, truth = "truth", rf_500) %>%
    add_column(model = "Random Forest with 500 trees"),
  roc_auc(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  roc_auc(predictions, truth = "truth", random) %>%
    add_column(model = "Random Classifier")
)

pr_aocs <- pr_aocs %>%
  rename("PR-AUC" = .estimate) %>%
  select("PR-AUC", model)

roc_aocs <- roc_aocs %>%
  rename("ROC-AUC" = .estimate) %>%
  select("ROC-AUC", model)

# PR-AUC and ROC-AUC of all tested algorithms
pr_aocs %>%
  left_join(roc_aocs, by = "model") %>%
  select(model, "PR-AUC", "ROC-AUC") %>%
  arrange("PR-AUC")

## ----pr-curves, echo=FALSE, cache=TRUE----------------------------------------------------------------------
pr_curves <- bind_rows(
  pr_curve(predictions, truth = "truth", knn) %>%
    add_column(model = "kNN"),
  pr_curve(predictions, truth = "truth", knn_normalized) %>%
    add_column(model = "Normalized kNN"),
  pr_curve(predictions, truth = "truth", knn_normalized_weigthed) %>%
    add_column(model = "Weigthed Normalized kNN"),
  pr_curve(predictions, truth = "truth", nn) %>%
    add_column(model = "Neural Network"),
  pr_curve(predictions, truth = "truth", rf_150) %>%
    add_column(model = "Random Forest with 150 trees"),
  pr_curve(predictions, truth = "truth", rf_500) %>%
    add_column(model = "Random Forest with 500 trees"),
  pr_curve(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  # fix random curve as it should be a constant line approx at the vandalism/regular ratio
  pr_curve(predictions, truth = "truth", random) %>%
    mutate(precision = 0.073) %>%
    add_column(model = "Random Classifier")
)

pr_curves %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  ggtitle("PR Curves of all tested algorithms")


## ----roc-curves, echo=FALSE, cache=TRUE---------------------------------------------------------------------
roc_curves <- bind_rows(
  roc_curve(predictions, truth = "truth", knn) %>%
    add_column(model = "kNN"),
  roc_curve(predictions, truth = "truth", knn_normalized) %>%
    add_column(model = "Normalized kNN"),
  roc_curve(predictions, truth = "truth", knn_normalized_weigthed) %>%
    add_column(model = "Weigthed Normalized kNN"),
  roc_curve(predictions, truth = "truth", nn) %>%
    add_column(model = "Neural Network"),
  roc_curve(predictions, truth = "truth", rf_150) %>%
    add_column(model = "Random Forest with 150 trees"),
  roc_curve(predictions, truth = "truth", rf_500) %>%
    add_column(model = "Random Forest with 500 trees"),
  roc_curve(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  roc_curve(predictions, truth = "truth", random) %>%
    add_column(model = "Random Classifier")
)

roc_curves %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line() +
  ggtitle("ROC Curves of all tested algorithms") +
  ylab("sensitivity (TP rate)") +
  xlab("1 - specificity (FP rate)")


## ----potthast-comparison, echo=FALSE, cache=TRUE------------------------------------------------------------
# load in data from best algorithm from Potthast
archivePath <- "data/pan-wikipedia-vandalism-detection-2010/molavelasco10-runs/"
archiveName <- "molavelasco10-run-2010-06-23-1103.txt.zip"
fileName <- "molavelasco10-run-2010-06-23-1103.txt"
unzip(paste(archivePath, archiveName, sep = ""),
      exdir = archivePath,
      files = c(fileName))

molaVelascoRun <- read_delim(
  file = paste(archivePath, fileName, sep = ""),
  delim = " ",
  col_names = c("oldrevisionid", "newrevisionid", "prediction", "probability"),
  col_types = cols(
    oldrevisionid = col_integer(),
    newrevisionid = col_integer(),
    prediction = col_character(),
    probability = col_double()
  )
) %>% mutate(
  prediction = factor(
    if_else(prediction == "R", "regular", "vandalism"),
    levels = c("vandalism", "regular"))
) %>% right_join(validation_edits, by = c("oldrevisionid", "newrevisionid")) %>%
  select(editid, oldrevisionid, newrevisionid, class, prediction, probability)

comparison_pr_curves <- bind_rows(
  pr_curve(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  pr_curve(molaVelascoRun, truth = "class", probability) %>%
    add_column(model = "Mola Velasco detector from [6]")
)

comparison_pr_curves %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  ggtitle("PR Curves of our best algorithm compared with best of [6]")

pr_aocs <- bind_rows(
  pr_auc(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  pr_auc(molaVelascoRun, truth = "class", probability) %>%
    add_column(model = "Mola Velasco detector from [6]")
)

roc_aocs <- bind_rows(
  roc_auc(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  roc_auc(molaVelascoRun, truth = "class", probability) %>%
    add_column(model = "Mola Velasco detector from [6]")
)

pr_aocs <- pr_aocs %>%
  rename("PR-AUC" = .estimate) %>%
  select("PR-AUC", model)

roc_aocs <- roc_aocs %>%
  rename("ROC-AUC" = .estimate) %>%
  select("ROC-AUC", model)

# PR-AUC and ROC-AUC of best algorithm and best from [6]
pr_aocs %>%
  left_join(roc_aocs, by = "model") %>%
  select(model, "PR-AUC", "ROC-AUC") %>%
  arrange("PR-AUC")


## ----cluebot-comparison, echo=FALSE, warning=FALSE, cache=TRUE----------------------------------------------
comparison_roc_curves <- bind_rows(
  roc_curve(predictions, truth = "truth", rf_1000) %>%
    add_column(model = "Random Forest with 1000 trees"),
  roc_curve(molaVelascoRun, truth = "class", probability) %>%
    add_column(model = "Mola Velasco detector from [6]"),
  tibble(specificity = c(1 - 0.01, 1 - 0.025), sensitivity = c(0.4,  0.55)) %>%
    add_column(model = "ClueBot NG")
)

cluebot <-
  tibble(specificity = c(1 - 0.01, 1 - 0.025),
         sensitivity = c(0.4,  0.55))

comparison_ggplot <- comparison_roc_curves %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line() +
  geom_point(data = cluebot,
             mapping = aes(x = 1 - specificity, y = sensitivity),
             inherit.aes = FALSE) +
  ggtitle("ROC Curve of best algorithm and data from ClueBot NG") +
  ylab("sensitivity (TP rate)") +
  xlab("1 - specificity (FP rate)")

# comparison_ggplot

# plot it against zoomed in
comparison_ggplot +
  ggtitle("Zoomed in ROC Curve of best algorithm and data from ClueBot NG") +
  xlim(0, 0.03) +
  ylim(0.35, 0.60)

# Practical comparison points of best algorithms and ClueBot NG
tibble(
  "model" = c("Random Forest with 1000 trees", "ClueBot NG", "Mola Velasco from [6]"),
  "TP-rate @ 0.01% FP-rate" =  c(42.5, 40, 40),
  "TP-rate @ 0.025% FP-rate" = c(57.5, 55, 51.5),
  "Average Improvement % from ClueBot NG" = c(
    (42.5 + 57.5 - 40 - 55) / (40 + 55) * 100,
    (40 + 55 - 40 - 55) / (40 + 55) * 100,
    (40 + 51.5 - 40 - 55) / (40 + 55) * 100)
)

