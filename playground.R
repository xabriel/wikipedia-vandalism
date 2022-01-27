##
# In this project we intend to build a vandalism detection system for Wikipedia edits.
##

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

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

edits <- fread(file = paste(parentPath, "edits.csv", sep = ""),
               sep = ",",
               header = TRUE,
               showProgress = TRUE)

annotations <- fread(file = paste(parentPath, "gold-annotations.csv", sep = ""),
                     sep = ",",
                     header = TRUE,
                     showProgress = TRUE) %>%
  mutate(class = factor(class, levels = c("vandalism", "regular"))) # make sure vandalism == 1

edits <- edits %>%
  left_join(annotations %>% select(editid, class), by = "editid")

rm(parentPath, annotations)

revisionPaths <- list.files(path = paste(parentPath, "article-revisions", sep = ""),
                            full.names = TRUE,
                            recursive = TRUE)

revisions <- map_dfr(revisionPaths, function(path) {
  c(revisionid = str_remove(basename(path), ".txt"),
    text = read_file(path))
})
revisions <- revisions %>%
  mutate(revisionid = as.integer(revisionid))

rm(revisionPaths)

# save here as pristine?




# maybe use a text corpus package like tm?
