#
# Helper functions
#

#
# git diff system call with arguments needed to make the output easily parsable
# the key arg is --word-diff=porcelain, which separates edits into additions (+) and deletions (-)
#
git_diff <- Vectorize(function(old, new) {
  system2(
    command = "git",
    args = c(
      "diff",
      "--no-prefix",
      "--no-index",
      "--word-diff=porcelain",
      "--unified=0",
      old,
      new
    ),
    stdout = TRUE
  )
})


#
# binary search implementation based on:
# https://en.wikipedia.org/wiki/Binary_search_algorithm#Algorithm
#
bin_search <- function(vec, key) {
  l <- 1
  r <- length(vec)
  while (l <= r) {
    m <- as.integer(floor((l + r) / 2))
    if (vec[m] < key) {
      l <- m + 1
    } else if (vec[m] > key) {
      r <- m - 1
    } else {
      return(m)
    }
  }
  return(-1)
}

# quick sanity tests for bin_search
empty <- c()
chars <- c("a", "b", "c")
stopifnot(bin_search(empty, "b") < 0)
stopifnot(bin_search(chars, "b") == 2)
stopifnot(bin_search(chars, "d") < 0)
rm(empty, chars)


#
# given a char vector, find the number of unique chars for each one
#
num_unique_chars <- function(s) {
  chars_list <- str_split(s, pattern = "")
  map_int(chars_list, function(chars) {
    length(unique(chars))
  })
}

# sanity tests for num_unique_chars
empty <- c()
strings <- c("a", "aa", "abcdefg", "")
stopifnot(num_unique_chars(empty) == 0)
stopifnot(num_unique_chars(strings) == c(1, 1, 7, 0))
rm(empty, strings)
