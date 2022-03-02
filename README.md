## Detecting Vandalism on Wikipedia

In this report we discuss the problem of detecting vandalism in Wikipedia, an online encyclopedia that anyone can edit. We utilize the PAN Wikipedia 2010 Vandalism corpus to derive a set of 24 features. We discuss, train and validate a set of binary classification algorithms. We find that Random Forests yield the best performance both from a PR-AUC (0.6608231), as well as a ROC-AUC (0.9306720) perspective. Specifically, a Random Forest with 1000 trees and 3 random features at each split point yields the best performance.

When we compare it against "ClueBot NG", Wikipedia's current production classifier, we find our solution has an average ~5% advantage at FP-rates of 0.01% and 0.025%. In other words, out of every 1000 edits, we can detect 425 vandalisms while we incorrectly label 10. The current system detects 400.

### Relevant files:

wikipedia-vandalism.Rmd : Rmd file with report of our work, explaining the problem, our methods, and results.

wikipedia-vandalism.pdf : PDF rendering of the above.

wikipedia-vandalism.R : Stand alone R script that computes our model.


### Supporting files:

functions.R : Helper functions used in wikipedia-vandalism.R and wikipedia-vandalism.Rmd.

scrapes.R: Useful scrapes to get word lists. Scraped lists can be found under `data/en/`.

### Warning:

Because of the nature of vandalism, this report contains discussions of profanities, as well as a word list of profanities.
