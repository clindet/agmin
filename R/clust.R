#' One-click clustering from feature selection to
#' visulization and correlation analysis
#'
#'
easy_clust <- function(x, meta = NULL) {

  # step 1 (recommended): get protein-coding subset

  # step 2 (optional): drop low variance features

  # step 3 (recommended): drop genes with high relavance of gender
  # according to the threadhod of gender correlations

  # step 4 (recommended): top 2000+ genes for
  # defing bias gene clusters (unstable; most from immune microenvironment;
  # highly correlated with tumour cell purity)

  # step 5 (recommended): get top features using different methods, ref. cola

  # step 6 (optional): generate gene correlation modules for adjust top features;

  # step 6 (recommended): sampling, iteration and generate a pool of cluster labels;

  # step 7 (optional): evaluation significance:
  # known strong connection between clusters and meta features;
  # prognosis or other clinical features
}
