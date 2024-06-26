% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pop.R
\name{vis_hist}
\alias{vis_hist}
\title{Draw histogram of age.}
\usage{
vis_hist(
  x,
  age_col = "age",
  facet_var = NULL,
  fill = "#000000",
  binwidth = 1,
  add_param = list(title = "All samples", title_median = TRUE, title_iqr = TRUE, digits =
    0, add_line = "median", linecol = "gray", linetype = "dashed"),
  ...
)
}
\arguments{
\item{x}{A data.frame. It contains age column at least.}

\item{age_col}{Column name of age.}

\item{facet_var}{Column name for facet display.}

\item{binwidth}{vis_histogram binwidth.}

\item{title}{Do not work in the facet mode.}

\item{title_median}{Show median in title.}

\item{title_iqr}{Show IQR in title.}

\item{digits}{Set digits.}

\item{add_line}{Add median or mean line in plots.}
}
\description{
Draw histogram of age.
}
\examples{
age <- data.frame(age=rgamma(10000, shape = 50), groups = "Age-50")
vis_hist(age)
age2 <- data.frame(age=rgamma(10000, shape = 70), groups = "Age-70")
age <- rbind(age, age2)
vis_hist(age, facet_var = "groups")
}
