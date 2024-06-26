% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mut.R
\name{vis_mutc_percent}
\alias{vis_mutc_percent}
\title{Percent barplot}
\usage{
vis_mutc_percent(
  x,
  maf,
  age = "age",
  facet_var = "fusion_genes",
  y_axis = "age_cut",
  palt = NULL,
  sample_id = "Tumor_Sample_Barcode",
  gene = "Hugo_Symbol",
  type = "Variant_Classification",
  logist_mutc = 4
)
}
\description{
Percent barplot
}
\examples{
maf <- read.csv(system.file("extdata", "aml_maf.txt", package = "agmin"), sep = "\t")
x <- read.csv(system.file("extdata", "aml_samples.txt", package = "agmin"), sep = "\t")
row.names(x) <- x[,1]
x$age_cut <- ag_cut(x$Age)
vis_mutc_percent(x, maf, age = "Age", facet_var = "", fill = "", palt = ag_col())
}
