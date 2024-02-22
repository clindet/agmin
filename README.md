# agmin

The aim of the [agmin] R package is to simplify the data mining steps for age-correlated clinical- and/or omics-based features.

This R package provides unified interfaces to transform data format or pre-process variables, such as age, genders, gene expression counts and other genetic or protein level data. Users can easily conducted the data analysis works by using R functions and HIPLOT (ORG)-based web plugins. The integration of public databases and customized cohorts can also be integrated in a simple way.

It is noted that we developed a standard workflow to perform correlation analysis, pathway enrichment, and clustering/classification based on age-correlated genesets/features and pre-trained models. It will provides more insights how age impact the disease and biological process. As a demo, we has applied this package to more than thousands of patients with acute myeloid leukaemia. These results can help us to define the aging status/patterns and identify potential age-correlated therapeutic targets in AML patients.

## Core features 

- Age cuts: demo output [<20, 20-29, 30-39, ..., >=70];
- Rate trend in age groups, e.g. genes mutation rate and its statistical significance with age;
- Oncoplots of age groups;
- Correlation/differences analysis of continuous variables;
- Clustering via using age-correlated features and known aging genesets;
- ......

## LICENSE

MIT