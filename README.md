
<!-- README.md is generated from README.Rmd. Please edit that file -->
mnma
====

The goal of mnma package is to make it easy to run Bayesian multivariate network meta-analysis (NMA) method by Efthimiou et al. [(Biostatistics, 2015)](https://www.ncbi.nlm.nih.gov/pubmed/?term=Joint+synthesis+of+multiple+correlated+outcomes+in+networks+of+interventions) using OpenBUGS. Although there are many packages for the univariate NMA, there is no package available for multivariate NMA due to its complexity. To run bugs command in this package, [OpenBUGS](http://www.openbugs.net/w/Downloads) has to be pre-installed in the local computer.

Installation
------------

You can install mnma from github with:

``` r
# install.packages("devtools")
devtools::install_github("vandy10s/mnma")
```

Example
-------

This is a basic example which shows you how to use functions in this pacakge

*1. Split dataset into outcome1, outcome2, and treatments.*

First 2 (or 3) columns in the outcome contain the information about number of events for each arm and the rest 2 (or 3) columns contain the sample size for each arm. Treatments column contains maximum 3 treatment arms for each study.

``` r
library(mnma)
data(acute)
outcome1 <- acute[,c(1:6)]     # outcome1
outcome2 <- acute[,c(7:12)]    # outcome2
TX    <- acute[,13:15]
```

*2. Convert data *

`mnma.model` function converts outcome data into log odds ratio (OR) inputs and arrange them for multivariate NMA.

``` r
data <- mnma.model(outcome1,outcome2,TX, ref=1, parameters="simple")
```

*3. Run multivariate NMA using `mnma.run` function and check the result *

Adjust options in the function in the similar way as OpenBUGS. Type `res$summary` after running the function to check the summarized results. The BUGS code is executed automatically depending on the condition of inputs.

``` r
res <- mnma.run(data, n.iter=3000, n.burnin=1000)
```

*4. Similarly, run univariate NMA using `unma.run` function and compare results with those from multivariate NMA model. *
