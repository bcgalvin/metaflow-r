
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/metaflow)](https://CRAN.R-project.org/package=metaflow)
<!-- badges: end -->

# <img src="man/figures/logo.png" align="center" width="100" /> metaflow

Interface to ‘Metaflow’ <https://metaflow.org/>, a framework for
constructing and managing data science workflows. ‘Metaflow’ implements
a unified API for the entire data science project lifecycle, from
initial prototyping all the way to production deployment. Key features
encompass version control, scalability, and seamless integration with
popular cloud orchestration tools. This R package enables data
scientists to harness ‘Metaflow’s’ capabilities within their preferred R
environment, facilitating efficient development and deployment of data
science projects.

## Installation

You can install the development version of metaflow from
[GitHub](https://github.com/bcgalvin/metaflow-r) with:

``` r
# install.packages("devtools")
devtools::install_github("bcgalvin/metaflow-r")
```

## Usage

``` r
library(metaflow)

# Install Metaflow
install_metaflow()

# Use Metaflow functions
metaflow()
mf()
```

For more detailed information, please refer to the package documentation
and vignettes.
