
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

------------------------------------------------------------------------

## Installation

You can install the development version of metaflow from GitHub with:

``` r
devtools::install_github("bcgalvin/metaflow-r")
```

## Implemented Features

The metaflow package offers functionality for managing Metaflow named
profiles and provides integration with the Metaflow S3 client. Here’s an
overview of the main features:

### Profile Management

The package provides functions to manage Metaflow profiles, allowing you
to handle different configurations for your workflows. It respects
`METAFLOW_HOME` and `METAFLOW_PROFILE` environment variables.

``` r
# List all profiles in Metaflow home directory
list_profiles()

# Get Active Metaflow Profile
get_active_profile()

# Update the Metaflow profile by name or path
update_profile(name = "my_profile")
# or
update_profile(path = "/path/to/profile.json")
```

### S3 Client

The metaflow S3 client is now fully implemented with R6 classes but
there will be a more R-friendly interface for this soon. Check out the
vignettes for the R equivalents to the metaflow s3 client python docs.

``` r
library(metaflow)

s3 <- S3$new(s3root='s3://metaflow-r-s3/tmp/s3demo/')
s3obj <- s3$get('fruit')
# > s3obj
# S3 Object:
#   URL: s3://metaflow-r-s3/tmp/s3demo/fruit 
#   Key: fruit 
#   Prefix: s3://metaflow-r-s3/tmp/s3demo 
#   Size: 9 bytes
#   Exists: TRUE 
#   Downloaded: TRUE 
#   Content Type: binary/octet-stream 
#   Last Modified: 2024-10-15 16:23:46 
#   Has Info: TRUE 
#   Encryption: AES256 

cat('location', s3obj$url, '\n')
cat('key', s3obj$key, '\n')
cat('size', s3obj$size, '\n')
cat('local path', s3obj$path, '\n')
cat("bytes", as.character(s3obj$blob), "\n") # return is really a python bytes object but can't cat it 
cat('unicode', s3obj$text, '\n')
cat('metadata', s3obj$metadata, '\n')
cat('content-type', s3obj$content_type, '\n')
cat('downloaded', s3obj$downloaded, '\n')
# location s3://metaflow-r-s3/tmp/s3demo/fruit 
# key fruit 
# size 9 
# local path metaflow.s3.dcn4wxq6/metaflow.s3.one_file.o34_2d3z 
# bytes pineapple 
# unicode pineapple 
# metadata  NULL
# content-type binary/octet-stream 
# downloaded TRUE 

s3 <- S3$new()
res <- s3$get('s3://metaflow-r-s3/tmp/external_data')
# > res$text
# [1] "I know nothing about Metaflow"
```
