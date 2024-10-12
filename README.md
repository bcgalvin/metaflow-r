
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

You can install the development version of metaflow from
[GitHub](https://github.com/) with:

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

The interface for the metaflow S3 client is implemented with R6 classes
but there will be a more user-friendly interface built on top soon.

``` r
# Create a new S3Client
s3_client <- create_s3_client(tmproot = ".", bucket = "my-bucket", prefix = "my-prefix")

# Connect to S3
s3_client$connect()

# List objects in the root of the S3 path
root_objects <- s3_client$list_paths()

# Get a single object
s3_object <- s3_client$get("my_key")

# Put a single object
url <- s3_client$put("my_key", "My object content")

# Get multiple objects
s3_objects <- s3_client$get_many(c("key1", "key2", "key3"))

# Put multiple objects
results <- s3_client$put_many(list(
  list(key = "key1", value = "Content 1"),
  list(key = "key2", value = "Content 2")
))

# Get objects recursively
recursive_objects <- s3_client$get_recursive(c("prefix1", "prefix2"))

# Put multiple files
results <- s3_client$put_files(list(
  list(key = "key1", path = "path/to/file1"),
  list(key = "key2", path = "path/to/file2")
))
```
