#' Metaflow for R
#'
#' @description
#' \href{https://metaflow.org/}{Metaflow} is an open-source framework
#' for data science projects. It's designed to handle the entire lifecycle
#' of data science workflows, from prototyping to production deployment.
#' Metaflow allows you to write and manage data processing pipelines using
#' a simple, intuitive API.
#'
#' @details
#' The \href{https://docs.metaflow.org/}{Metaflow API} is primarily
#' implemented in Python, with bindings for R. This package provides access
#' to the complete Metaflow functionality from within R.
#'
#' Key features of Metaflow include:
#' \itemize{
#'   \item Seamless scaling from prototype to production
#'   \item Built-in versioning of data and code
#'   \item Easy integration with cloud computing resources
#'   \item Support for parallel and distributed computing
#' }
#'
#' @section Documentation:
#' For additional documentation on the Metaflow package, see
#' \href{https://docs.metaflow.org/v/r/}{https://docs.metaflow.org/v/r/}
#'
#' @section Enterprise Support:
#' For more information about managed Metaflow enterprise offerings, visit
#' \href{https://outerbounds.com/}{Outerbounds}
#'
#'
#' @docType package
#' @name metaflow
#' @seealso
#' \code{\link{install_metaflow}} for installing Metaflow and its dependencies
#' @author Bryan Galvin (bcgalvin@@gmail.com)
#' @keywords package
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom testthat test_that expect_identical expect_error expect_silent
#' @importFrom testthat local_mocked_bindings
## usethis namespace: end
