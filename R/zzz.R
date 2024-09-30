.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  .check_python_installed()
}

#' @import cli
.check_python_installed <- function() {
  a <- reticulate::py_discover_config()
  if (is.null(a) || length(a)<1) {
    cli::cli_alert_warning('Could not locate Python installation, which will limit ability to query from model results.')
    cli::cli_inform('Try `reticulate::install_miniconda()`, or see https://rstudio.github.io/reticulate/reference/install_miniconda.html')
  } else {
    cli::cli_alert_success(glue::glue('Python installation found (Default: {a$python}).'))
  }
}