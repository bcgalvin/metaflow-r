#' Install Metaflow and its dependencies
#'
#' @description
#' `install_metaflow()` installs the Metaflow Python package and its
#' direct dependencies from PyPI using pip. This function provides a convenient
#' way to set up Metaflow within an R environment.
#'
#' @param method Installation method. Can be "auto", "virtualenv", or "conda".
#'   This affects the type of environment created, but Metaflow will always
#'   be installed via pip.
#' @param conda Path to conda executable (only applicable if method = "conda")
#' @param version Metaflow version to install. Use "default" for the latest
#'   version, or specify a version like "2.12.23" or "2.11.6".
#' @param envname Name of the virtual/conda environment to install packages
#'   into. Default is "r-metaflow".
#' @param extra_packages Additional Python packages to install along with
#'   Metaflow
#' @param restart_session Restart R session after installing (only in RStudio)
#' @param python_version Python version to use. Must be >= 3.8.
#' @param new_env Whether to create a new environment or use an existing one
#' @param ... Additional arguments passed to reticulate installation functions
#'
#' @details
#' This function installs Metaflow from PyPI using pip, regardless of whether
#' a virtualenv or Conda environment is used. If a Conda environment is
#' specified, the function will create a Conda environment but still use pip to
#' install Metaflow within that environment.
#'
#' The function performs several checks and steps:
#' \itemize{
#'   \item Verifies that the system is not Windows (not supported)
#'   \item Checks and sets the appropriate Python version
#'   \item Prepares the installation environment (new or existing)
#'   \item Installs Metaflow and any additional specified packages
#' }
#'
#' @return
#' This function returns NULL invisibly. The primary side effect is the
#' installation of Metaflow and its dependencies in the specified environment.
#'
#' @examples
#' \dontrun{
#' install_metaflow()
#' install_metaflow(method = "conda", version = "2.3.0")
#' }
#'
#' @seealso
#' \code{\link{reticulate}} package for R interface to Python
#'
#' @export
install_metaflow <- function(
    method = c("virtualenv", "conda", "auto"),
    conda = "auto",
    version = "default",
    envname = "r-metaflow",
    extra_packages = NULL,
    restart_session = TRUE,
    python_version = NULL,
    new_env = TRUE,
    ...) {
  method <- match.arg(method)
  checkmate::assert_choice(method, c("virtualenv", "conda", "auto"))
  checkmate::assert_string(conda)
  checkmate::assert_string(version)
  checkmate::assert_string(envname, min.chars = 1L)
  checkmate::assert_character(extra_packages, null.ok = TRUE)
  checkmate::assert_logical(restart_session, len = 1L)
  checkmate::assert_string(python_version, null.ok = TRUE)
  checkmate::assert_logical(new_env, len = 1L)

  check_system()
  python_version <- check_python_version(python_version)

  prepare_environment(new_env, envname, method, python_version)

  packages <- prepare_packages(version, extra_packages, method)

  # Install packages
  reticulate::py_install(
    packages = packages,
    envname = envname,
    method = method,
    conda = conda,
    python_version = python_version,
    pip = TRUE,
    ...
  )

  cli::cli_alert_success("{.pkg Metaflow} installation complete.")

  if (restart_session &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::hasFun("restartSession")) {
    rstudioapi::restartSession()
  }

  invisible(NULL)
}

#' Get the installed Metaflow version
#'
#' @description
#' `metaflow_version()` returns the version of the installed Metaflow Python
#' package.
#'
#' @return A character string representing the Metaflow version.
#'
#' @examples
#' metaflow_version()
#'
#' @export
metaflow_version <- function() {
  if (is_metaflow_available()) {
    .globals[["mf"]][["__version__"]]
  } else {
    stop(
      "Metaflow is not available. ",
      "Please install Metaflow using install_metaflow()."
    )
  }
}
