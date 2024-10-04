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
#' @param python_version Python version to use. Must be >= 3.7.
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
    method = c("auto", "virtualenv", "conda"),
    conda = "auto",
    version = "default",
    envname = "r-metaflow",
    extra_packages = NULL,
    restart_session = TRUE,
    python_version = NULL,
    new_env = TRUE,
    ...) {
  method <- match.arg(method)

  check_system()
  python_version <- check_python_version(python_version)

  prepare_environment(new_env, envname, method, python_version)

  packages <- prepare_packages(version, extra_packages, method)

  # Determine if pip should be used
  pip <- if (method == "conda") FALSE else TRUE

  # Set Conda channel if method is 'conda'
  channel <- if (method == "conda") "conda-forge" else NULL

  if (method == "conda") {
    cli::cli_alert_info(paste0(
      "Installing Metaflow via Conda in the environment '",
      envname,
      "'."
    ))
  } else {
    cli::cli_alert_info(paste0(
      "Installing Metaflow via pip in the environment '",
      envname,
      "'."
    ))
  }

  suppressMessages({
    reticulate::py_install(
      packages = packages,
      envname = envname,
      method = method,
      conda = conda,
      python_version = python_version,
      pip = pip,
      channel = channel,
      ...
    )

    cli::cli_alert_success("Metaflow installation complete.")

    if (restart_session &&
          requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::hasFun("restartSession")) {
      rstudioapi::restartSession()
    }
  })

  invisible(NULL)
}

#' @keywords internal
check_system <- function() {
  if (Sys.info()[["sysname"]] == "Windows") {
    cli::cli_abort(c(
      "Metaflow installation is not supported on Windows.",
      "Installation is only available for Mac and Linux systems."
    ))
  }
}

#' @keywords internal
check_python_version <- function(python_version) {
  if (is.null(python_version)) {
    return(">=3.7")
  } else if (numeric_version(python_version) < "3.7") {
    cli::cli_abort("Python version must be 3.7 or higher for Metaflow.")
  }
  return(python_version)
}

#' @keywords internal
check_existing_environment <- function(envname, method) {
  if (method == "conda" && reticulate::condaenv_exists(envname)) {
    return(TRUE)
  } else if (method != "conda" && reticulate::virtualenv_exists(envname)) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
prepare_environment <- function(new_env, envname, method, python_version) {
  if (new_env && check_existing_environment(envname, method)) {
    cli::cli_alert_info("Removing existing environment: {envname}")
    if (method == "conda") {
      reticulate::conda_remove(envname)
      reticulate::conda_create(
        envname,
        packages = "python",
        python_version = python_version
      )
    } else {
      reticulate::virtualenv_remove(envname, confirm = FALSE)
      reticulate::virtualenv_create(envname, python = python_version)
    }
  } else if (new_env) {
    if (method == "conda") {
      reticulate::conda_create(
        envname,
        packages = "python",
        python_version = python_version
      )
    } else {
      reticulate::virtualenv_create(envname, python = python_version)
    }
  } else {
    cli::cli_alert_info("Using existing environment: {envname}")
  }
}

check_existing_environment <- function(envname, method) {
  if (method == "conda") {
    return(reticulate::condaenv_exists(envname))
  } else {
    return(reticulate::virtualenv_exists(envname))
  }
}

#' @keywords internal
prepare_packages <- function(version, extra_packages, method) {
  mf_package_spec <- parse_metaflow_version(version, method)
  unique(c(mf_package_spec, as.character(extra_packages)))
}

#' @keywords internal
parse_metaflow_version <- function(version, method) {
  if (is.null(version) || is.na(version) ||
        version %in% c("", "default", "latest")) {
    return("metaflow")  # Install the latest version
  }

  version <- as.character(version)

  valid_version_pattern <- "^[><=]*\\s*\\d+(\\.\\d+)*$"
  if (!grepl(valid_version_pattern, version)) {
    cli::cli_abort("Invalid version specified")
  }

  if (method == "conda") {
    # For Conda, use '=' for exact version
    if (!grepl("[><=]", version)) {
      version <- sprintf("=%s", version)
    }
  } else {
    # For pip, use '==' for exact version
    if (!grepl("[><=]", version)) {
      version <- sprintf("==%s", version)
    }
  }

  paste0("metaflow", version)
}
