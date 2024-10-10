#' @keywords internal
as_r_value <- function(x) {
  if (inherits(x, "python.builtin.object")) {
    reticulate::py_to_r(x)
  } else {
    x
  }
}

#' @keywords internal
check_system <- function() {
  if (Sys.info()[["sysname"]] == "Windows") {
    cli::cli_abort(
      c(
        "Metaflow installation is not supported on Windows.",
        i = "Installation is only available for Mac and Linux systems."
      )
    )
  }
}

#' @keywords internal
check_python_version <- function(python_version) {
  if (is.null(python_version)) {
    return(">=3.8")
  }
  checkmate::assert_string(python_version)
  if (numeric_version(python_version) < "3.8") {
    stop("Python version must be 3.8 or higher for Metaflow.")
  }
  return(python_version)
}

#' @keywords internal
check_existing_environment <- function(envname, method) {
  if (method == "conda") {
    return(reticulate::condaenv_exists(envname))
  } else {
    return(reticulate::virtualenv_exists(envname))
  }
}

#' @keywords internal
prepare_environment <- function(new_env, envname, python_version, method = c("conda", "virtualenv")) {
  method <- match.arg(method)

  env_exists <- switch(method,
    conda = reticulate::condaenv_exists(envname),
    virtualenv = reticulate::virtualenv_exists(envname)
  )

  if (new_env && env_exists) {
    cli::cli_inform("Removing existing environment: {envname}")

    remove_fn <- switch(method,
      conda = reticulate::conda_remove,
      virtualenv = function(env) reticulate::virtualenv_remove(env, confirm = FALSE)
    )
    remove_fn(envname)
  }

  if (new_env || !env_exists) {
    create_fn <- switch(method,
      conda = function(env, ver) reticulate::conda_create(env, packages = "python", python_version = ver),
      virtualenv = reticulate::virtualenv_create
    )
    if (method == "conda") {
      create_fn(envname, python_version)
    } else {
      create_fn(envname, python = python_version)
    }
  } else {
    cli::cli_inform("Using existing environment: {envname}")
  }
}

#' @keywords internal
prepare_packages <- function(version, extra_packages, method) {
  mf_package_spec <- parse_metaflow_version(version, method)
  unique(c(mf_package_spec, as.character(extra_packages)))
}

#' @keywords internal
is_default_version <- function(version) {
  checkmate::test_string(version, pattern = "^$") ||
    is.null(version) || is.na(version) ||
    version %in% c("default", "latest")
}

#' @keywords internal
validate_version_format <- function(version) {
  valid_version_pattern <- "^([><=]+\\s*)?\\d+(\\.\\d+)*$"
  checkmate::assert_true(grepl(valid_version_pattern, version),
    .var.name = "version"
  )
}

#' @keywords internal
format_version_for_method <- function(version, method) {
  if (grepl("[><=]", version)) {
    version
  } else if (method == "conda") {
    sprintf("=%s", version)
  } else {
    sprintf("==%s", version)
  }
}

#' @keywords internal
parse_metaflow_version <- function(version, method) {
  checkmate::assert_choice(method, c("conda", "virtualenv", "auto"))

  if (is_default_version(version)) {
    return("metaflow") # Install the latest version
  }

  checkmate::assert_string(version)
  validate_version_format(version)
  formatted_version <- format_version_for_method(version, method)

  paste0("metaflow", formatted_version)
}

#' @keywords internal
#' @keywords internal
ensure_metaflow <- function() {
  # Reset mf in case of re-import
  .globals[["mf"]] <- NULL

  # Check if METAFLOW_PYTHON is set
  mf_python <- Sys.getenv("METAFLOW_PYTHON", unset = NA)
  if (!is.na(mf_python) && nzchar(mf_python)) {
    # Set the Python executable
    tryCatch(
      {
        reticulate::use_python(python = mf_python, required = TRUE)
      },
      error = function(e) {
        cli::cli_abort(
          "Python executable not found at path: '{.path {mf_python}}'.",
          x = paste0(
            "Ensure that {.envvar METAFLOW_PYTHON} points to a valid ",
            "Python executable."
          )
        )
      }
    )

    # Get Python configuration
    py_config <- reticulate::py_config()
    py_version <- numeric_version(py_config[["version"]])
    if (py_version < "3.8") {
      cli::cli_abort(
        c(
          "The Python specified by {.envvar METAFLOW_PYTHON} has version",
          "{py_version}, which is unsupported."
        ),
        x = "Metaflow requires Python >= 3.8.",
        i = "Please install a compatible Python version."
      )
    }

    # Check if Metaflow is available
    if (!reticulate::py_module_available("metaflow")) {
      cli::cli_abort(
        c(
          "Metaflow is not installed in the Python environment",
          "specified by {.envvar METAFLOW_PYTHON}."
        ),
        i = "Use {.code install_metaflow()} to install Metaflow."
      )
    }

    # Import Metaflow
    mf_local <- reticulate::import("metaflow")

    # Get Metaflow version
    mf_version <- numeric_version(mf_local[["__version__"]])
    if (mf_version < "2.0") {
      cli::cli_abort(
        c(
          "Metaflow version {mf_version} is installed, but",
          "version >= 2.0 is required."
        ),
        x = "Please upgrade Metaflow to a compatible version."
      )
    }
    # Assign mf_local to .globals after all checks pass
    .globals[["mf"]] <- mf_local
    return(invisible(NULL))
  }

  # Proceed with default behavior when METAFLOW_PYTHON is not set
  mf_local <- tryCatch(
    {
      reticulate::import("metaflow", delay_load = FALSE)
    },
    error = function(e) {
      NULL
    }
  )

  # Assign mf to .globals if successfully loaded
  if (!is.null(mf_local)) {
    .globals[["mf"]] <- mf_local
  }
}

#' @keywords internal
is_metaflow_available <- function() {
  !is.null(.globals[["mf"]])
}
