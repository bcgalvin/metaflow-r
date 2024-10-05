# package level state
.globals <- new.env(parent = emptyenv())
.globals[["mf"]] <- NULL


.onLoad <- function(libname, pkgname) {
  ensure_metaflow()
  return(NULL)
}

.onAttach <- function(libname, pkgname) {
  if (is_metaflow_available()) {
    mf_python <- Sys.getenv("METAFLOW_PYTHON", unset = NA)
    python_path <- if (!is.na(mf_python) && nzchar(mf_python)) {
      mf_python
    } else {
      reticulate::py_config()[["python"]]
    }
    packageStartupMessage(
      "Metaflow successfully loaded with these installed versions:",
      "\n   Python ", metaflow_version(),
      "\n   R      ", packageVersion("metaflow"),
      "\nPython executable: ", python_path
    )
  } else {
    packageStartupMessage(
      "Metaflow Python module not loaded. ",
      "Use install_metaflow() to install it."
    )
  }
}

#' @keywords internal
#' @keywords internal
ensure_metaflow <- function() {
  # Reset mf in case of re-import
  .globals[["mf"]] <- NULL

  # Use withr to temporarily set environment variables during function execution
  new_env <- list()
  if (length(new_env) > 0L) {
    withr::local_envvar(new = new_env)
  }

  # Check if METAFLOW_PYTHON is set
  mf_python <- Sys.getenv("METAFLOW_PYTHON", unset = NA)
  if (!is.na(mf_python) && nzchar(mf_python)) {
    # Use the specified Python executable
    py_config <- tryCatch(
      {
        reticulate::py_config(python = mf_python)
      },
      error = function(e) {
        stop(
          "Python executable not found at path: ", mf_python, ".",
          "\nEnsure that METAFLOW_PYTHON points to a valid Python executable."
        )
      }
    )
    py_version <- numeric_version(py_config[["version"]])
    if (py_version < "3.8") {
      stop(
        "The Python specified by METAFLOW_PYTHON has version ", py_version, ".",
        "\nMetaflow requires Python >= 3.8."
      )
    }

    # Check if Metaflow is available
    if (reticulate::py_module_available("metaflow")) {
      # Import Metaflow
      mf_local <- reticulate::import("metaflow")

      # Get Metaflow version
      mf_version <- numeric_version(mf_local[["__version__"]])
      if (mf_version < "2.0") {
        stop(
          "Metaflow version ", mf_version,
          " is installed, but version >= 2.0 is required."
        )
      }
    } else {
      stop(
        "Metaflow is not installed in the Python environment ",
        "specified by METAFLOW_PYTHON."
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
