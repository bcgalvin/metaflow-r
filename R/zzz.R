# Initialize package-level global variables
.globals <- new.env(parent = emptyenv())

# Package-level state
.globals[["mf"]] <- NULL
.globals[["mf_profile"]] <- NULL
.globals[["mf_profile_name"]] <- NULL
.globals[["mf_profile_path"]] <- NULL

.onLoad <- function(libname, pkgname) {
  ensure_metaflow()
  load_default_profile()
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
    cli::cli_div(theme = list(
      span.red = list(color = "red"),
      ul = list("margin-left" = 4L)
    ))
    cli::cli_alert_info("Python executable: {python_path}")
    cli::cli_alert_success(
      "Metaflow successfully loaded with these installed versions:"
    )

    # Define labels and versions
    python_label <- "Python"
    r_label <- paste0("R")
    python_version <- metaflow_version()
    r_version <- packageVersion("metaflow")

    # Format labels for alignment
    formatted_python_label <- sprintf("%-*s", 8L, python_label)
    formatted_r_label <- sprintf("%-*s", 8L, r_label)

    cli::cli_ul()
    cli::cli_li("{.br_white {formatted_python_label}}{.red {python_version}}")
    cli::cli_li("{.br_white {formatted_r_label}} {.red {r_version}}")
    cli::cli_end()
  } else {
    cli::cli_alert_warning(c(
      "Metaflow Python module not loaded.",
      "Use {.code install_metaflow()} to install it."
    ))
  }
}
