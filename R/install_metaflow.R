#' @title Install Metaflow
#'
#' @description Install Metaflow python

#' @param version for specific version of Metaflow, e.g. '2.7.9'
#' @param ... other arguments passed to [reticulate::py_install()].
#' @param restart_session Restart R session after installing (note this will only occur within RStudio).
#' @param from_git install the recent GitHub version of Metaflow
#' @export
install_metaflow <- function(version = NULL, ..., restart_session = TRUE,
                             from_git = FALSE) {
  metaflow_py_install <- function(pkgs) {
    system(paste(
      reticulate::py_discover_config()[["python"]],
      "-m pip install", paste(pkgs, collapse = " ")
    ))
  }


  if (is.null(version) & !from_git) {
    module_string <- paste0("metaflow==", "2.7.9")
  } else if (!is.null(version)) {
    module_string <- paste0("metaflow==", version)
  } else if (isTRUE(from_git)) {
    module_string <- paste0("git+https://github.com/netflix/metaflow.git")
  }

  invisible(reticulate::py_config())
  py_path <- Sys.which("python") %>%
    as.character()

  metaflow_py_install(module_string)

  invisible(reticulate::use_python(py_path, required = TRUE))

  fun <- function() {
    py_path <- gsub(py_path,
      replacement = "/", pattern = "\\",
      fixed = TRUE
    )
    error <- crayon::red$bold
    error2 <- crayon::red$bold
    cat(error2("Metaflow is installed here:"), paste("\"",
      error(py_path), "\"",
      sep = ""
    ))
  }

  fun()

  if (restart_session && rstudioapi::hasFun("restartSession")) {
    rstudioapi::restartSession()
  }
}
