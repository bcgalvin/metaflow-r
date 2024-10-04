# Combine globals
.globals <- new.env(parent = emptyenv())
mf <- NULL

#' Handle to the `metaflow` module
#'
#' @export
#' @return Module(metaflow)
mf <- NULL

.onLoad <- function(libname, pkgname) {
  .import_metaflow()
}

.onAttach <- function(libname, pkgname) {
  tryCatch({
    if (is_metaflow_available()) {
      packageStartupMessage(
        "Metaflow Python module successfully loaded.\n",
        "Metaflow version: ", .globals[["mf"]][["__version__"]], "\n",
        "Python executable: ", reticulate::py_config()[["python"]]
      )
    } else {
      packageStartupMessage(
        "Metaflow Python module not loaded. ",
        "Use install_metaflow() to install it."
      )
    }
  }, error = function(e) {
    packageStartupMessage("Error during package attachment: ", e[["message"]])
  })
}

.import_metaflow <- function() {
  mf <<- reticulate::import(
    "metaflow",
    delay_load = list(
      priority = 5L,
      on_load = function() {},
      on_error = function(e) NULL
    )
  )
  if (!is.null(mf)) {
    assign("mf", mf, envir = .globals)
  }
}

is_metaflow_available <- function() {
  !is.null(.globals[["mf"]]) && !is.null(.globals[["mf"]][["__version__"]])
}
