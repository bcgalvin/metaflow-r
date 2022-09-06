onload_error <- function(e) {
  if (grepl("No module named metaflow", e$message)) {
    stop("Use metaflow::install_metaflow() to install metaflow python ",
      call. = FALSE
    )
  } else {
    stop(e$message, call. = FALSE)
  }
}

.onLoad <- function(libname, pkgname) {
  metaflow <<- reticulate::import(
    "metaflow",
    delay_load = list(
      priority = 10,
      environment = "r-metaflow",
      on_error = onload_error
    )
  )
  invisible(NULL)
}
