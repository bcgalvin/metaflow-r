#' @export
print.S3Object <- function(x, ...) {
  if (reticulate::py_is_null_xptr(x$py_obj)) {
    cat("S3 Object: <NULL>\n")
    return(invisible(x))
  }
  cat("S3 Object:\n")
  cat("  URL:", tryCatch(x$url(), error = function(e) "Unknown"), "\n")
  cat("  Key:", tryCatch(x$key(), error = function(e) "Unknown"), "\n")
  cat("  Prefix:", tryCatch(x$prefix(), error = function(e) "Unknown"), "\n")
  cat("  Exists:", tryCatch(x$exists(), error = function(e) "Unknown"), "\n")
  cat("  Size:", tryCatch(x$size(), error = function(e) "Unknown"), "bytes\n")
  cat("  Downloaded:", tryCatch(x$downloaded(), error = function(e) "Unknown"), "\n")
  cat("  Has Info:", tryCatch(x$has_info(), error = function(e) "Unknown"), "\n")
  cat("  Encryption:", tryCatch(x$encryption(), error = function(e) "Unknown"), "\n")
  invisible(x)
}

#' @export
summary.S3Object <- function(object, ...) {
  list(
    url = object$url(),
    key = object$key(),
    prefix = object$prefix(),
    exists = object$exists(),
    size = object$size(),
    downloaded = object$downloaded(),
    content_type = object$content_type(),
    last_modified = object$last_modified(),
    has_info = object$has_info(),
    encryption = object$encryption(),
    range_info = object$range_info()
  )
}

#' @export
str.S3Object <- function(object, ...) {
  cat("S3 Object <", object$url(), ">\n", sep = "")
  cat("  Size:", object$size(), "bytes\n")
  cat("  Content Type:", object$content_type(), "\n")
}

#' @export
as.data.frame.S3Object <- function(x, row.names = NULL, optional = FALSE, ...) {
  safe_access <- purrr::safely(~ x[[.x]](), otherwise = NA)
  properties <- c(
    "url", "key", "prefix", "exists", "size", "downloaded",
    "content_type", "last_modified", "has_info", "encryption"
  )
  df <- purrr::map_dfc(properties, ~ tibble::tibble(!!.x := safe_access(.x)$result))
  range_info <- x$range_info()
  range_df <- tibble::tibble(
    range_total_size = if (!is.null(range_info)) range_info$total_size else NA_real_,
    range_request_offset = if (!is.null(range_info)) range_info$request_offset else NA_real_,
    range_request_length = if (!is.null(range_info)) range_info$request_length else NA_real_
  )
  tibble::as_tibble(purrr::list_modify(df, !!!range_df))
}

#' @export
length.S3Object <- function(x) {
  if (x$downloaded()) {
    reticulate::py_len(x$py_obj$text)
  } else {
    x$size()
  }
}

#' @export
as.character.S3Object <- function(x, ...) {
  if (x$downloaded()) {
    reticulate::py_str(x$py_obj$text)
  } else {
    cli::cli_abort("Object not downloaded. Use get() first.")
  }
}
