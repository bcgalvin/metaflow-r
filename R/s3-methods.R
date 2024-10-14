#' @importFrom purrr map_dfc safely
#' @importFrom tibble tibble as_tibble
#' @importFrom cli cli_abort
NULL

#' Print method for s3_object
#'
#' @param x An s3_object
#' @param ... Additional arguments passed to print
#' @export
print.s3_object <- function(x, ...) {
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

#' Summary method for s3_object
#'
#' @param object An s3_object
#' @param ... Additional arguments passed to summary
#' @export
summary.s3_object <- function(object, ...) {
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

#' Str method for s3_object
#'
#' @param object An s3_object
#' @param ... Additional arguments passed to str
#' @export
str.s3_object <- function(object, ...) {
  cat("S3 Object <", object$url(), ">\n", sep = "")
  cat("  Size:", object$size(), "bytes\n")
  cat("  Content Type:", object$content_type(), "\n")
}

#' Convert s3_object to data.frame
#'
#' @param x An s3_object
#' @param row.names NULL or a character vector giving the row names for the data frame
#' @param optional A logical value. If TRUE, setting row names and converting column names is optional
#' @param ... Additional arguments passed to as.data.frame
#' @export
as.data.frame.s3_object <- function(x, row.names = NULL, optional = FALSE, ...) {
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

#' Length method for s3_object
#'
#' @param x An s3_object
#' @export
#' @importFrom reticulate py_len
length.s3_object <- function(x) {
  if (x$downloaded()) {
    reticulate::py_len(x$py_obj$text)
  } else {
    x$size()
  }
}

#' Convert s3_object to character
#'
#' @param x An s3_object
#' @param ... Additional arguments passed to as.character
#' @export
#' @importFrom reticulate py_str
as.character.s3_object <- function(x, ...) {
  if (x$downloaded()) {
    reticulate::py_str(x$py_obj$text)
  } else {
    cli::cli_abort("Object not downloaded. Use get() first.")
  }
}
