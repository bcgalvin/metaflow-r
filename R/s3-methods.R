#' @export
print.S3Object <- function(s3_object, ...) {
  if (reticulate::py_is_null_xptr(s3_object[["py_obj"]])) {
    cat("S3 Object: <NULL>\n")
    return(invisible(s3_object))
  }
  cat("S3 Object:\n")
  cat("  URL:", tryCatch(s3_object[["url"]], error = function(e) "Unknown"), "\n")
  cat("  Key:", tryCatch(s3_object[["key"]], error = function(e) "Unknown"), "\n")
  cat("  Prefix:", tryCatch(s3_object[["prefix"]], error = function(e) "Unknown"), "\n")
  cat("  Size:", tryCatch(s3_object[["size"]], error = function(e) "Unknown"), "bytes\n")
  cat("  Exists:", tryCatch(s3_object[["exists"]], error = function(e) "Unknown"), "\n")
  cat("  Downloaded:", tryCatch(s3_object[["downloaded"]], error = function(e) "Unknown"), "\n")
  cat("  Content Type:", tryCatch(s3_object[["content_type"]], error = function(e) "Unknown"), "\n")
  cat("  Last Modified:", tryCatch(s3_object[["last_modified"]], error = function(e) "Unknown"), "\n")
  cat("  Has Info:", tryCatch(s3_object[["has_info"]], error = function(e) "Unknown"), "\n")
  cat("  Encryption:", tryCatch(s3_object[["encryption"]], error = function(e) "Unknown"), "\n")
  invisible(s3_object)
}

#' @export
summary.S3Object <- function(s3_object, ...) {
  list(
    url = s3_object[["url"]],
    key = s3_object[["key"]],
    prefix = s3_object[["prefix"]],
    exists = s3_object[["exists"]],
    size = s3_object[["size"]],
    downloaded = s3_object[["downloaded"]],
    content_type = s3_object[["content_type"]],
    last_modified = s3_object[["last_modified"]],
    has_info = s3_object[["has_info"]],
    encryption = s3_object[["encryption"]],
    range_info = s3_object[["range_info"]]
  )
}

#' @export
str.S3Object <- function(s3_object, ...) {
  cat("S3 Object <", s3_object[["url"]], ">\n", sep = "")
  cat("  Size:", s3_object[["size"]], "bytes\n")
  cat("  Content Type:", s3_object[["content_type"]], "\n")
}

#' @export
as.character.S3Object <- function(s3_object, ...) {
  if (s3_object[["downloaded"]]) {
    s3_object[["text"]]
  } else {
    cli::cli_abort("Object not downloaded. Use get() first.")
  }
}
