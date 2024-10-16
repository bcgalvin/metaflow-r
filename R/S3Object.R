#' R6 Class Wrapper for Metaflow S3Object
#'
#' This class represents an S3 object returned by the s3_client.
#'
#' @export
#' @importFrom utils str
S3Object <- R6::R6Class(
  "S3Object",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field py_obj Python S3Object
    py_obj = NULL,

    #' @description
    #' Create a new S3Object
    #' @param py_obj Python S3Object to wrap
    initialize = function(py_obj) {
      if (reticulate::py_is_null_xptr(py_obj)) {
        cli::cli_warn("Received a NULL Python object")
        py_obj <<- NULL
      } else if (reticulate::py_has_attr(py_obj, "url")) {
        py_obj <<- py_obj
      } else {
        cli::cli_warn("Received an invalid Python object")
        py_obj <<- NULL
      }
    }
  ),
  active = list(
    #' @field url The S3 URL of the object
    url = function() py_obj[["url"]],

    #' @field key The S3 key of the object
    key = function() py_obj[["key"]],

    #' @field size The size of the object in bytes
    size = function() py_obj[["size"]],

    #' @field path The local path of the object if downloaded
    path = function() py_obj[["path"]],

    #' @field blob The object's content as bytes
    blob = function() py_obj[["blob"]],

    #' @field text The object's content as text
    text = function() py_obj[["text"]],

    #' @field metadata The object's metadata
    metadata = function() py_obj[["metadata"]],

    #' @field content_type The MIME type of the object
    content_type = function() py_obj[["content_type"]],

    #' @field downloaded Whether the object has been downloaded
    downloaded = function() convert_bool(py_obj[["downloaded"]]),

    #' @field exists Whether the object exists in S3
    exists = function() convert_bool(py_obj[["exists"]]),

    #' @field last_modified The last modified timestamp of the object
    last_modified = function() as.character(as.POSIXct(py_obj[["last_modified"]], origin = "1970-01-01", tz = "UTC")),

    #' @field prefix The S3 prefix of the object
    prefix = function() py_obj[["prefix"]],

    #' @field has_info Whether the object has additional info
    has_info = function() convert_bool(py_obj[["has_info"]]),

    #' @field encryption The encryption status of the object
    encryption = function() py_obj[["encryption"]],

    #' @field range_info Information about partial content requests
    range_info = function() {
      info <- py_obj[["range_info"]]
      if (!is.null(info)) {
        list(
          total_size = info[["total_size"]],
          request_offset = info[["request_offset"]],
          request_length = info[["request_length"]]
        )
      } else {
        NULL
      }
    }
  )
)

#' Convert Python boolean to R boolean
#'
#' @param value The value to convert
#' @return A logical value
#' @keywords internal
convert_bool <- function(value) {
  if (is.logical(value)) {
    return(value)
  } else if (is.null(value)) {
    return(FALSE)
  } else if (reticulate::is_py_object(value)) {
    return(reticulate::py_bool(value))
  } else {
    cli::cli_abort("Unexpected input type for boolean conversion")
  }
}
