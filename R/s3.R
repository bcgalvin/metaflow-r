#' R6 Class Wrapper for Metaflow S3 Client
#'
#' This class provides an R interface to the Metaflow S3 client functionality.
#'
#' @export
S3 <- R6::R6Class(
  "S3",
  public = list(
    #' @field py_s3 Python S3 object
    py_s3 = NULL,

    #' @description
    #' Create a new S3Client object
    #' @param ... Additional arguments passed to the Python S3 constructor
    initialize = function(...) {
      if (!reticulate::py_module_available("metaflow")) {
        cli::cli_abort("The 'metaflow' Python module is not available. Please install it.")
      }
      metaflow <- reticulate::import("metaflow", delay_load = TRUE)
      self$py_s3 <- metaflow$S3(...)
    },

    #' @description
    #' Get a single object from S3
    #' @param key Object key
    #' @param return_missing If TRUE, return a missing object instead of raising
    #'   an exception
    #' @param return_info If TRUE, fetch content-type and user metadata
    #' @param ... Additional arguments passed to the Python get method
    #' @return An S3Object
    get = function(key, return_missing = FALSE, return_info = TRUE, ...) {
      py_obj <- self$py_s3$get(
        key,
        return_missing = return_missing,
        return_info = return_info,
        ...
      )
      if (identical(py_obj, reticulate::py_none())) {
        return(NULL)
      }
      S3Object$new(reticulate::py_to_r(py_obj))
    },

    #' @description
    #' Put a single object to S3
    #' @param key Object key
    #' @param obj Object to store
    #' @param overwrite If TRUE, overwrite existing object
    #' @param content_type MIME type for the object
    #' @param metadata A list of additional headers to be stored as metadata
    #' @param ... Additional arguments passed to the Python put method
    #' @return URL of the stored object
    put = function(key, obj, overwrite = TRUE, content_type = NULL,
                   metadata = NULL, ...) {
      reticulate::py_call(self$py_s3$put, key, reticulate::r_to_py(obj),
        overwrite = overwrite,
        content_type = content_type,
        metadata = reticulate::r_to_py(metadata),
        ...
      )
    },

    #' @description
    #' Get many objects from S3 in parallel
    #' @param keys List of object keys
    #' @param return_missing If TRUE, return missing objects instead of raising
    #'   exceptions
    #' @param return_info If TRUE, fetch content-type and user metadata
    #' @return List of S3Object instances
    get_many = function(keys, return_missing = FALSE, return_info = TRUE) {
      py_objs <- self$py_s3$get_many(
        keys,
        return_missing = return_missing,
        return_info = return_info
      )
      purrr::map(py_objs, S3Object$new)
    },

    #' @description
    #' Put many objects to S3
    #' @param key_objs List of key-object pairs or S3PutObjects
    #' @param overwrite If TRUE, overwrite existing objects
    #' @return List of (key, url) pairs for uploaded objects
    put_many = function(key_objs, overwrite = TRUE) {
      self$py_s3$put_many(key_objs, overwrite = overwrite)
    },

    #' @description
    #' List the next level of paths in S3
    #' @param keys List of paths to list
    #' @return List of S3Object instances
    list_paths = function(keys = NULL) {
      py_objs <- self$py_s3$list_paths(keys)
      purrr::map(py_objs, ~ if (reticulate::py_is_null_xptr(.x)) NULL else S3Object$new(.x))
    },

    #' @description
    #' List all objects recursively under the given prefixes
    #' @param keys List of prefixes to list
    #' @return List of S3Object instances
    list_recursive = function(keys = NULL) {
      py_objs <- self$py_s3$list_recursive(keys)
      if (inherits(py_objs, "python.builtin.iterator")) {
        result <- reticulate::iterate(py_objs, S3Object$new)
      } else if (is.list(py_objs)) {
        result <- purrr::map(py_objs, S3Object$new)
      } else {
        cli::cli_abort("Unexpected return type from list_recursive")
      }
      purrr::compact(result)
    },

    #' @description
    #' Get metadata about a single object in S3
    #' @param key Object key
    #' @param return_missing If TRUE, return a missing object instead of raising
    #'   an exception
    #' @return An S3Object
    info = function(key = NULL, return_missing = FALSE) {
      output <- py_capture_output({
        py_obj <- self$py_s3$info(key, return_missing = return_missing)
      })
      if (nzchar(output)) {
        cli::cli_inform("Python output: {output}")
      }
      S3Object$new(py_obj)
    },

    #' @description
    #' Get metadata about many objects in S3 in parallel
    #' @param keys List of object keys
    #' @param return_missing If TRUE, return missing objects instead of raising
    #'   exceptions
    #' @return List of S3Object instances
    info_many = function(keys, return_missing = FALSE) {
      py_objs <- self$py_s3$info_many(keys, return_missing = return_missing)
      purrr::map(py_objs, S3Object$new)
    },

    #' @description
    #' Get all objects under the prefix set in the S3 constructor
    #' @param return_info If TRUE, fetch content-type and user metadata
    #' @return List of S3Object instances
    get_all = function(return_info = FALSE) {
      py_objs <- self$py_s3$get_all(return_info = return_info)
      purrr::map(py_objs, S3Object$new)
    },

    #' @description
    #' Get many objects from S3 recursively in parallel
    #' @param keys List of prefixes to download recursively
    #' @param return_info If TRUE, fetch content-type and user metadata
    #' @return List of S3Object instances
    get_recursive = function(keys, return_info = FALSE) {
      py_objs <- self$py_s3$get_recursive(keys, return_info = return_info)
      purrr::map(py_objs, S3Object$new)
    },

    #' @description
    #' Upload many local files to S3
    #' @param key_paths List of key-path pairs or S3PutObjects
    #' @param overwrite If TRUE, overwrite existing objects
    #' @return List of (key, url) pairs for uploaded files
    put_files = function(key_paths, overwrite = TRUE) {
      # Convert R list to Python list of tuples or S3PutObjects
      py_key_paths <- reticulate::r_to_py(key_paths)

      # Call the Python method
      result <- self$py_s3$put_files(py_key_paths, overwrite = overwrite)

      # Convert the result back to R
      reticulate::py_to_r(result)
    },

    #' @description
    #' Close the S3 client and delete temporary files
    close = function() {
      self$py_s3$close()
    }
  )
)

#' R6 Class Wrapper for Metaflow S3Object
#'
#' This class represents an S3 object returned by the s3_client.
#'
#' @export
#' @importFrom utils str
S3Object <- R6::R6Class(
  "S3Object",
  public = list(
    #' @field py_obj Python S3Object
    py_obj = NULL,

    #' @description
    #' Convert a value to a boolean, handling both R logical and Python boolean objects
    #' @param value The value to convert
    #' @return A logical value
    #' @examples
    #' s3obj$convert_bool(TRUE)  # Returns TRUE
    #' s3obj$convert_bool(get_py_false())  # Returns FALSE
    #' s3obj$convert_bool(NULL)  # Returns FALSE
    #' \dontrun{
    #' s3obj$convert_bool("not a boolean")  # Throws an error
    #' }
    convert_bool = function(value) {
      if (is.logical(value)) {
        return(value)
      } else if (is.null(value)) {
        return(FALSE)
      } else if (reticulate::is_py_object(value)) {
        return(reticulate::py_bool(value))
      } else {
        cli::cli_abort("Unexpected input type for boolean conversion")
      }
    },

    #' @description
    #' Create a new S3Object
    #' @param py_obj Python S3Object
    initialize = function(py_obj) {
      if (reticulate::py_is_null_xptr(py_obj)) {
        cli::cli_warn("Received a NULL Python object")
        self$py_obj <- NULL
      } else if (reticulate::py_has_attr(py_obj, "url")) {
        self$py_obj <- py_obj
      } else {
        cli::cli_warn("Received an invalid Python object")
        self$py_obj <- NULL
      }
    },

    #' @description
    #' Get the URL of the S3 object
    #' @return URL as a string
    url = function() {
      self$py_obj$url
    },

    #' @description
    #' Get the content of the S3 object as text
    #' @return Content as a string
    text = function() {
      self$py_obj$text
    },

    #' @description
    #' Get the size of the S3 object
    #' @return Size in bytes
    size = function() {
      self$py_obj$size
    },

    #' @description
    #' Check if the object exists in S3
    #' @return Boolean indicating existence
    exists = function() {
      self$convert_bool(self$py_obj$exists)
    },

    #' @description
    #' Check if the object has been downloaded
    #' @return Boolean indicating if downloaded
    downloaded = function() {
      self$convert_bool(self$py_obj$downloaded)
    },

    #' @description
    #' Get the local path of the downloaded object
    #' @return Local path as a string
    path = function() {
      self$py_obj$path
    },

    #' @description
    #' Get the content of the S3 object as a byte string
    #' @return The decoded content of the Python bytes object
    #' @note This method uses `decode()` on the Python bytes object.
    blob = function() {
      self$py_obj$blob$decode()
    },

    #' @description
    #' Get the content type of the S3 object
    #' @return Content type as a string
    content_type = function() {
      self$py_obj$content_type
    },

    #' @description
    #' Get the metadata of the S3 object
    #' @return Metadata as a list
    metadata = function() {
      self$py_obj$metadata
    },

    #' @description
    #' Get the last modified timestamp of the S3 object
    #' @return Last modified timestamp as a POSIXct object
    last_modified = function() {
      as.POSIXct(self$py_obj$last_modified, origin = "1970-01-01", tz = "UTC")
    },

    #' @description
    #' Get the prefix requested that matches this object
    #' @return Prefix as a string
    prefix = function() {
      self$py_obj$prefix
    },

    #' @description
    #' Get the key corresponding to the get call that produced this object
    #' @return Key as a string
    key = function() {
      self$py_obj$key
    },

    #' @description
    #' Check if this S3Object contains additional metadata
    #' @return Boolean indicating if additional metadata is available
    #' @note This is a read-only property. It cannot be set directly.
    has_info = function() {
      self$convert_bool(self$py_obj$has_info)
    },

    #' @description
    #' Get the encryption type of the S3 object
    #' @return Encryption type as a string or NULL if not defined
    encryption = function() {
      self$py_obj$encryption
    },

    #' @description
    #' Get information about a partially downloaded object
    #' @return A list with total_size, request_offset, and request_length, or NULL if not applicable
    range_info = function() {
      info <- self$py_obj$range_info
      if (!is.null(info)) {
        list(
          total_size = info$total_size,
          request_offset = info$request_offset,
          request_length = info$request_length
        )
      } else {
        NULL
      }
    }
  )
)
