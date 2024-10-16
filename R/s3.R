#' R6 Class Wrapper for Metaflow S3 Client
#'
#' This class provides an R interface to the Metaflow S3 client functionality.
#'
#' @export
S3 <- R6::R6Class(
  "S3",
  portable = FALSE,
  cloneable = FALSE,
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
      py_s3 <<- metaflow[["S3"]](...)
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
      py_obj <- py_s3[["get"]](
        key,
        return_missing = return_missing,
        return_info = return_info,
        ...
      )
      if (identical(py_obj, reticulate::py_none())) {
        return(NULL)
      }
      s3_object[["new"]](reticulate::py_to_r(py_obj))
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
      reticulate::py_call(py_s3[["put"]], key, reticulate::r_to_py(obj),
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
      py_objs <- py_s3[["get_many"]](
        keys,
        return_missing = return_missing,
        return_info = return_info
      )
      purrr::map(py_objs, s3_object[["new"]])
    },

    #' @description
    #' Put many objects to S3
    #' @param key_objs List of key-object pairs or S3PutObjects
    #' @param overwrite If TRUE, overwrite existing objects
    #' @return List of (key, url) pairs for uploaded objects
    put_many = function(key_objs, overwrite = TRUE) {
      py_s3[["put_many"]](key_objs, overwrite = overwrite)
    },

    #' @description
    #' List the next level of paths in S3
    #' @param keys List of paths to list
    #' @return List of S3Object instances
    list_paths = function(keys = NULL) {
      py_objs <- py_s3[["list_paths"]](keys)
      purrr::map(py_objs, function(obj) if (reticulate::py_is_null_xptr(obj)) NULL else s3_object[["new"]](obj))
    },

    #' @description
    #' List all objects recursively under the given prefixes
    #' @param keys List of prefixes to list
    #' @return List of S3Object instances
    list_recursive = function(keys = NULL) {
      py_objs <- py_s3[["list_recursive"]](keys)
      if (inherits(py_objs, "python.builtin.iterator")) {
        result <- reticulate::iterate(py_objs, s3_object[["new"]])
      } else if (is.list(py_objs)) {
        result <- purrr::map(py_objs, s3_object[["new"]])
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
        py_obj <- py_s3[["info"]](key, return_missing = return_missing)
      })
      if (nzchar(output)) {
        cli::cli_inform("Python output: {output}")
      }
      s3_object[["new"]](py_obj)
    },

    #' @description
    #' Get metadata about many objects in S3 in parallel
    #' @param keys List of object keys
    #' @param return_missing If TRUE, return missing objects instead of raising
    #'   exceptions
    #' @return List of S3Object instances
    info_many = function(keys, return_missing = FALSE) {
      py_objs <- py_s3[["info_many"]](keys, return_missing = return_missing)
      purrr::map(py_objs, s3_object[["new"]])
    },

    #' @description
    #' Get all objects under the prefix set in the S3 constructor
    #' @param return_info If TRUE, fetch content-type and user metadata
    #' @return List of S3Object instances
    get_all = function(return_info = FALSE) {
      py_objs <- py_s3[["get_all"]](return_info = return_info)
      purrr::map(py_objs, s3_object[["new"]])
    },

    #' @description
    #' Get many objects from S3 recursively in parallel
    #' @param keys List of prefixes to download recursively
    #' @param return_info If TRUE, fetch content-type and user metadata
    #' @return List of S3Object instances
    get_recursive = function(keys, return_info = FALSE) {
      py_objs <- py_s3[["get_recursive"]](keys, return_info = return_info)
      purrr::map(py_objs, s3_object[["new"]])
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
      result <- py_s3[["put_files"]](py_key_paths, overwrite = overwrite)

      # Convert the result back to R
      reticulate::py_to_r(result)
    },

    #' @description
    #' Close the S3 client and delete temporary files
    close = function() {
      py_s3[["close"]]()
    }
  )
)
