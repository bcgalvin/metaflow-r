#' S3Client Class
#'
#' @description
#' An R6 class that manages the S3 client for AWS authentication and access.
#'
#' @export
S3Client <- R6::R6Class("S3Client",
  public = list(
    #' @description
    #' Create a new S3Client object.
    #' @param s3_role_arn Optional character string specifying the AWS role ARN.
    #' @param s3_session_vars Optional list of session variables.
    #' @param s3_client_params Optional list of client parameters.
    initialize = function(s3_role_arn = NULL, s3_session_vars = NULL, s3_client_params = NULL) {
      private$s3_role <- s3_role_arn
      private$s3_session_vars <- s3_session_vars
      private$s3_client_params <- s3_client_params
      private$reset_client()
    },

    #' @description
    #' Get the S3 client.
    #' @return The S3 client object.
    client = function() {
      if (is.null(private$s3_client)) {
        private$reset_client()
      }
      private$s3_client
    },

    #' @description
    #' Get the error associated with the S3 client.
    #' @return The error object, if any.
    error = function() {
      if (is.null(private$s3_error)) {
        private$reset_client()
      }
      private$s3_error
    },

    #' @description
    #' Set the AWS role for the S3 client.
    #' @param role Character string specifying the AWS role ARN.
    set_role = function(role) {
      private$s3_role <- role
      private$reset_client()
    },

    #' @description
    #' Set the session variables for the S3 client.
    #' @param session_vars List of session variables.
    set_session_vars = function(session_vars) {
      private$s3_session_vars <- session_vars
      private$reset_client()
    },

    #' @description
    #' Set the client parameters for the S3 client.
    #' @param client_params List of client parameters.
    set_client_params = function(client_params) {
      private$s3_client_params <- client_params
      private$reset_client()
    }
  ),
  private = list(
    s3_client = NULL,
    s3_error = NULL,
    s3_role = NULL,
    s3_session_vars = NULL,
    s3_client_params = NULL,
    reset_client = function() {
      result <- get_s3_client(
        s3_role_arn = private$s3_role,
        s3_session_vars = private$s3_session_vars,
        s3_client_params = private$s3_client_params
      )
      private$s3_client <- result$client
      private$s3_error <- result$error
    }
  )
)

#' Get S3 Client
#'
#' @param s3_role_arn Optional character string specifying the AWS role ARN.
#' @param s3_session_vars Optional list of session variables.
#' @param s3_client_params Optional list of client parameters.
#' @return A list containing the S3 client and any error that occurred.
get_s3_client <- function(s3_role_arn = NULL, s3_session_vars = NULL, s3_client_params = NULL) {
  tryCatch(
    {
      client <- S3Client(
        s3_role_arn = s3_role_arn,
        s3_session_vars = s3_session_vars,
        s3_client_params = s3_client_params
      )
      list(client = client, error = NULL)
    },
    error = function(e) {
      list(client = NULL, error = e)
    }
  )
}


#' S3Object Class
#'
#' @description
#' Represents a path or an object in S3, with an optional local copy.
#' S3Objects are not instantiated directly, but they are returned
#' by many methods of the S3Client.
#'
#' @export
S3Object <- R6::R6Class("S3Object",
  public = list(
    #' @field exists Logical indicating if this key corresponds to an object in S3.
    exists = NULL,
    #' @field url Character string of the S3 location of the object.
    url = NULL,
    #' @field prefix Character string of the prefix requested that matches this object.
    prefix = NULL,
    #' @field key Character string corresponding to the key given to the get call that produced this object.
    key = NULL,
    #' @field size Integer size of the object in bytes (or NULL if the key doesn't correspond to an object in S3).
    size = NULL,
    #' @field has_info Logical indicating if this S3Object contains additional metadata.
    has_info = NULL,
    #' @field metadata List of user-defined metadata (or NULL if no metadata is defined).
    metadata = NULL,
    #' @field content_type Character string of the content-type of the S3 object (or NULL if not defined).
    content_type = NULL,
    #' @field last_modified Integer Unix timestamp of the last modified time.
    last_modified = NULL,
    #' @field encryption Character string of the server-side-encryption type (or NULL if not set).
    encryption = NULL,

    #' @description
    #' Initialize a new S3Object.
    #' @param py_obj A Python S3Object instance.
    initialize = function(py_obj) {
      self$exists <- py_obj$exists # Correctly access the property
      self$url <- py_obj$url
      self$prefix <- py_obj$prefix
      self$key <- py_obj$key
      self$size <- py_obj$size
      self$has_info <- py_obj$has_info
      self$metadata <- if (!is.null(py_obj$metadata)) reticulate::py_to_r(py_obj$metadata) else NULL
      self$content_type <- py_obj$content_type
      self$last_modified <- py_obj$last_modified
      self$encryption <- py_obj$encryption

      # Initialize range_info
      private$.range_info <- if (!is.null(py_obj$range_info)) {
        list(
          total_size = as.numeric(py_obj$range_info$total_size),
          request_offset = as.numeric(py_obj$range_info$request_offset),
          request_length = as.numeric(py_obj$range_info$request_length)
        )
      } else {
        NULL
      }
    },
    #' @description
    #' Print method for S3Object.
    #' @param ... Additional arguments passed to print (unused).
    print = function(...) {
      cat("S3Object:\n")
      cat("  exists:", self$exists, "\n")
      cat("  url:", self$url, "\n")
      cat("  prefix:", self$prefix, "\n")
      cat("  key:", self$key, "\n")
      cat("  size:", self$size, "\n")
      cat("  has_info:", self$has_info, "\n")
      cat("  content_type:", self$content_type, "\n")
      cat("  last_modified:", self$last_modified, "\n")
      cat("  encryption:", self$encryption, "\n")
      if (!is.null(private$.range_info)) {
        cat("  range_info:\n")
        cat("    total_size:", private$.range_info$total_size, "\n")
        cat("    request_offset:", private$.range_info$request_offset, "\n")
        cat("    request_length:", private$.range_info$request_length, "\n")
      } else {
        cat("  range_info: NULL\n")
      }
    },

    #' @description
    #' Check if the object is partially downloaded.
    #' @return Logical indicating if the object is partially downloaded.
    is_partially_downloaded = function() {
      !is.null(private$.range_info)
    }
  ),
  active = list(
    #' @field range_info A list containing information about partial downloads, or NULL.
    range_info = function(value) {
      if (missing(value)) {
        private$.range_info
      } else {
        stop("range_info is read-only")
      }
    }
  ),
  private = list(
    .range_info = NULL
  )
)

#' S3 Class
#'
#' @description
#' An R6 interface to Metaflow's S3 client. This class provides methods for
#' interacting with S3, including connecting, listing, and retrieving objects.
#' It uses the S3Client class internally for AWS authentication and client management.
#'
#' @importFrom R6 R6Class
#' @export
S3 <- R6::R6Class("S3",
  public = list(
    #' @description
    #' Create a new S3 object.
    #' @param tmproot Character string specifying the temporary directory root.
    #' @param bucket Optional character string specifying the S3 bucket.
    #' @param prefix Optional character string specifying the S3 prefix.
    #' @param run Optional Metaflow Run object or FlowSpec.
    #' @param s3root Optional character string specifying the S3 root path.
    #' @param verbose Logical, if TRUE, print detailed error messages.
    #' @param mf Metaflow object, defaults to .globals[["mf"]].
    initialize = function(tmproot = ".", bucket = NULL, prefix = NULL, run = NULL, s3root = NULL, verbose = FALSE, mf = .globals[["mf"]]) {
      private$tmproot <- tmproot
      private$bucket <- bucket
      private$prefix <- prefix
      private$run <- run
      private$s3root <- s3root
      private$s3_client <- S3Client$new()
      private$is_connected <- FALSE
      private$verbose <- verbose
      private$mf <- mf
    },

    #' @description
    #' Connect to the S3 client.
    #' @return Invisible self (for method chaining).
    connect = function() {
      if (!private$is_connected) {
        tryCatch(
          {
            private$py_s3 <- private$mf$S3(
              tmproot = private$tmproot,
              bucket = private$bucket,
              prefix = private$prefix,
              run = private$run,
              s3root = private$s3root,
              external_client = private$s3_client$client()
            )
            private$is_connected <- TRUE
          },
          error = function(e) {
            private$is_connected <- FALSE # Reset connection status on error
            error_message <- if (private$verbose) {
              c(
                "Error in connect()",
                i = conditionMessage(e),
                x = "Stack trace:",
                trace = capture.output(print(e))
              )
            } else {
              c(
                "Error in connect()",
                i = conditionMessage(e)
              )
            }
            cli::cli_abort(error_message)
          }
        )
      }
      invisible(self)
    },

    #' @description
    #' Set the AWS role for the S3 client.
    #' @param role Character string specifying the AWS role ARN.
    #' @return Invisible self (for method chaining).
    set_role = function(role) {
      private$s3_client$set_role(role)
      invisible(self)
    },

    #' @description
    #' Set the session variables for the S3 client.
    #' @param session_vars List of session variables.
    #' @return Invisible self (for method chaining).
    set_session_vars = function(session_vars) {
      private$s3_client$set_session_vars(session_vars)
      invisible(self)
    },

    #' @description
    #' Set the client parameters for the S3 client.
    #' @param client_params List of client parameters.
    #' @return Invisible self (for method chaining).
    set_client_params = function(client_params) {
      private$s3_client$set_client_params(client_params)
      invisible(self)
    },

    #' @description
    #' Print method for S3 objects.
    #' @param ... Additional arguments passed to print (unused).
    print = function(...) {
      cat("S3:\n")
      cat("  tmproot:", trimws(private$tmproot %||% "NULL"), "\n")
      cat("  bucket:", trimws(private$bucket %||% "NULL"), "\n")
      cat("  prefix:", trimws(private$prefix %||% "NULL"), "\n")
      cat("  s3root:", trimws(private$s3root %||% "NULL"), "\n")
      cat("  connected:", private$is_connected, "\n")
      invisible(self)
    },

    #' @description
    #' Get a single object from S3
    #' @param key Character string or S3GetObject representing the S3 key
    #' @param return_missing Logical, if TRUE, return missing keys as S3Objects with exists = FALSE
    #' @param return_info Logical, if TRUE, fetch content-type and user metadata
    #' @return An S3Object
    get = function(key = NULL, return_missing = FALSE, return_info = TRUE) {
      self$connect()

      result <- tryCatch(
        {
          py_obj <- private$py_s3$get(key, return_missing, return_info)
          S3Object$new(py_obj)
        },
        error = function(e) {
          if (inherits(e, "python.builtin.KeyError") && return_missing) {
            S3Object$new(list(exists = FALSE, key = key))
          } else {
            cli::cli_abort(c(
              "Error in get operation",
              i = "{e$message}"
            ))
          }
        }
      )

      result
    },

    #' @description
    #' Get multiple objects from S3
    #' @param keys Character vector or list of S3GetObjects representing the S3 keys
    #' @param return_missing Logical, if TRUE, return missing keys as S3Objects with exists = FALSE
    #' @param return_info Logical, if TRUE, fetch content-type and user metadata
    #' @return A list of S3Objects
    get_many = function(keys, return_missing = FALSE, return_info = TRUE) {
      self$connect()

      results <- tryCatch(
        {
          py_objs <- private$py_s3$get_many(keys, return_missing, return_info)
          private$convert_s3objects(py_objs)
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in get_many operation",
            i = "{e$message}"
          ))
        }
      )

      results
    },

    #' @description
    #' List the next level of paths in S3.
    #' @param keys Optional character vector of keys to list. If NULL, lists from the S3 root.
    #' @return A list of `S3Object` representations.
    list_paths = function(keys = NULL) {
      self$connect()
      result <- tryCatch(
        {
          if (is.null(keys)) {
            private$py_s3$list_paths()
          } else {
            private$py_s3$list_paths(keys)
          }
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in list_paths()",
            i = "{e$message}"
          ))
        }
      )
      private$convert_s3objects(result)
    },

    #' @description
    #' List all objects recursively under the given prefixes.
    #' @param keys Optional character vector of keys to list. If NULL, lists from the S3 root.
    #' @return A list of `S3Object` representations.
    list_recursive = function(keys = NULL) {
      self$connect()
      result <- tryCatch(
        {
          if (is.null(keys)) {
            private$py_s3$list_recursive()
          } else {
            private$py_s3$list_recursive(keys)
          }
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in list_recursive()",
            i = "{e$message}"
          ))
        }
      )
      private$convert_s3objects(result)
    },

    #' @description
    #' Get objects from S3 recursively
    #' @param keys Character vector of keys or prefixes to download recursively
    #' @param return_info Logical, if TRUE, fetch content-type and user metadata
    #' @return A list of S3Objects
    get_recursive = function(keys, return_info = FALSE) {
      self$connect()
      # Convert R vector to Python list if necessary
      py_keys <- reticulate::r_to_py(keys)
      results <- tryCatch(
        {
          py_objs <- private$py_s3$get_recursive(py_keys, return_info)
          private$convert_s3objects(py_objs)
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in get_recursive operation",
            i = "{e$message}"
          ))
        }
      )
      results
    },

    #' @description
    #' Get all objects under the prefix set in the S3 constructor
    #' @param return_info Logical, if TRUE, fetch content-type and user metadata
    #' @return A list of S3Objects
    get_all = function(return_info = FALSE) {
      self$connect()

      if (is.null(private$s3root)) {
        cli::cli_abort("Can't get_all() when S3 is initialized without a prefix")
      }

      results <- tryCatch(
        {
          py_objs <- private$py_s3$get_all(return_info)
          private$convert_s3objects(py_objs)
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in get_all operation",
            i = "{e$message}"
          ))
        }
      )

      results
    },

    #' @description
    #' Put a single object to S3
    #' @param key Character string or S3PutObject representing the S3 key
    #' @param obj Object to store in S3
    #' @param overwrite Logical, if TRUE, overwrite existing object
    #' @param content_type Optional MIME type for the object
    #' @param metadata Optional list of metadata to store with the object
    #' @return Character string of the URL of the stored object
    put = function(key, obj, overwrite = TRUE, content_type = NULL, metadata = NULL) {
      self$connect()
      result <- tryCatch(
        {
          # Convert R object to Python if necessary
          py_obj <- reticulate::r_to_py(obj)
          # Call Python put method
          url <- private$py_s3$put(key, py_obj, overwrite, content_type, metadata)
          as.character(url)
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in put operation",
            i = "{e$message}"
          ))
        }
      )
      result
    },

    #' @description
    #' Put multiple objects to S3
    #' @param key_objs List of key-object pairs or S3PutObjects
    #' @param overwrite Logical, if TRUE, overwrite existing objects
    #' @return List of character vectors, each containing key and URL of stored object
    put_many = function(key_objs, overwrite = TRUE) {
      self$connect()
      result <- tryCatch(
        {
          # Convert R list to Python list of tuples or S3PutObjects
          py_key_objs <- reticulate::r_to_py(purrr::map(key_objs, function(item) {
            if (is.list(item) && all(c("key", "value") %in% names(item))) {
              # It's an S3PutObject-like structure
              reticulate::dict(key = item$key, value = item$value)
            } else {
              # It's a key-value pair
              reticulate::tuple(item[[1L]], item[[2L]])
            }
          }))
          # Call Python put_many method
          py_result <- private$py_s3$put_many(py_key_objs, overwrite)
          # Convert Python result to R
          purrr::map(py_result, as.list)
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in put_many operation",
            i = "{e$message}"
          ))
        }
      )
      result
    },

    #' @description
    #' Put multiple files to S3
    #' @param key_paths List of key-path pairs or S3PutObjects
    #' @param overwrite Logical, if TRUE, overwrite existing objects
    #' @return List of character vectors, each containing key and URL of stored object
    put_files = function(key_paths, overwrite = TRUE) {
      self$connect()
      result <- tryCatch(
        {
          # Convert R list to Python list of tuples or S3PutObjects
          py_key_paths <- reticulate::r_to_py(purrr::map(key_paths, function(item) {
            if (is.list(item) && all(c("key", "path") %in% names(item))) {
              # It's an S3PutObject-like structure
              reticulate::dict(key = item$key, path = item$path)
            } else {
              # It's a key-path pair
              reticulate::tuple(item[[1L]], item[[2L]])
            }
          }))
          # Call Python put_files method
          py_result <- private$py_s3$put_files(py_key_paths, overwrite)
          # Convert Python result to R
          purrr::map(py_result, as.list)
        },
        error = function(e) {
          cli::cli_abort(c(
            "Error in put_files operation",
            i = "{e$message}"
          ))
        }
      )
      result
    }
  ),
  private = list(
    mf = NULL,
    s3_client = NULL,
    py_s3 = NULL,
    tmproot = NULL,
    bucket = NULL,
    prefix = NULL,
    run = NULL,
    s3root = NULL,
    is_connected = FALSE,
    verbose = FALSE,
    convert_s3objects = function(py_objects) {
      if (is.null(py_objects) || length(py_objects) == 0L) {
        return(list())
      }
      if (!is.list(py_objects)) {
        cli::cli_abort("py_objects must be a list")
      }
      required_attrs <- c("url", "prefix")
      result <- tryCatch(
        {
          purrr::map(py_objects, function(py_obj) {
            missing_attrs <- required_attrs[!purrr::map_lgl(required_attrs, ~ reticulate::py_has_attr(py_obj, .))]
            if (length(missing_attrs) > 0L) {
              cli::cli_warn("Python object is missing required attributes: {paste(missing_attrs, collapse = ', ')}")
              return(NULL)
            }
            s3obj <- S3Object$new(py_obj)
            if (is.null(s3obj$key)) {
              cli::cli_warn("Created S3Object is missing 'key' attribute")
              return(NULL)
            }
            s3obj
          })
        },
        error = function(e) {
          cli::cli_abort("Critical error converting Python objects to R S3Objects: {e$message}")
        }
      )
      purrr::compact(result)
    }
  )
)

#' Create a new S3 object
#'
#' @param tmproot Character string specifying the temporary directory root.
#' @param bucket Optional character string specifying the S3 bucket.
#' @param prefix Optional character string specifying the S3 prefix.
#' @param run Optional Metaflow Run object or FlowSpec.
#' @param s3root Optional character string specifying the S3 root path.
#' @return An S3 object
#' @export
create_s3_client <- function(tmproot = ".", bucket = NULL, prefix = NULL, run = NULL, s3root = NULL) {
  S3$new(tmproot, bucket, prefix, run, s3root)
}

#' Create a new S3 object with a specified AWS role
#'
#' @param role Character string specifying the AWS role ARN.
#' @param ... Additional arguments passed to create_s3_client
#' @return An S3 object with the specified role
#' @export
create_s3_client_with_role <- function(role, ...) {
  client <- create_s3_client(...)
  client$set_role(role)
  client
}

#' Create a new S3 object with specified session variables
#'
#' @param session_vars List of session variables.
#' @param ... Additional arguments passed to create_s3_client
#' @return An S3 object with the specified session variables
#' @export
create_s3_client_with_session <- function(session_vars, ...) {
  client <- create_s3_client(...)
  client$set_session_vars(session_vars)
  client
}
