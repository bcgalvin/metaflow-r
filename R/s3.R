#' S3Client
#'
#' An R6 interface to Metaflow's S3 client.
#'
#' @importFrom R6 R6Class
#' @export
S3Client <- R6::R6Class("S3Client",
  public = list(
    #' @description
    #' Create a new S3Client object.
    #' @param tmproot Character string specifying the temporary directory root.
    #' @param bucket Optional character string specifying the S3 bucket.
    #' @param prefix Optional character string specifying the S3 prefix.
    #' @param run Optional Metaflow Run object or FlowSpec.
    #' @param s3root Optional character string specifying the S3 root path.
    initialize = function(tmproot = ".", bucket = NULL, prefix = NULL, run = NULL, s3root = NULL) {
      private$py_s3 <- NULL # Delayed initialization
      private$tmproot <- tmproot
      private$bucket <- bucket
      private$prefix <- prefix
      private$run <- run
      private$s3root <- s3root
      private$role <- NULL
      private$session_vars <- NULL
      private$client_params <- NULL
    },

    #' @description
    #' Connect to the S3 client.
    #' @return Invisible self (for method chaining).
    connect = function() {
      if (is.null(private$py_s3)) {
        mf <- .globals[["mf"]]
        private$py_s3 <- mf$S3(
          tmproot = private$tmproot,
          bucket = private$bucket,
          prefix = private$prefix,
          run = private$run,
          s3root = private$s3root,
          role = private$role,
          session_vars = private$session_vars,
          client_params = private$client_params
        )
        # Update the existing fields, using tryCatch to handle potential errors
        private$tmproot <- tryCatch(private$py_s3$tmproot, error = function(e) private$tmproot)
        private$bucket <- tryCatch(private$py_s3$bucket, error = function(e) private$bucket)
        private$prefix <- tryCatch(private$py_s3$prefix, error = function(e) private$prefix)
      }
      invisible(self)
    },

    #' @description
    #' Set the AWS role for the S3 client.
    #' @param role Character string specifying the AWS role ARN.
    #' @return Invisible self (for method chaining).
    set_role = function(role) {
      private$role <- role
      invisible(self)
    },

    #' @description
    #' Set the session variables for the S3 client.
    #' @param session_vars List of session variables.
    #' @return Invisible self (for method chaining).
    set_session_vars = function(session_vars) {
      private$session_vars <- session_vars
      invisible(self)
    },

    #' @description
    #' Set the client parameters for the S3 client.
    #' @param client_params List of client parameters.
    #' @return Invisible self (for method chaining).
    set_client_params = function(client_params) {
      private$client_params <- client_params
      invisible(self)
    },

    #' @description
    #' Print method for S3Client objects.
    #' @param ... Additional arguments passed to print (unused).
    print = function(...) {
      cat("S3Client:\n")
      cat("  tmproot:", private$tmproot, "\n")
      cat("  bucket:", private$bucket %||% "NULL", "\n")
      cat("  prefix:", private$prefix %||% "NULL", "\n")
      cat("  s3root:", private$s3root %||% "NULL", "\n")
      cat("  role:", private$role %||% "NULL", "\n")
      cat("  connected:", !is.null(private$py_s3), "\n")
      invisible(self)
    }
  ),
  private = list(
    py_s3 = NULL,
    tmproot = NULL,
    bucket = NULL,
    prefix = NULL,
    run = NULL,
    s3root = NULL,
    role = NULL,
    session_vars = NULL,
    client_params = NULL
  )
)

#' Create a new S3Client
#'
#' @export
create_s3_client <- function(tmproot = ".", bucket = NULL, prefix = NULL, run = NULL, s3root = NULL) {
  S3Client$new(tmproot, bucket, prefix, run, s3root)
}

#' Create a new S3Client with a specified AWS role
#'
#' @export
create_s3_client_with_role <- function(role, ...) {
  client <- create_s3_client(...)
  client$set_role(role)
  client
}

#' Create a new S3Client with specified session variables
#'
#' @export
create_s3_client_with_session <- function(session_vars, ...) {
  client <- create_s3_client(...)
  client$set_session_vars(session_vars)
  client
}
