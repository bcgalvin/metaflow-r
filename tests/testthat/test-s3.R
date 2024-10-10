create_mock_s3 <- function(tmproot, bucket, prefix, run, s3root, role, session_vars, client_params) {
  mock_s3 <- structure(
    list(
      tmproot = tmproot,
      bucket = bucket,
      prefix = prefix,
      run = run,
      s3root = s3root,
      role = role,
      session_vars = session_vars,
      client_params = client_params
    ),
    class = c("metaflow.plugins.datatools.s3.s3.S3", "python.builtin.object")
  )

  # Add a custom `$` method to mimic Python object attribute access
  attr(mock_s3, "$") <- function(x, name) {
    if (name %in% names(x)) {
      return(x[[name]])
    }
    stop(paste("AttributeError: 'S3' object has no attribute", name))
  }

  mock_s3
}

# Mock metaflow module
mock_metaflow <- function() {
  structure(
    list(
      S3 = function(...) {
        create_mock_s3(...)
      }
    ),
    class = c("python.builtin.module", "python.builtin.object")
  )
}

setup_mock_env <- function(code) {
  withr::with_environment(
    new.env(),
    {
      .globals <- new.env()
      .globals[["mf"]] <- mock_metaflow()
      code
    }
  )
}

test_that("S3Client initialization works", {
  setup_mock_env({
    client <- S3Client$new(tmproot = "/tmp", bucket = "test-bucket", prefix = "test-prefix")

    expect_s3_class(client, "R6")
    expect_true(inherits(client, "S3Client"))

    expect_null(client$.__enclos_env__$private$py_s3)
    expect_equal(client$.__enclos_env__$private$tmproot, "/tmp")
    expect_equal(client$.__enclos_env__$private$bucket, "test-bucket")
    expect_equal(client$.__enclos_env__$private$prefix, "test-prefix")
  })
})
