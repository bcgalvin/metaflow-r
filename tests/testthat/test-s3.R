# Helper functions
mock_mf <- function(fail = FALSE) {
  if (fail) {
    list(
      S3 = function(...) {
        cli::cli_abort(c(
          "Error in connect()",
          "i" = "Simulated connection error"
        ))
      }
    )
  } else {
    s3_mock <- reticulate::py_eval("type('S3Mock', (object,), {
      'tmproot': 'mocked_tmproot',
      'bucket': 'mocked_bucket',
      'prefix': 'mocked_prefix',
      '__init__': lambda self, **kwargs: None,
      'close': lambda self: None
    })()")

    list(
      S3 = function(tmproot, bucket, prefix, run, s3root, role, session_vars, client_params) {
        s3_mock
      }
    )
  }
}

transform_temp_paths <- function(x) {
  x <- gsub("\\\\", "/", x)
  x <- gsub("/var/folders/[^/]+/[^/]+/[^/]+/[^/]+/", "<temp_dir>/", x)
  x <- gsub("/tmp/Rtmp[A-Za-z0-9]+/", "<temp_dir>/", x)
  x <- gsub("file[a-f0-9]+", "<temp_file>", x)
  x <- gsub("/private/var/", "/var/", x)
  x <- gsub("Rtmp[A-Za-z0-9]+", "<temp_dir>", x)
  x <- gsub("/Users/[^/]+/", "<home_dir>/", x)
  x <- gsub("tmproot: .+$", "tmproot: <temp_dir>", x)
  x <- gsub("tmproot: mocked_tmproot", "tmproot: <mocked_tmproot>", x)
  x <- gsub("bucket: mocked_bucket", "bucket: <mocked_bucket>", x)
  x <- gsub("prefix: mocked_prefix", "prefix: <mocked_prefix>", x)
  x
}

# Initialization and basic functionality tests
test_that("S3Client initializes correctly", {
  client <- S3Client$new(tmproot = "test_root", bucket = "test_bucket", prefix = "test_prefix")
  expect_equal(client$.__enclos_env__$private$tmproot, "test_root")
  expect_equal(client$.__enclos_env__$private$bucket, "test_bucket")
  expect_equal(client$.__enclos_env__$private$prefix, "test_prefix")
  expect_false(client$.__enclos_env__$private$is_connected)
})

test_that("S3Client prints correctly when not connected", {
  client <- S3Client$new()
  captured_output <- capture.output(print(client))
  expected_output <- c(
    "S3Client:",
    "  tmproot: .",
    "  bucket: NULL",
    "  prefix: NULL",
    "  s3root: NULL",
    "  role: NULL",
    "  connected: FALSE"
  )
  expect_equal(trimws(captured_output), trimws(expected_output))
})

# Connection tests
test_that("S3Client connects successfully", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  client <- S3Client$new(tmproot = tmp_dir, bucket = "my_bucket", prefix = "data/")

  expect_silent(client$connect())
  expect_output(print(client), "connected: TRUE")
})

test_that("S3Client initializes py_s3 only when connect is called", {
  client <- S3Client$new()

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf(fail = TRUE)

  expect_snapshot(print(client))
  expect_snapshot(client$connect(), error = TRUE)
  expect_snapshot(print(client))

  .globals[["mf"]] <- mock_mf(fail = FALSE)
  expect_snapshot(client$connect())
  expect_snapshot(print(client))
})

test_that("S3Client connect method can be called multiple times without error", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  client <- S3Client$new(tmproot = withr::local_tempdir())

  expect_silent(client$connect())
  expect_output(print(client), "connected: TRUE")

  expect_silent(client$connect())
  expect_output(print(client), "connected: TRUE")
})

test_that("S3Client connect method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  client <- S3Client$new(tmproot = tmp_dir, bucket = "my_bucket", prefix = "data/")

  expect_snapshot(
    {
      client$connect()
      print(client)
    },
    transform = transform_temp_paths
  )
})

# Method tests
test_that("S3Client public methods support method chaining", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  client <- S3Client$new(tmproot = withr::local_tempdir())

  expect_silent({
    client$connect()$set_role("arn:aws:role")$set_session_vars(list(var1 = "value1"))$set_client_params(list(param1 = "value1"))
  })

  expect_output(print(client), "connected: TRUE")
})

test_that("S3Client set_role method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  client <- S3Client$new()
  role_arn <- "arn:aws:iam::123456789012:role/test-role"

  expect_silent(client$set_role(role_arn))
  expect_output(print(client), paste0("role: ", role_arn))
  expect_equal(client$set_role(role_arn), client)
})

test_that("S3Client set_session_vars method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  client <- S3Client$new()
  session_vars <- list(var1 = "value1", var2 = "value2")

  expect_silent(client$set_session_vars(session_vars))
  expect_equal(client$set_session_vars(session_vars), client)
})

test_that("S3Client set_client_params method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  client <- S3Client$new()
  client_params <- list(param1 = "value1", param2 = "value2")

  expect_silent(client$set_client_params(client_params))
  expect_equal(client$set_client_params(client_params), client)
})

# Factory function tests
test_that("create_s3_client function creates a new S3Client object", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  client <- create_s3_client(tmproot = tmp_dir, bucket = "test_bucket", prefix = "test/")

  expect_true(R6::is.R6(client))
  expect_true(inherits(client, "S3Client"))

  expect_silent(client$connect())
  expect_output(print(client), "connected: TRUE")
})

test_that("create_s3_client_with_role function creates a new S3Client object with a role", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  role_arn <- "arn:aws:iam::123456789012:role/test-role"
  client <- create_s3_client_with_role(role_arn, tmproot = tmp_dir)

  expect_true(R6::is.R6(client))
  expect_true(inherits(client, "S3Client"))

  expect_silent(client$connect())
  expect_output(print(client), "connected: TRUE")
  expect_output(print(client), paste0("role: ", role_arn))
})

test_that("create_s3_client_with_session function creates a new S3Client object with session variables", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  session_vars <- list(var1 = "value1", var2 = "value2")
  client <- create_s3_client_with_session(session_vars, tmproot = tmp_dir)

  expect_true(R6::is.R6(client))
  expect_true(inherits(client, "S3Client"))

  expect_silent(client$connect())
  expect_output(print(client), "connected: TRUE")
})
