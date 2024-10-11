## S3Object

test_that("S3Object initializes correctly", {
  py_obj <- mock_py_s3object()
  s3obj <- S3Object$new(py_obj)

  expect_true(s3obj$exists)
  expect_equal(s3obj$url, "s3://bucket/key")
  expect_equal(s3obj$prefix, "prefix")
  expect_equal(s3obj$key, "key")
  expect_equal(s3obj$size, 100)
  expect_true(s3obj$has_info)
  expect_equal(s3obj$metadata, list(key = "value"))
  expect_equal(s3obj$content_type, "text/plain")
  expect_type(s3obj$last_modified, "double")
  expect_equal(s3obj$encryption, "AES256")
  expect_null(s3obj$range_info)
})
test_that("S3Object handles range_info correctly", {
  py_obj <- mock_py_s3object(range_info = list(total_size = 1000, request_offset = 0, request_length = 100))
  s3obj <- S3Object$new(py_obj)

  expect_equal(s3obj$range_info, list(total_size = 1000, request_offset = 0, request_length = 100))
  expect_error(s3obj$range_info <- list(), "range_info is read-only")
  expect_true(s3obj$is_partially_downloaded())
})

test_that("S3Object print method works correctly", {
  py_obj <- mock_py_s3object()
  s3obj <- S3Object$new(py_obj)

  expect_output(print(s3obj), "S3Object:")
  expect_output(print(s3obj), "exists: TRUE")
  expect_output(print(s3obj), "url: s3://bucket/key")
  expect_output(print(s3obj), "range_info: NULL")
})

test_that("S3Object print method handles range_info correctly", {
  # Test without range_info
  py_obj_without_range <- mock_py_s3object()
  s3obj_without_range <- S3Object$new(py_obj_without_range)

  output_without_range <- capture.output(print(s3obj_without_range))
  expect_true(any(grepl("range_info: NULL", output_without_range)))

  # Test with range_info
  py_obj_with_range <- mock_py_s3object(
    range_info = reticulate::py_eval("type('RangeInfo', (), {'total_size': 1000, 'request_offset': 0, 'request_length': 100})()")
  )
  s3obj_with_range <- S3Object$new(py_obj_with_range)

  output_with_range <- capture.output(print(s3obj_with_range))
  expect_true(any(grepl("range_info:", output_with_range)))
  expect_true(any(grepl("total_size: 1000", output_with_range)))
  expect_true(any(grepl("request_offset: 0", output_with_range)))
  expect_true(any(grepl("request_length: 100", output_with_range)))
})

## S3Client

test_that("S3Client initializes correctly", {
  s3_client <- S3Client$new()
  expect_true(R6::is.R6(s3_client))
  expect_true(inherits(s3_client, "S3Client"))
})

test_that("S3Client methods work correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  # Create a mock S3Client object
  s3_client_mock <- reticulate::py_eval("type('S3ClientMock', (object,), {
    '__init__': lambda self, **kwargs: None,
    'client': lambda self: None,
    'error': lambda self: None,
    'set_role': lambda self, role: None,
    'set_session_vars': lambda self, session_vars: None,
    'set_client_params': lambda self, client_params: None
  })()")

  # Define a mock get_s3_client function
  mock_get_s3_client <- function(...) {
    list(client = s3_client_mock, error = NULL)
  }

  # Use local_mocked_bindings to replace get_s3_client in the package namespace
  testthat::local_mocked_bindings(
    get_s3_client = mock_get_s3_client,
    .package = "metaflow" # Replace with your actual package name
  )

  s3_client <- S3Client$new()

  expect_null(s3_client$error())
  expect_silent(s3_client$set_role("test_role"))
  expect_silent(s3_client$set_session_vars(list(var1 = "value1")))
  expect_silent(s3_client$set_client_params(list(param1 = "value1")))
})

## S3

test_that("S3 initializes correctly", {
  s3 <- S3$new(tmproot = "test_root", bucket = "test_bucket", prefix = "test_prefix")
  expect_equal(s3$.__enclos_env__$private$tmproot, "test_root")
  expect_equal(s3$.__enclos_env__$private$bucket, "test_bucket")
  expect_equal(s3$.__enclos_env__$private$prefix, "test_prefix")
  expect_false(s3$.__enclos_env__$private$is_connected)
  expect_true(R6::is.R6(s3$.__enclos_env__$private$s3_client))
  expect_true(inherits(s3$.__enclos_env__$private$s3_client, "S3Client"))
})

test_that("S3 prints correctly when not connected", {
  s3 <- S3$new()
  captured_output <- capture.output(print(s3))
  expected_output <- c(
    "S3:",
    "  tmproot: .",
    "  bucket: NULL",
    "  prefix: NULL",
    "  s3root: NULL",
    "  connected: FALSE"
  )
  expect_equal(trimws(captured_output), trimws(expected_output))
})

# Connection tests
test_that("S3 connects successfully", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  s3 <- S3$new(tmproot = tmp_dir, bucket = "my_bucket", prefix = "data/")

  expect_silent(s3$connect())
  expect_output(print(s3), "connected: TRUE")
})

test_that("S3 initializes py_s3 only when connect is called", {
  s3 <- S3$new()

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf(fail = TRUE)

  expect_snapshot(print(s3))
  expect_snapshot(s3$connect(), error = TRUE)
  expect_snapshot(print(s3))

  .globals[["mf"]] <- mock_mf(fail = FALSE)
  expect_snapshot(s3$connect())
  expect_snapshot(print(s3))
})

test_that("S3 connect method can be called multiple times without error", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new(tmproot = withr::local_tempdir())

  expect_silent(s3$connect())
  expect_output(print(s3), "connected: TRUE")

  expect_silent(s3$connect())
  expect_output(print(s3), "connected: TRUE")
})

test_that("S3 connect method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  s3 <- S3$new(tmproot = tmp_dir, bucket = "my_bucket", prefix = "data/")

  expect_snapshot(
    {
      s3$connect()
      print(s3)
    },
    transform = transform_temp_paths
  )
})

# Method tests
test_that("S3 public methods support method chaining", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new(tmproot = withr::local_tempdir())

  expect_silent({
    s3$connect()$set_role("arn:aws:role")$set_session_vars(list(var1 = "value1"))$set_client_params(list(param1 = "value1"))
  })

  expect_output(print(s3), "connected: TRUE")
})

test_that("S3 set_role method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  role_arn <- "arn:aws:iam::123456789012:role/test-role"

  expect_silent(s3$set_role(role_arn))
  expect_equal(s3$set_role(role_arn), s3)
  expect_equal(s3$.__enclos_env__$private$s3_client$.__enclos_env__$private$s3_role, role_arn)
})

test_that("S3 set_session_vars method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  session_vars <- list(var1 = "value1", var2 = "value2")

  expect_silent(s3$set_session_vars(session_vars))
  expect_equal(s3$set_session_vars(session_vars), s3)
  expect_equal(s3$.__enclos_env__$private$s3_client$.__enclos_env__$private$s3_session_vars, session_vars)
})

test_that("S3 set_client_params method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  client_params <- list(param1 = "value1", param2 = "value2")

  expect_silent(s3$set_client_params(client_params))
  expect_equal(s3$set_client_params(client_params), s3)
  expect_equal(s3$.__enclos_env__$private$s3_client$.__enclos_env__$private$s3_client_params, client_params)
})

# Factory function tests
test_that("create_s3_client function creates a new S3 object", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  s3 <- create_s3_client(tmproot = tmp_dir, bucket = "test_bucket", prefix = "test/")

  expect_true(R6::is.R6(s3))
  expect_true(inherits(s3, "S3"))

  expect_silent(s3$connect())
  expect_output(print(s3), "connected: TRUE")
})

test_that("create_s3_client_with_role function creates a new S3 object with a role", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  role_arn <- "arn:aws:iam::123456789012:role/test-role"
  s3 <- create_s3_client_with_role(role_arn, tmproot = tmp_dir)

  expect_true(R6::is.R6(s3))
  expect_true(inherits(s3, "S3"))

  expect_silent(s3$connect())
  expect_output(print(s3), "connected: TRUE")
  expect_equal(s3$.__enclos_env__$private$s3_client$.__enclos_env__$private$s3_role, role_arn)
})

test_that("create_s3_client_with_session function creates a new S3 object with session variables", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  tmp_dir <- withr::local_tempdir()
  session_vars <- list(var1 = "value1", var2 = "value2")
  s3 <- create_s3_client_with_session(session_vars, tmproot = tmp_dir)

  expect_true(R6::is.R6(s3))
  expect_true(inherits(s3, "S3"))

  expect_silent(s3$connect())
  expect_output(print(s3), "connected: TRUE")
  expect_equal(s3$.__enclos_env__$private$s3_client$.__enclos_env__$private$s3_session_vars, session_vars)
})
# Add direct tests for S3Client methods
test_that("S3Client set_role method works correctly", {
  s3_client <- S3Client$new()
  role_arn <- "arn:aws:iam::123456789012:role/test-role"

  expect_silent(s3_client$set_role(role_arn))
  expect_equal(s3_client$.__enclos_env__$private$s3_role, role_arn)
})

# Add tests for list_paths and list_recursive
test_that("S3 list_paths method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  s3$connect()

  result <- s3$list_paths()
  expect_length(result, 2)
  expect_true(all(sapply(result, inherits, "S3Object")))
  expect_equal(sapply(result, function(x) x$key), c("path1", "path2"))
  expect_equal(sapply(result, function(x) x$size), c(100, 200))
})

test_that("S3 list_recursive method works correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  s3$connect()

  result <- s3$list_recursive()

  expect_length(result, 3)
  expect_equal(sapply(result, function(x) x$key), c("path1/file1", "path1/file2", "path2/file3"))
})

test_that("S3 uses S3Client for connection", {
  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  s3$connect()

  expect_true(s3$.__enclos_env__$private$is_connected)
  expect_true(!is.null(s3$.__enclos_env__$private$py_s3))
})

test_that("S3 list_paths handles single and multiple keys correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  s3$connect()

  # Test with a single key
  result_single <- s3$list_paths("path1")
  expect_length(result_single, 1)
  expect_equal(result_single[[1]]$key, "path1")

  # Test with multiple keys
  result_multiple <- s3$list_paths(c("path1", "path2"))
  expect_length(result_multiple, 2)
  expect_equal(sapply(result_multiple, function(x) x$key), c("path1", "path2"))

  # Test with empty list of keys
  result_empty <- s3$list_paths(character(0))
  expect_length(result_empty, 0)
})
test_that("S3 list_recursive handles nested structures correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)
  .globals[["mf"]] <- mock_mf()

  s3 <- S3$new()
  s3$connect()

  result <- s3$list_recursive()

  expect_length(result, 3)
  expect_true(any(grepl("path1/file1", sapply(result, function(x) x$key))))
  expect_true(any(grepl("path1/file2", sapply(result, function(x) x$key))))
  expect_true(any(grepl("path2/file3", sapply(result, function(x) x$key))))
})

test_that("S3 list_paths and list_recursive handle errors correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  original_mf <- .globals[["mf"]]
  withr::defer(.globals[["mf"]] <- original_mf)

  # Mock an error scenario
  .globals[["mf"]] <- list(
    S3 = function(...) {
      list(
        list_paths = function(...) stop("Access denied"),
        list_recursive = function(...) stop("Access denied")
      )
    }
  )

  s3 <- S3$new()
  s3$connect()

  expect_error(s3$list_paths(), "Access denied")
  expect_error(s3$list_recursive(), "Access denied")
})
