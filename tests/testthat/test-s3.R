## S3Object

test_that("S3Object initializes correctly with various configurations", {
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

  # initialization with range_info
  py_obj_with_range <- mock_py_s3object(range_info = list(total_size = 1000, request_offset = 0, request_length = 100))
  s3obj_with_range <- S3Object$new(py_obj_with_range)
  expect_equal(s3obj_with_range$range_info, list(total_size = 1000, request_offset = 0, request_length = 100))
  expect_true(s3obj_with_range$is_partially_downloaded())

  # print method
  expect_output(print(s3obj), "S3Object:")
  expect_output(print(s3obj), "exists: TRUE")
  expect_output(print(s3obj), "url: s3://bucket/key")
  expect_output(print(s3obj), "range_info: NULL")

  expect_output(print(s3obj_with_range), "range_info:")
  expect_output(print(s3obj_with_range), "total_size: 1000")
  expect_output(print(s3obj_with_range), "request_offset: 0")
  expect_output(print(s3obj_with_range), "request_length: 100")
})

test_that("S3 list_paths handles various scenarios correctly", {
  skip_if_not_installed("reticulate")

  mocked_mf <- mock_mf(
    list_paths_fn = mock_list_paths("scenario1")
  )

  s3 <- S3$new(mf = mocked_mf)
  s3$connect()

  # root-level listing
  result_root <- s3$list_paths()
  expect_length(result_root, 3)
  expect_equal(sort(sapply(result_root, function(x) x$key)), c("file5", "folder1", "folder2"))

  # prefix matching
  result_prefix <- s3$list_paths("folder1")
  expect_length(result_prefix, 3)
  expect_equal(sort(sapply(result_prefix, function(x) x$key)), c("folder1/file1", "folder1/file2", "folder1/subfolder"))

  # non-matching prefix
  result_non_matching <- s3$list_paths("folder3")
  expect_length(result_non_matching, 0)

  # multiple keys
  result_multiple <- s3$list_paths(c("folder1/file1", "folder2/file4"))
  expect_length(result_multiple, 2)
  expect_equal(sort(sapply(result_multiple, function(x) x$key)), c("folder1/file1", "folder2/file4"))
})

test_that("S3 list_recursive handles nested structures correctly", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("metaflow"))

  mocked_mf <- mock_mf(
    list_recursive_fn = mock_list_recursive("scenario2")
  )

  s3 <- S3$new(mf = mocked_mf)
  s3$connect()

  result <- s3$list_recursive()

  expect_length(result, 3)
  expect_true(any(grepl("path1/file1", sapply(result, function(x) x$key))))
  expect_true(any(grepl("path1/file2", sapply(result, function(x) x$key))))
  expect_true(any(grepl("path2/file3", sapply(result, function(x) x$key))))
})

test_that("S3 resets connection status on failed connect", {
  s3 <- S3$new(mf = mock_mf(fail = TRUE))

  expect_error(s3$connect(), "Simulated connection error")
  expect_false(s3$.__enclos_env__$private$is_connected)

  s3$.__enclos_env__$private$mf <- mock_mf()

  expect_silent(s3$connect())
  expect_true(s3$.__enclos_env__$private$is_connected)
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
  py_obj_without_range <- mock_py_s3object()
  s3obj_without_range <- S3Object$new(py_obj_without_range)

  output_without_range <- capture.output(print(s3obj_without_range))
  expect_true(any(grepl("range_info: NULL", output_without_range)))

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

  s3_client_mock <- reticulate::py_eval("type('S3ClientMock', (object,), {
    '__init__': lambda self, **kwargs: None,
    'client': lambda self: None,
    'error': lambda self: None,
    'set_role': lambda self, role: None,
    'set_session_vars': lambda self, session_vars: None,
    'set_client_params': lambda self, client_params: None
  })()")

  mock_get_s3_client <- function(...) {
    list(client = s3_client_mock, error = NULL)
  }

  testthat::local_mocked_bindings(
    get_s3_client = mock_get_s3_client,
    .package = "metaflow"
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

test_that("S3 connection behavior is correct", {
  mocked_mf <- mock_mf()

  s3 <- S3$new(tmproot = withr::local_tempdir(), mf = mocked_mf)
  expect_silent(s3$connect())
  expect_true(s3$.__enclos_env__$private$is_connected)

  # connection is only initialized when needed
  s3_uninit <- S3$new(mf = mocked_mf)
  expect_null(s3_uninit$.__enclos_env__$private$py_s3)
  s3_uninit$connect()
  expect_false(is.null(s3_uninit$.__enclos_env__$private$py_s3))

  # multiple connections
  expect_silent(s3$connect())
  expect_silent(s3$connect())
  expect_true(s3$.__enclos_env__$private$is_connected)
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

# Factory tests

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

test_that("S3Client set_role method works correctly", {
  s3_client <- S3Client$new()
  role_arn <- "arn:aws:iam::123456789012:role/test-role"

  expect_silent(s3_client$set_role(role_arn))
  expect_equal(s3_client$.__enclos_env__$private$s3_role, role_arn)
})

# List methods

test_that("S3 list_paths method works correctly", {
  skip_if_not_installed("reticulate")

  mocked_mf <- mock_mf(
    list_paths_fn = mock_list_paths("scenario2")
  )

  s3 <- S3$new(mf = mocked_mf)
  s3$connect()

  result <- s3$list_paths()

  expect_length(result, 2)
  expect_true(all(sapply(result, inherits, "S3Object")))
  expect_equal(sapply(result, function(x) x$key), c("path1", "path2"))
})

test_that("S3 list_recursive method works correctly", {
  skip_if_not_installed("reticulate")

  mocked_mf <- mock_mf(
    list_recursive_fn = mock_list_recursive("scenario2")
  )

  s3 <- S3$new(mf = mocked_mf)
  s3$connect()

  result <- s3$list_recursive()

  expect_length(result, 3)
  expect_equal(sapply(result, function(x) x$key), c("path1/file1", "path1/file2", "path2/file3"))
  expect_equal(sapply(result, function(x) x$size), c(100, 150, 200))
})

test_that("S3 list_paths handles single and multiple keys correctly", {
  skip_if_not_installed("reticulate")

  mocked_mf <- mock_mf(
    list_paths_fn = mock_list_paths("scenario1")
  )

  s3 <- S3$new(mf = mocked_mf)
  s3$connect()

  # single key
  result_single <- s3$list_paths("folder1")
  expect_length(result_single, 3)
  expect_equal(sort(sapply(result_single, function(x) x$key)), c("folder1/file1", "folder1/file2", "folder1/subfolder"))

  # multiple keys
  result_multiple <- s3$list_paths(c("folder1/file1", "folder2/file4"))
  expect_length(result_multiple, 2)
  expect_equal(sort(sapply(result_multiple, function(x) x$key)), c("folder1/file1", "folder2/file4"))

  # empty input
  result_empty <- s3$list_paths(character(0))
  expect_length(result_empty, 3)
  expect_equal(sort(sapply(result_empty, function(x) x$key)), c("file5", "folder1", "folder2"))
})

test_that("S3 list_recursive handles nested structures correctly", {
  skip_if_not_installed("reticulate")

  # Create the mocked mf object with scenario2 data
  mocked_mf <- mock_mf(
    list_recursive_fn = mock_list_recursive("scenario2")
  )

  # Inject the mocked mf into the S3 object
  s3 <- S3$new(mf = mocked_mf)
  s3$connect()

  result <- s3$list_recursive()

  expect_length(result, 3)
  expect_true(any(grepl("path1/file1", sapply(result, function(x) x$key))))
  expect_true(any(grepl("path1/file2", sapply(result, function(x) x$key))))
  expect_true(any(grepl("path2/file3", sapply(result, function(x) x$key))))
})

test_that("S3Client methods work correctly", {
  s3_client <- S3Client$new()

  # set_role
  role_arn <- "arn:aws:iam::123456789012:role/test-role"
  s3_client$set_role(role_arn)
  expect_equal(s3_client$.__enclos_env__$private$s3_role, role_arn)

  # set_session_vars
  session_vars <- list(var1 = "value1", var2 = "value2")
  s3_client$set_session_vars(session_vars)
  expect_equal(s3_client$.__enclos_env__$private$s3_session_vars, session_vars)

  # set_client_params
  client_params <- list(param1 = "value1", param2 = "value2")
  s3_client$set_client_params(client_params)
  expect_equal(s3_client$.__enclos_env__$private$s3_client_params, client_params)
})
