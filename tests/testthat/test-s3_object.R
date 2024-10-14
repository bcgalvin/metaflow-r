test_that("s3_object methods work correctly", {
  # Get a mock s3_object
  s3obj <- get_mock_S3Object()

  # Test new methods
  expect_equal(s3obj$prefix(), "prefix")
  expect_equal(s3obj$key(), "key")
  expect_true(s3obj$has_info())
  expect_equal(s3obj$encryption(), "AES256")
  expect_equal(s3obj$range_info(), list(total_size = 1000, request_offset = 0, request_length = 1000))

  # Test existing methods
  expect_equal(s3obj$url(), "s3://bucket/key")
  expect_true(s3obj$exists())
  expect_equal(s3obj$size(), 1000)
  expect_false(s3obj$downloaded())
  expect_equal(s3obj$content_type(), "text/plain")
  expect_equal(s3obj$last_modified(), as.POSIXct("2023-05-01 12:00:00", tz = "UTC"))

  # Test print method
  expect_output(print(s3obj), "S3 Object:")
  expect_output(print(s3obj), "URL: s3://bucket/key")
  expect_output(print(s3obj), "Key: key")
  expect_output(print(s3obj), "Prefix: prefix")

  # Test summary method
  summary_obj <- summary(s3obj)
  expect_equal(summary_obj$prefix, "prefix")
  expect_equal(summary_obj$key, "key")
  expect_true(summary_obj$has_info)
  expect_equal(summary_obj$encryption, "AES256")

  # Test as.data.frame method
  df <- as.data.frame(s3obj)
  expect_equal(df$prefix, "prefix")
  expect_equal(df$key, "key")
  expect_true(df$has_info)
  expect_equal(df$encryption, "AES256")
  expect_equal(df$range_total_size, 1000)
  expect_equal(df$range_request_offset, 0)
  expect_equal(df$range_request_length, 1000)

  # Test as.data.frame.s3_object handles missing range info
  test_that("as.data.frame.S3Object handles missing range info", {
    s3obj <- get_mock_S3Object()
    s3obj$py_obj$range_info <- NULL
    df <- as.data.frame(s3obj)
    expect_true(all(is.na(df[c("range_total_size", "range_request_offset", "range_request_length")])))
  })

  # Test edge cases for boolean conversion
  s3obj$py_obj$exists <- TRUE # R logical
  expect_true(s3obj$exists())
  s3obj$py_obj$exists <- FALSE # R logical
  expect_false(s3obj$exists())
  s3obj$py_obj$exists <- get_py_true() # Python boolean
  expect_true(s3obj$exists())
  s3obj$py_obj$exists <- get_py_false() # Python boolean
  expect_false(s3obj$exists())

  s3obj$py_obj$downloaded <- TRUE # R logical
  expect_true(s3obj$downloaded())
  s3obj$py_obj$downloaded <- FALSE # R logical
  expect_false(s3obj$downloaded())
  s3obj$py_obj$downloaded <- get_py_true() # Python boolean
  expect_true(s3obj$downloaded())
  s3obj$py_obj$downloaded <- get_py_false() # Python boolean
  expect_false(s3obj$downloaded())

  s3obj$py_obj$`_has_info` <- TRUE # R logical
  expect_true(s3obj$has_info())
  s3obj$py_obj$`_has_info` <- FALSE # R logical
  expect_false(s3obj$has_info())
  s3obj$py_obj$`_has_info` <- get_py_true() # Python boolean
  expect_true(s3obj$has_info())
  s3obj$py_obj$`_has_info` <- get_py_false() # Python boolean
  expect_false(s3obj$has_info())

  test_that("has_info is read-only", {
    s3obj <- get_mock_S3Object()
    expect_error(s3obj$py_obj$has_info <- TRUE, "AttributeError")
  })

  # Test last_modified returns correct UTC timestamp
  test_that("last_modified returns correct UTC timestamp", {
    s3obj <- get_mock_S3Object()
    expected_time <- as.POSIXct("2023-05-01 12:00:00", tz = "UTC")
    expect_equal(s3obj$last_modified(), expected_time)
    expect_equal(attr(s3obj$last_modified(), "tzone"), "UTC")
  })

  # Test convert_bool method
  test_that("convert_bool method works correctly", {
    s3obj <- get_mock_S3Object()

    expect_true(s3obj$convert_bool(TRUE))
    expect_false(s3obj$convert_bool(FALSE))
    expect_true(s3obj$convert_bool(get_py_true()))
    expect_false(s3obj$convert_bool(get_py_false()))
    expect_false(s3obj$convert_bool(NULL))
    expect_error(s3obj$convert_bool("not a boolean"), "Unexpected input type for boolean conversion")
    expect_error(s3obj$convert_bool(1), "Unexpected input type for boolean conversion")
  })
})
