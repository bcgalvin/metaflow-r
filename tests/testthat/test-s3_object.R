test_that("s3_object methods work correctly", {
  # Get a mock s3_object
  s3obj <- get_mock_S3Object()

  # Test all fields
  expect_equal(s3obj$url, "s3://bucket/key")
  expect_equal(s3obj$key, "key")
  expect_equal(s3obj$size, 1000)
  expect_equal(s3obj$path, "/local/path/to/file")
  expect_equal(as.raw(s3obj$blob), charToRaw("mock blob content"))
  expect_equal(s3obj$text, "mock text content")
  expect_equal(s3obj$metadata, list(key1 = "value1", key2 = "value2"))
  expect_equal(s3obj$content_type, "text/plain")
  expect_false(s3obj$downloaded)
  expect_true(s3obj$exists)
  expect_equal(s3obj$last_modified, as.character(as.POSIXct("2023-05-01 12:00:00", tz = "UTC")))
  expect_equal(s3obj$prefix, "prefix")
  expect_true(s3obj$has_info)
  expect_equal(s3obj$encryption, "AES256")
  expect_equal(s3obj$range_info, list(total_size = 1000, request_offset = 0, request_length = 1000))

  # Test print method
  expect_output(print(s3obj), "S3 Object:")
  expect_output(print(s3obj), "URL: s3://bucket/key")
  expect_output(print(s3obj), "Key: key")
  expect_output(print(s3obj), "Prefix: prefix")
  expect_output(print(s3obj), "Size: 1000 bytes")
  expect_output(print(s3obj), "Exists: TRUE")
  expect_output(print(s3obj), "Downloaded: FALSE")
  expect_output(print(s3obj), "Content Type: text/plain")
  expect_output(print(s3obj), "Last Modified: 2023-05-01 12:00:00")
  expect_output(print(s3obj), "Has Info: TRUE")
  expect_output(print(s3obj), "Encryption: AES256")

  # Test summary method
  summary_obj <- summary(s3obj)
  expect_equal(summary_obj$url, "s3://bucket/key")
  expect_equal(summary_obj$key, "key")
  expect_equal(summary_obj$prefix, "prefix")
  expect_true(summary_obj$exists)
  expect_equal(summary_obj$size, 1000)
  expect_false(summary_obj$downloaded)
  expect_equal(summary_obj$content_type, "text/plain")
  expect_equal(summary_obj$last_modified, as.character(as.POSIXct("2023-05-01 12:00:00", tz = "UTC")))
  expect_true(summary_obj$has_info)
  expect_equal(summary_obj$encryption, "AES256")
  expect_equal(summary_obj$range_info, list(total_size = 1000, request_offset = 0, request_length = 1000))

  # Test str method
  expect_output(str(s3obj), "S3 Object <s3://bucket/key>")
  expect_output(str(s3obj), "Size: 1000 bytes")
  expect_output(str(s3obj), "Content Type: text/plain")

  # Test as.character method
  s3obj$py_obj$downloaded <- TRUE
  expect_equal(as.character(s3obj), "mock text content")
  s3obj$py_obj$downloaded <- FALSE
  expect_error(as.character(s3obj), "Object not downloaded. Use get\\(\\) first.")

  # Test edge cases for boolean conversion
  s3obj$py_obj$exists <- TRUE # R logical
  expect_true(s3obj$exists)
  s3obj$py_obj$exists <- FALSE # R logical
  expect_false(s3obj$exists)
  s3obj$py_obj$exists <- get_py_true() # Python boolean
  expect_true(s3obj$exists)
  s3obj$py_obj$exists <- get_py_false() # Python boolean
  expect_false(s3obj$exists)

  s3obj$py_obj$downloaded <- TRUE # R logical
  expect_true(s3obj$downloaded)
  s3obj$py_obj$downloaded <- FALSE # R logical
  expect_false(s3obj$downloaded)
  s3obj$py_obj$downloaded <- get_py_true() # Python boolean
  expect_true(s3obj$downloaded)
  s3obj$py_obj$downloaded <- get_py_false() # Python boolean
  expect_false(s3obj$downloaded)

  s3obj$py_obj$`_has_info` <- TRUE # R logical
  expect_true(s3obj$has_info)
  s3obj$py_obj$`_has_info` <- FALSE # R logical
  expect_false(s3obj$has_info)
  s3obj$py_obj$`_has_info` <- get_py_true() # Python boolean
  expect_true(s3obj$has_info)
  s3obj$py_obj$`_has_info` <- get_py_false() # Python boolean
  expect_false(s3obj$has_info)
})

test_that("has_info is read-only", {
  s3obj <- get_mock_S3Object()
  expect_error(s3obj$py_obj$has_info <- TRUE, "AttributeError")
})

test_that("last_modified returns correct UTC timestamp", {
  s3obj <- get_mock_S3Object()
  expected_time <- as.character(as.POSIXct("2023-05-01 12:00:00", tz = "UTC"))
  expect_equal(s3obj$last_modified, expected_time)
})

test_that("convert_bool method works correctly", {
  s3obj <- get_mock_S3Object()

  expect_true(convert_bool(TRUE))
  expect_false(convert_bool(FALSE))
  expect_true(convert_bool(get_py_true()))
  expect_false(convert_bool(get_py_false()))
  expect_false(convert_bool(NULL))
  expect_error(convert_bool("not a boolean"), "Unexpected input type for boolean conversion")
  expect_error(convert_bool(1), "Unexpected input type for boolean conversion")
})
