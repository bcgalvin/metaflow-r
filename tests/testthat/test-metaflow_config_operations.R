test_that("get method retrieves the correct value for a given key", {
  config <- MetaflowConfig$new()

  config$set("key1", "value1")
  config$set("key2", "value2")
  config$set("METAFLOW_DEFAULT_DATASTORE", "s3")
  config$set("install.remotes", TRUE)
  config$set("install.verbose", FALSE)

  expect_identical(config$get("key1"), "value1")
  expect_identical(
    config$get("non_existent", default = "default_value"),
    "default_value"
  )
  expect_true(all(c("key1", "key2") %in% names(config$get())))
  expect_identical(config$get("METAFLOW_DEFAULT_DATASTORE"), "s3")
  expect_true(as.logical(config$get("install.remotes")))
  expect_false(as.logical(config$get("install.verbose")))
})


test_that("get method applies environment variable overrides", {
  Sys.setenv(METAFLOW_KEY1 = "env_value1")
  on.exit(Sys.unsetenv("METAFLOW_KEY1"), add = TRUE)

  config <- MetaflowConfig$new()
  config$set("key1", "config_value1")
  config$set("key2", "config_value2")

  expect_identical(config$get("key1"), "env_value1")
  expect_identical(config$get("key2"), "config_value2")
})

test_that("set method throws an error for invalid keys", {
  config <- MetaflowConfig$new()

  # Test with NULL key
  expect_error(
    config$set(NULL, "value"),
    regexp = "Invalid key: Key must be a non-empty character string of length 1.",
    class = "rlang_error"
  )

  # Test with empty string key
  expect_error(
    config$set("", "value"),
    regexp = "Invalid key: Key must be a non-empty character string of length 1.",
    class = "rlang_error"
  )

  # Test with NA_character_ key
  expect_error(
    config$set(NA_character_, "value"),
    regexp = "Invalid key: Key must be a non-empty character string of length 1.",
    class = "rlang_error"
  )
})

test_that("validate_key method throws an error for invalid keys", {
  config <- MetaflowConfig$new()

  # Test with NULL key
  expect_error(
    config$validate_key(NULL),
    regexp = "Invalid key: Key must be a non-empty character string of length 1.",
    class = "rlang_error"
  )

  # Test with empty string key
  expect_error(
    config$validate_key(""),
    regexp = "Invalid key: Key must be a non-empty character string of length 1.",
    class = "rlang_error"
  )

  # Test with NA_character_ key
  expect_error(
    config$validate_key(NA_character_),
    regexp = "Invalid key: Key must be a non-empty character string of length 1.",
    class = "rlang_error"
  )
})

test_that("get method handles empty environment variable overrides", {
  Sys.setenv(METAFLOW_KEY1 = "")
  on.exit(Sys.unsetenv("METAFLOW_KEY1"), add = TRUE)

  config <- MetaflowConfig$new()
  config$set("key1", "config_value1")
  config$set("key2", "config_value2")

  expect_identical(config$get("key1"), "")
  expect_identical(config$get("key2"), "config_value2")
})


test_that("switch_env method changes the active environment", {
  config <- MetaflowConfig$new()
  config$set("key1", "default_value1")
  config$switch_env("production")
  config$set("key1", "prod_value1")

  expect_identical(config$active_env, "production")
  expect_identical(config$get("key1"), "prod_value1")

  config$switch_env("non_existent")
  expect_identical(config$active_env, "non_existent")
})

test_that("validate_key checks if key exists in active environment", {
  config <- MetaflowConfig$new()
  config$set("key1", "value1")

  expect_true(config$validate_key("key1"))
  expect_false(config$validate_key("non_existent_key"))
})


test_that("print method displays configurations of the active environment", {
  config <- MetaflowConfig$new()
  config$set("key1", "value1")
  config$set("key2", "value2")

  expect_snapshot(
    config$print(),
    variant = "cli"
  )
})
test_that("validate_value correctly validates different types", {
  .globals[["mf_config"]] <- list(
    default = list(
      key1 = "value1"
    )
  )

  config <- MetaflowConfig$new()

  expect_true(config$validate_value("METAFLOW_DEFAULT_DATASTORE", "s3"))
  expect_true(config$validate_value("install.remotes", TRUE))
  expect_true(config$validate_value("install.verbose", FALSE))
  expect_false(config$validate_value("METAFLOW_DEFAULT_DATASTORE", 123L))
  expect_false(config$validate_value("install.remotes", "TRUE"))

  withr::defer({
    .globals[["mf_config"]] <- NULL
  })
})
