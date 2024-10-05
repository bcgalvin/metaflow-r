.globals <- get(".globals", envir = asNamespace("metaflow"))

test_that("MetaflowConfig initializes correctly with default parameters", {
  withr::local_envvar(c(R_CONFIG_ACTIVE = NA, METAFLOW_KEY1 = NA, METAFLOW_KEY2 = NA))
  # Set up the global configuration and ensure it's reset after the test
  .globals[["mf_config"]] <- list(
    default = list(
      key1 = "value1",
      key2 = "value2"
    )
  )

  # Initialize the MetaflowConfig object
  config <- MetaflowConfig$new()
  expect_identical(config$active_env, "default")
  expect_identical(config$get("key1"), "value1")
  expect_identical(config$get("key2"), "value2")
  withr::defer({
    .globals[["mf_config"]] <- NULL
  })
})

test_that("MetaflowConfig initializes correctly with specified active env", {
  withr::local_envvar(c(R_CONFIG_ACTIVE = NA, METAFLOW_KEY1 = NA))
  .globals[["mf_config"]] <- list(
    default = list(
      key1 = "default_value1",
      key2 = "default_value2"
    ),
    development = list(
      key1 = "dev_value1"
      # key2 is not specified here
    )
  )
  withr::defer({
    .globals[["mf_config"]] <- NULL
  })

  config <- MetaflowConfig$new(active_env = "development")
  expect_identical(config$active_env, "development")
  expect_identical(config$get("key1"), "dev_value1")
  expect_identical(config$get("key2"), "default_value2")
})


test_that("MetaflowConfig uses R_CONFIG_ACTIVE env var if active_env is NULL", {
  withr::local_envvar(c(R_CONFIG_ACTIVE = "test_env", METAFLOW_KEY1 = NA))

  .globals[["mf_config"]] <- list(
    default = list(key1 = "default_value1"),
    test_env = list(key1 = "test_value1")
  )
  withr::defer({
    .globals[["mf_config"]] <- NULL
  })

  config <- MetaflowConfig$new()

  expect_identical(config$active_env, "test_env")
  expect_identical(config$get("key1"), "test_value1")
})


test_that("MetaflowConfig save() sets environment variables", {
  withr::local_envvar(c(METAFLOW_KEY1 = NA, METAFLOW_KEY2 = NA))
  .globals[["mf_config"]] <- list(
    default = list(key1 = "value1")
  )
  withr::defer({
    .globals[["mf_config"]] <- NULL
  })

  config <- MetaflowConfig$new()
  config$set("key2", "value2")
  config$save()

  expect_identical(Sys.getenv("METAFLOW_KEY1"), "value1")
  expect_identical(Sys.getenv("METAFLOW_KEY2"), "value2")
})
