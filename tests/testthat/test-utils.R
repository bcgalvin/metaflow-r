# Load helper functions
testthat::local_test_context()
withr::local_envvar(R_METAFLOW_TEST_MOCK = "true")
local_mf()

test_that("parse_metaflow_version handles different inputs correctly", {
  test_cases <- list(
    list(input = "2.2.5", method = "conda", expected = "metaflow=2.2.5"),
    list(input = "2.2.5", method = "virtualenv", expected = "metaflow==2.2.5"),
    list(input = "default", method = "conda", expected = "metaflow"),
    list(input = NULL, method = "virtualenv", expected = "metaflow"),
    list(input = NA, method = "conda", expected = "metaflow"),
    list(input = "", method = "virtualenv", expected = "metaflow"),
    list(input = ">=2.2.5", method = "conda", expected = "metaflow>=2.2.5"),
    list(input = "<2.2.5", method = "virtualenv", expected = "metaflow<2.2.5")
  )

  for (case in test_cases) {
    expect_identical(
      parse_metaflow_version(case[["input"]], method = case[["method"]]),
      case[["expected"]]
    )
  }
})

test_that("metaflow_version returns correct version", {
  # Mock the .globals environment to simulate Metaflow being available
  # with a specific version
  .globals[["mf"]] <- list()
  .globals[["mf"]][["__version__"]] <- "2.3.0"

  expect_identical(metaflow_version(), "2.3.0")
})

test_that("check_python_version handles versions correctly", {
  expect_identical(check_python_version(NULL), ">=3.8")
  expect_identical(check_python_version("3.8"), "3.8")
  expect_identical(check_python_version("3.9"), "3.9")
  expect_error(
    check_python_version("3.6"),
    "Python version must be 3.8 or higher for Metaflow."
  )
  expect_error(
    check_python_version("2.7"),
    "Python version must be 3.8 or higher for Metaflow."
  )
})

test_that("check_system rejects Windows correctly", {
  withr::local_envvar(R_METAFLOW_TEST_MOCK = "true")
  original_sys_info <- Sys.info
  mockery::stub(check_system, "Sys.info", function() list(sysname = "Windows"))
  withr::defer({
    mockery::stub(check_system, "Sys.info", original_sys_info)
  })
  expect_error(
    check_system(),
    "Metaflow installation is not supported on Windows."
  )
  mockery::stub(check_system, "Sys.info", function() list(sysname = "Darwin"))
  expect_silent(check_system())
  withr::defer({
    mockery::stub(check_system, "Sys.info", original_sys_info)
  })
  mockery::stub(check_system, "Sys.info", function() list(sysname = "Linux"))
  expect_silent(check_system())
})

test_that("prepare_environment works correctly", {
  mockery::stub(prepare_environment, "check_existing_environment", TRUE)
  mockery::stub(prepare_environment, "reticulate::conda_remove", NULL)
  mockery::stub(prepare_environment, "reticulate::conda_create", NULL)
  mockery::stub(prepare_environment, "reticulate::virtualenv_remove", NULL)
  mockery::stub(prepare_environment, "reticulate::virtualenv_create", NULL)
  mockery::stub(prepare_environment, "cli::cli_alert_info", NULL)

  expect_silent(prepare_environment(TRUE, "test-env", "conda", "3.8"))
  expect_silent(prepare_environment(TRUE, "test-env", "virtualenv", "3.8"))
})
