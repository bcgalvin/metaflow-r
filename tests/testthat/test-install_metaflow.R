# Load helper functions
testthat::local_test_context()

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

test_that("install_metaflow handles restart_session parameter", {
  mockery::stub(install_metaflow, "reticulate::py_install", NULL)
  mockery::stub(install_metaflow, "check_system", NULL)
  mockery::stub(install_metaflow, "check_python_version", "3.8")
  mockery::stub(install_metaflow, "prepare_environment", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_info", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_success", NULL)

  # Create a mock for rstudioapi::restartSession()
  restart_mock <- mockery::mock()

  # Stub requireNamespace to return TRUE
  mockery::stub(install_metaflow, "requireNamespace", TRUE)

  # Stub rstudioapi::hasFun to return TRUE
  mockery::stub(install_metaflow, "rstudioapi::hasFun", TRUE)

  # Stub rstudioapi::restartSession with our mock
  mockery::stub(install_metaflow, "rstudioapi::restartSession", restart_mock)

  # First call with restart_session = TRUE
  install_metaflow(restart_session = TRUE)
  expect_length(mockery::mock_calls(restart_mock), 1L)

  # Second call with restart_session = FALSE
  install_metaflow(restart_session = FALSE)
  # The total number of calls should still be 1
  expect_length(mockery::mock_calls(restart_mock), 1L)
})

test_that("check_python_version handles versions correctly", {
  expect_identical(check_python_version(NULL), ">=3.7")
  expect_identical(check_python_version("3.7"), "3.7")
  expect_identical(check_python_version("3.8"), "3.8")
  expect_identical(check_python_version("3.9"), "3.9")
  expect_error(
    check_python_version("3.6"),
    "Python version must be 3.7 or higher for Metaflow."
  )
  expect_error(
    check_python_version("2.7"),
    "Python version must be 3.7 or higher for Metaflow."
  )
})

test_that("check_system rejects Windows correctly", {
  withr::local_envvar(R_METAFLOW_TEST_MOCK = "true")
  mockery::stub(check_system, "Sys.info", function() list(sysname = "Windows"))
  expect_error(
    check_system(),
    "Metaflow installation is not supported on Windows."
  )
  mockery::stub(check_system, "Sys.info", function() list(sysname = "Darwin"))
  expect_silent(check_system())
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

test_that("install_metaflow handles different methods", {
  # Mock functions to prevent actual installations and side effects
  mockery::stub(install_metaflow, "reticulate::py_install", NULL)
  mockery::stub(install_metaflow, "check_system", NULL)
  mockery::stub(install_metaflow, "check_python_version", "3.8")
  mockery::stub(install_metaflow, "prepare_environment", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_info", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_success", NULL)
  mockery::stub(install_metaflow, "restart_session", NULL)

  # Test different installation methods

  expect_silent(suppressMessages(
    install_metaflow(method = "auto", version = "2.2.5")
  ))
  expect_silent(suppressMessages(
    install_metaflow(method = "virtualenv", version = "2.2.5")
  ))
  expect_silent(suppressMessages(
    install_metaflow(method = "conda", version = "2.2.5")
  ))

  # Test error handling for invalid input
  expect_error(
    install_metaflow(method = "invalid_method"),
    "should be one of"
  )

  expect_error(
    install_metaflow(version = "invalid_version"),
    "Invalid version specified"
  )
})

test_that("install_metaflow handles system check failures", {
  mockery::stub(
    install_metaflow,
    "check_system",
    function() stop("System check failed")
  )
  expect_error(install_metaflow(), "System check failed")
})

test_that("install_metaflow handles Python version check failures", {
  mockery::stub(install_metaflow, "check_system", function() NULL)
  mockery::stub(
    install_metaflow,
    "check_python_version",
    function(x) stop("Python version check failed")
  )
  expect_error(install_metaflow(), "Python version check failed")
})
