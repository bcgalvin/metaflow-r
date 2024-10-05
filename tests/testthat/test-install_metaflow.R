# Load helper functions
testthat::local_test_context()


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
  withr::local_envvar(R_RESTART_SESSION = "TRUE")
  install_metaflow(restart_session = TRUE)
  expect_length(mockery::mock_calls(restart_mock), 1L)

  # Second call with restart_session = FALSE
  withr::local_envvar(R_RESTART_SESSION = "FALSE")
  install_metaflow(restart_session = FALSE)
  # The total number of calls should still be 1
  expect_length(mockery::mock_calls(restart_mock), 1L)
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
