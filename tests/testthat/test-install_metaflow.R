# Load helper functions
testthat::local_test_context()


test_that("install_metaflow restarts session when restart_session is TRUE", {
  mockery::stub(install_metaflow, "reticulate::py_install", NULL)
  mockery::stub(install_metaflow, "check_system", NULL)
  mockery::stub(install_metaflow, "check_python_version", "3.8")
  mockery::stub(install_metaflow, "prepare_environment", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_info", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_success", NULL)

  restart_mock <- mockery::mock()
  mockery::stub(install_metaflow, "requireNamespace", TRUE)
  mockery::stub(install_metaflow, "rstudioapi::hasFun", TRUE)
  mockery::stub(install_metaflow, "rstudioapi::restartSession", restart_mock)

  withr::local_envvar(R_RESTART_SESSION = "TRUE")
  install_metaflow(method = "auto", restart_session = TRUE)
  expect_length(mockery::mock_calls(restart_mock), 1L)
})

test_that("install_metaflow does not restart session when restart_session is FALSE", {
  mockery::stub(install_metaflow, "reticulate::py_install", NULL)
  mockery::stub(install_metaflow, "check_system", NULL)
  mockery::stub(install_metaflow, "check_python_version", "3.8")
  mockery::stub(install_metaflow, "prepare_environment", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_info", NULL)
  mockery::stub(install_metaflow, "cli::cli_alert_success", NULL)

  restart_mock <- mockery::mock()
  mockery::stub(install_metaflow, "requireNamespace", TRUE)
  mockery::stub(install_metaflow, "rstudioapi::hasFun", TRUE)
  mockery::stub(install_metaflow, "rstudioapi::restartSession", restart_mock)

  withr::local_envvar(R_RESTART_SESSION = "FALSE")
  install_metaflow(method = "auto", restart_session = FALSE)
  expect_length(mockery::mock_calls(restart_mock), 0L)
})




test_that("install_metaflow handles different methods", {
  local_edition(3)
  local_reproducible_output(crayon = TRUE)
  # Mock functions to prevent actual installations and side effects
  mockery::stub(install_metaflow, "reticulate::py_install", NULL)
  mockery::stub(install_metaflow, "check_system", NULL)
  mockery::stub(install_metaflow, "check_python_version", "3.8")
  mockery::stub(install_metaflow, "prepare_environment", NULL)
  mockery::stub(install_metaflow, "restart_session", NULL)

  # Test different installation methods
  expect_snapshot(install_metaflow(method = "auto", version = "2.2.5"))
  expect_snapshot(install_metaflow(method = "virtualenv", version = "2.2.5"))
  expect_snapshot(install_metaflow(method = "conda", version = "2.2.5"))

  # Test error handling for invalid input
  expect_snapshot_error(install_metaflow(method = "invalid_method"))
  expect_snapshot_error(install_metaflow(version = "invalid_version"))
})

test_that("install_metaflow emits success message upon completion", {
  local_edition(3)
  local_reproducible_output(crayon = TRUE)

  # Mock necessary functions
  mockery::stub(install_metaflow, "reticulate::py_install", NULL)
  mockery::stub(install_metaflow, "prepare_environment", NULL)

  # Use expect_snapshot to capture the message
  expect_snapshot(install_metaflow())
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
