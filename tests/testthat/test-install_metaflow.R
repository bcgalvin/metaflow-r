# Package preparation and installation tests
test_that("prepare_packages handles different version specifications correctly", {
  expect_equal(prepare_packages("default", NULL, "virtualenv"), "metaflow")
  expect_equal(prepare_packages("2.7.3", NULL, "virtualenv"), "metaflow==2.7.3")
  expect_equal(prepare_packages("2.7.3", NULL, "conda"), "metaflow=2.7.3")
  expect_equal(prepare_packages(">=2.7.3", NULL, "virtualenv"), "metaflow>=2.7.3")
  expect_equal(prepare_packages("2.7.3", c("numpy", "pandas"), "virtualenv"), c("metaflow==2.7.3", "numpy", "pandas"))
})

cli::test_that_cli("install_metaflow installs correct packages", {
  mock_py_install <- function(packages, ...) {
    testthat::expect_equal(packages, c("metaflow==2.7.3", "numpy"))
    NULL
  }

  testthat::local_mocked_bindings(
    check_system = function() NULL,
    check_python_version = function(...) "3.8",
    prepare_environment = function(...) NULL,
    .package = "metaflow"
  )
  testthat::local_mocked_bindings(
    py_install = mock_py_install,
    .package = "reticulate"
  )

  testthat::expect_snapshot(
    install_metaflow(
      method = "virtualenv",
      version = "2.7.3",
      extra_packages = "numpy",
      restart_session = FALSE
    )
  )
})

cli::test_that_cli("install_metaflow handles conda installations correctly", {
  mock_py_install <- function(packages, ...) {
    testthat::expect_equal(packages, c("metaflow=2.7.3", "numpy"))
    NULL
  }

  testthat::local_mocked_bindings(
    check_system = function() NULL,
    check_python_version = function(...) "3.8",
    prepare_environment = function(...) NULL,
    .package = "metaflow"
  )
  testthat::local_mocked_bindings(
    py_install = mock_py_install,
    .package = "reticulate"
  )

  testthat::expect_snapshot(
    install_metaflow(
      method = "conda",
      version = "2.7.3",
      extra_packages = "numpy",
      restart_session = FALSE
    )
  )
})

cli::test_that_cli("install_metaflow installs latest version when 'default' is specified", {
  mock_py_install <- function(packages, ...) {
    testthat::expect_equal(packages, "metaflow")
    NULL
  }

  testthat::local_mocked_bindings(
    check_system = function() NULL,
    check_python_version = function(...) "3.8",
    prepare_environment = function(...) NULL,
    .package = "metaflow"
  )

  testthat::local_mocked_bindings(
    py_install = mock_py_install,
    .package = "reticulate"
  )

  testthat::expect_snapshot(
    install_metaflow(
      method = "virtualenv",
      version = "default",
      restart_session = FALSE
    )
  )
})

test_that("install_metaflow validates input parameters", {
  expect_error(install_metaflow(method = "invalid"), "should be one of")
  expect_error(install_metaflow(version = 123), "Must be of type 'string'")
  expect_error(install_metaflow(extra_packages = 123), "Must be of type 'character'")
  expect_error(install_metaflow(restart_session = "yes"), "Must be of type 'logical'")
})

# Add new tests for error handling
cli::test_that_cli("install_metaflow handles unsupported Python version correctly", {
  testthat::local_mocked_bindings(
    check_system = function() NULL,
    check_python_version = function(...) stop("Unsupported Python version"),
    .package = "metaflow"
  )

  expect_snapshot(
    install_metaflow(method = "virtualenv", version = "2.7.3"),
    error = TRUE
  )
})

cli::test_that_cli("install_metaflow handles environment preparation errors", {
  testthat::local_mocked_bindings(
    check_system = function() NULL,
    check_python_version = function(...) "3.8",
    prepare_environment = function(...) stop("Failed to prepare environment"),
    .package = "metaflow"
  )

  expect_snapshot(
    install_metaflow(method = "virtualenv", version = "2.7.3"),
    error = TRUE
  )
})

cli::test_that_cli("install_metaflow handles package installation errors", {
  testthat::local_mocked_bindings(
    py_install = function(...) stop("Failed to install packages"),
    .package = "reticulate"
  )
  testthat::local_mocked_bindings(
    check_system = function() NULL,
    check_python_version = function(...) "3.8",
    prepare_environment = function(...) NULL,
    .package = "metaflow"
  )

  expect_snapshot(
    install_metaflow(method = "virtualenv", version = "2.7.3"),
    error = TRUE
  )
})
