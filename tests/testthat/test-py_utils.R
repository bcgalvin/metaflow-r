# System checks
test_that("check_system handles Windows correctly", {
  testthat::local_mocked_bindings(
    Sys.info = function() list(sysname = "Windows"),
    .package = "base"
  )

  expect_snapshot(check_system(), error = TRUE)
})

test_that("check_system allows non-Windows systems", {
  testthat::local_mocked_bindings(
    Sys.info = function() list(sysname = "Linux"),
    .package = "base"
  )

  expect_snapshot(check_system())
})

# Python version checks
test_that("check_python_version handles unsupported Python versions correctly", {
  expect_snapshot(check_python_version("3.7.0"), error = TRUE)
  expect_snapshot(check_python_version("3.6.0"), error = TRUE)
  expect_snapshot(check_python_version("2.7.0"), error = TRUE)
})

test_that("check_python_version accepts supported Python versions", {
  expect_snapshot(check_python_version("3.8.0"))
  expect_snapshot(check_python_version("3.9.0"))
  expect_snapshot(check_python_version(NULL))
})

# Environment preparation
cli::test_that_cli("prepare_environment handles conda environments correctly", {
  testthat::local_mocked_bindings(
    condaenv_exists = function(...) FALSE,
    conda_create = function(env, packages, python_version) NULL,
    .package = "reticulate"
  )

  testthat::expect_snapshot(prepare_environment(TRUE, "test_env", "3.8", "conda"))
  
  testthat::local_mocked_bindings(
    condaenv_exists = function(...) TRUE,
    conda_remove = function(...) NULL,
    conda_create = function(env, packages, python_version) NULL,
    .package = "reticulate"
  )

  testthat::expect_snapshot(prepare_environment(TRUE, "test_env", "3.8", "conda"))
  testthat::expect_snapshot(prepare_environment(FALSE, "test_env", "3.8", "conda"))
})

cli::test_that_cli("prepare_environment handles virtualenv environments correctly", {
  testthat::local_mocked_bindings(
    virtualenv_exists = function(...) FALSE,
    virtualenv_create = function(envname, python) NULL,
    .package = "reticulate"
  )

  testthat::expect_snapshot(prepare_environment(TRUE, "test_env", "3.8", "virtualenv"))
  
  testthat::local_mocked_bindings(
    virtualenv_exists = function(...) TRUE,
    virtualenv_remove = function(...) NULL,
    virtualenv_create = function(envname, python) NULL,
    .package = "reticulate"
  )

  testthat::expect_snapshot(prepare_environment(TRUE, "test_env", "3.8", "virtualenv"))
  testthat::expect_snapshot(prepare_environment(FALSE, "test_env", "3.8", "virtualenv"))
})

cli::test_that_cli("prepare_environment removes existing environment correctly", {
  testthat::local_mocked_bindings(
    condaenv_exists = function(...) TRUE,
    conda_remove = function(...) NULL,
    conda_create = function(...) NULL,
    .package = "reticulate"
  )

  testthat::expect_snapshot(
    prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8", method = "conda")
  )

  testthat::local_mocked_bindings(
    virtualenv_exists = function(...) TRUE,
    virtualenv_remove = function(...) NULL,
    virtualenv_create = function(...) NULL,
    .package = "reticulate"
  )

  testthat::expect_snapshot(
    prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8", method = "virtualenv")
  )
})

cli::test_that_cli("prepare_environment uses existing environment correctly", {
  testthat::local_mocked_bindings(
    condaenv_exists = function(...) TRUE,
    .package = "reticulate"
  )

  testthat::expect_snapshot(
    prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8", method = "conda")
  )

  testthat::local_mocked_bindings(
    virtualenv_exists = function(...) TRUE,
    .package = "reticulate"
  )

  testthat::expect_snapshot(
    prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8", method = "virtualenv")
  )
})

# Metaflow availability
test_that("ensure_metaflow handles METAFLOW_PYTHON correctly", {
  withr::local_envvar(METAFLOW_PYTHON = "/path/to/python")

  testthat::local_mocked_bindings(
    .package = "reticulate",
    use_python = function(...) NULL,
    py_config = function() list(version = "3.8.0"),
    py_module_available = function(...) TRUE,
    import = function(...) list(`__version__` = "2.0.0")
  )

  expect_no_error(ensure_metaflow())
  expect_true(is_metaflow_available())
})

test_that("ensure_metaflow handles missing Metaflow correctly", {
  withr::local_envvar(METAFLOW_PYTHON = "/path/to/python")

  testthat::local_mocked_bindings(
    use_python = function(...) NULL,
    py_config = function() list(version = "3.8.0"),
    py_module_available = function(...) FALSE,
    .package = "reticulate"
  )

  expect_error(ensure_metaflow(), "Metaflow is not installed")
})

test_that("ensure_metaflow handles unsupported Python version correctly", {
  withr::local_envvar(METAFLOW_PYTHON = "/path/to/python")

  testthat::local_mocked_bindings(
    use_python = function(...) NULL,
    py_config = function() list(version = "3.7.0"),
    .package = "reticulate"
  )

  expect_snapshot(ensure_metaflow(), error = TRUE)
})

cli::test_that_cli("ensure_metaflow handles incompatible Metaflow version correctly", {
  withr::local_envvar(METAFLOW_PYTHON = "/path/to/python")

  testthat::local_mocked_bindings(
    use_python = function(...) NULL,
    py_config = function() list(version = "3.8.0"),
    py_module_available = function(...) TRUE,
    import = function(...) list(`__version__` = "1.9.0"),
    .package = "reticulate"
  )

  expect_snapshot(ensure_metaflow(), error = TRUE)
})

cli::test_that_cli("ensure_metaflow handles invalid METAFLOW_PYTHON path correctly", {
  withr::local_envvar(METAFLOW_PYTHON = "/invalid/path/to/python")

  testthat::local_mocked_bindings(
    use_python = function(...) stop("Python executable not found"),
    .package = "reticulate"
  )

  expect_snapshot(ensure_metaflow(), error = TRUE)
})

cli::test_that_cli("ensure_metaflow handles missing Metaflow module correctly", {
  withr::local_envvar(METAFLOW_PYTHON = "/path/to/python")

  testthat::local_mocked_bindings(
    use_python = function(...) NULL,
    py_config = function() list(version = "3.8.0"),
    py_module_available = function(...) FALSE,
    .package = "reticulate"
  )

  expect_snapshot(ensure_metaflow(), error = TRUE)
})