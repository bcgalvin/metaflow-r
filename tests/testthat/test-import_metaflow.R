test_that("ensure_metaflow uses METAFLOW_PYTHON when set and valid", {
  withr::local_envvar(
    METAFLOW_PYTHON = file.path("path", "to", "valid", "python")
  )
  local_mf()

  # Mock reticulate functions
  mock_py_config <- mockery::mock(list(version = "3.8"))
  mock_py_module_available <- mockery::mock(TRUE)
  mock_import <- mockery::mock(list(`__version__` = "2.2.5"))

  # Stub reticulate functions
  mockery::stub(ensure_metaflow, "reticulate::py_config", mock_py_config)
  mockery::stub(
    ensure_metaflow,
    "reticulate::py_module_available",
    mock_py_module_available
  )
  mockery::stub(ensure_metaflow, "reticulate::import", mock_import)

  # Call the function
  expect_silent(ensure_metaflow())

  # Check that mf is assigned correctly
  expect_false(is.null(.globals[["mf"]]))
  expect_identical(.globals[["mf"]][["__version__"]], "2.2.5")

  # Verify that reticulate::py_config was called with the correct argument
  mockery::expect_called(mock_py_config, 1L)
  expect_identical(
    mockery::mock_args(mock_py_config)[[1L]][["python"]],
    file.path("path", "to", "valid", "python")
  )
})

test_that(paste0(
  "ensure_metaflow throws error when METAFLOW_PYTHON has ",
  "old Python version"
), {
  withr::local_envvar(METAFLOW_PYTHON = file.path("path", "to", "python3.7"))
  local_mf()

  # Mock reticulate functions
  mock_use_python <- mockery::mock()
  mock_py_config <- mockery::mock(list(version = "3.7"))

  # Stub reticulate functions
  mockery::stub(ensure_metaflow, "reticulate::use_python", mock_use_python)
  mockery::stub(ensure_metaflow, "reticulate::py_config", mock_py_config)

  # Expect an error
  expect_error(
    ensure_metaflow(),
    "Metaflow requires Python >= 3.8."
  )
})

test_that("ensure_metaflow throws error when Metaflow is not installed", {
  withr::local_envvar(METAFLOW_PYTHON = file.path("invalid", "path", "python"))
  local_mf()

  # Mock reticulate functions
  mock_use_python <- mockery::mock()
  mock_py_config <- mockery::mock(list(version = "3.8"))
  mock_py_module_available <- mockery::mock(FALSE)

  # Stub reticulate functions
  mockery::stub(ensure_metaflow, "reticulate::use_python", mock_use_python)
  mockery::stub(ensure_metaflow, "reticulate::py_config", mock_py_config)
  mockery::stub(
    ensure_metaflow,
    "reticulate::py_module_available",
    mock_py_module_available
  )

  local_mf()
  # Expect an error
  expect_error(
    ensure_metaflow(),
    paste0(
      "Metaflow is not installed in the Python environment ",
      "specified by METAFLOW_PYTHON."
    )
  )
})

test_that("ensure_metaflow throws error when Metaflow version is too low", {
  withr::local_envvar(METAFLOW_PYTHON = file.path("path", "to", "python3.8"))
  local_mf()

  # Mock reticulate functions
  mock_use_python <- mockery::mock()
  mock_py_config <- mockery::mock(list(version = "3.8"))
  mock_py_module_available <- mockery::mock(TRUE)
  mock_import <- mockery::mock(list(`__version__` = "1.9.0"))

  # Stub reticulate functions
  mockery::stub(ensure_metaflow, "reticulate::use_python", mock_use_python)
  mockery::stub(ensure_metaflow, "reticulate::py_config", mock_py_config)
  mockery::stub(
    ensure_metaflow,
    "reticulate::py_module_available",
    mock_py_module_available
  )
  mockery::stub(ensure_metaflow, "reticulate::import", mock_import)

  # Temporarily reset .globals[["mf"]]
  # Expect an error
  expect_error(
    ensure_metaflow(),
    "Metaflow version .* is installed, but version >= 2.0 is required."
  )
})

test_that(paste0(
  "ensure_metaflow proceeds with default behavior when ",
  "METAFLOW_PYTHON is not set"
), {
  withr::local_envvar(METAFLOW_PYTHON = NA)
  local_mf()

  # Mock reticulate functions for default behavior
  mock_import <- mockery::mock(list(`__version__` = "2.2.5"))

  # Stub reticulate import with delay_load
  mockery::stub(ensure_metaflow, "reticulate::import", mock_import)

  # Temporarily reset .globals[["mf"]]
  # Call the function
  expect_silent(ensure_metaflow())

  # Check that mf is assigned
  expect_false(is.null(.globals[["mf"]]))
  expect_identical(.globals[["mf"]][["__version__"]], "2.2.5")
})

test_that(paste0(
  "ensure_metaflow throws error when METAFLOW_PYTHON path ",
  "is invalid"
), {
  withr::local_envvar(METAFLOW_PYTHON = file.path("invalid", "path", "python"))
  local_mf()

  # Mock reticulate functions to simulate invalid path
  mock_use_python <- mockery::mock(side_effect = function(...) {
    stop("Python executable not found")
  })

  # Stub reticulate functions
  mockery::stub(ensure_metaflow, "reticulate::use_python", mock_use_python)

  # Temporarily reset .globals[["mf"]]
  # Expect an error with specific message
  expect_error(
    ensure_metaflow(),
    "Python executable not found at path: .*"
  )
})
