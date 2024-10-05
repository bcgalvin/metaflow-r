# Mock functions for testing
mock_sys_info <- function(sysname) {
  function() c(sysname = sysname)
}

# Setup mock objects
setup_mocks <- function(envir = parent.frame()) {
  mocks <- list(
    sys_info = mockery::mock(),
    virtualenv_exists = mockery::mock(),
    condaenv_exists = mockery::mock(),
    py_install = mockery::mock()
  )
  withr::defer({
    # Clean up or restore mocks if necessary
  }, envir = envir)
  mocks
}
local_mf <- function(value = NULL, envir = parent.frame()) {
  old_globals <- .globals
  .globals <- new.env(parent = emptyenv())
  .globals[["mf"]] <- value
  withr::defer({
    .globals <- old_globals
  }, envir = envir)
}
