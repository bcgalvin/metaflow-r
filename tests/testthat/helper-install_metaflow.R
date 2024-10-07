# Mock function for testing system information
mock_sys_info <- function(sysname) {
  function() c(sysname = sysname)
}

# Set up mock objects for testing install_metaflow functionality
setup_mocks <- function(envir = parent.frame()) {
  mocks <- list(
    sys_info = mockery::mock(),
    virtualenv_exists = mockery::mock(),
    condaenv_exists = mockery::mock(),
    py_install = mockery::mock(),
    cli_alert_info = mockery::mock(),
    cli_alert_warning = mockery::mock()
  )
  withr::defer(
    {
      # Clean up or restore mocks if necessary
    },
    envir = envir
  )
  mocks
}

# Helper function to manage .globals[["mf"]] within tests
local_mf <- function(value = NULL, envir = parent.frame()) {
  old_mf <- .globals[["mf"]]
  .globals[["mf"]] <- value
  withr::defer(
    {
      .globals[["mf"]] <- old_mf
    },
    envir = envir
  )
}

# Helper function to manage .globals[["mf_profile"]] within tests
local_mf_profile <- function(value = NULL, envir = parent.frame()) {
  old_mf_profile <- .globals[["mf_profile"]]
  .globals[["mf_profile"]] <- value
  withr::defer(
    {
      .globals[["mf_profile"]] <- old_mf_profile
    },
    envir = envir
  )
}
