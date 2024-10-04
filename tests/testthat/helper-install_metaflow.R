# Mock functions for testing
mock_sys_info <- function(sysname) {
  function() c(sysname = sysname)
}

# Setup mock objects
setup_mocks <- function() {
  list(
    sys_info = mockery::mock(),
    virtualenv_exists = mockery::mock(),
    condaenv_exists = mockery::mock(),
    py_install = mockery::mock()
  )
}
