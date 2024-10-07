# Mock functions for testing
mock_dir_exists <- function(exists) {
  function(...) exists
}

mock_dir_ls <- function(files) {
  function(...) files
}

mock_file_exists <- function(exists) {
  function(...) exists
}

mock_read_json <- function(content) {
  function(...) content
}

# Setup mock objects
setup_profile_mocks <- function() {
  mockery::stub(get_profiles, "fs::dir_exists", mockery::mock())
  mockery::stub(get_profiles, "fs::dir_ls", mockery::mock())
  mockery::stub(read_profile, "file.exists", mockery::mock())
  mockery::stub(read_profile, "jsonlite::read_json", mockery::mock())
}

# Helper function to temporarily set and restore the .globals[["mf_profile"]]
# and .globals[["mf_profile_path"]]
# This ensures that any changes to these globals during a test
# are reverted after the test completes.
local_profile <- function(value = NULL, envir = parent.frame()) {
  old_profile <- .globals[["mf_profile"]]
  old_profile_path <- .globals[["mf_profile_path"]]
  .globals[["mf_profile"]] <- value
  .globals[["mf_profile_path"]] <- NULL
  withr::defer(
    {
      .globals[["mf_profile"]] <- old_profile
      .globals[["mf_profile_path"]] <- old_profile_path
    },
    envir = envir
  )
}

# This section intentionally left blank to remove the unnecessary mocking functions
