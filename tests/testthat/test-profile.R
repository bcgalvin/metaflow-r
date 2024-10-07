test_that("get_active_profile returns the correct value when profile is set", {
  local_profile(list(name = "test_profile"))
  .globals[["mf_profile_path"]] <- "path/to/config.json"
  expect_equal(
    get_active_profile(),
    list(
      profile_name = list(name = "test_profile"),
      profile_path = "path/to/config.json"
    )
  )
})

test_that("get_active_profile returns a list with NULL values when no profile is set", {
  local_profile(NULL)
  expect_equal(
    get_active_profile(),
    list(
      profile_name = NULL,
      profile_path = NULL
    )
  )
})

test_that("get_profiles returns correct file paths or errors appropriately", {
  setup_profile_mocks()

  # Test successful case
  mockery::stub(get_profiles, "fs::path_home", function(...) "~")
  mockery::stub(get_profiles, "fs::dir_exists", TRUE)
  mockery::stub(get_profiles, "fs::dir_ls", c("~/metaflowconfig/config1.json", "~/metaflowconfig/config2.json"))

  result <- get_profiles()
  expect_equal(result, c("~/metaflowconfig/config1.json", "~/metaflowconfig/config2.json"))

  # Test when directory doesn't exist
  mockery::stub(get_profiles, "fs::dir_exists", FALSE)
  expect_error(get_profiles(), "No profiles found")

  # Test when directory exists but no config files
  mockery::stub(get_profiles, "fs::dir_exists", TRUE)
  mockery::stub(get_profiles, "fs::dir_ls", character(0))
  expect_error(get_profiles(), "No profiles found")
})

test_that("get_profiles handles directory with no matching files", {
  mockery::stub(get_profiles, "fs::dir_exists", TRUE)
  mockery::stub(get_profiles, "fs::dir_ls", character(0))
  expect_error(get_profiles(), "No profiles found")
})

test_that("get_profiles correctly filters config files", {
  mockery::stub(get_profiles, "fs::dir_exists", TRUE)
  mockery::stub(get_profiles, "fs::dir_ls", function(...) {
    c(
      "~/metaflowconfig/config1.json",
      "~/metaflowconfig/not_config.json",
      "~/metaflowconfig/config2.json",
      "~/metaflowconfig/config3.txt"
    )
  })
  expect_equal(get_profiles(), c("~/metaflowconfig/config1.json", "~/metaflowconfig/config2.json"))
})

test_that("read_profile handles successful read", {
  setup_profile_mocks()

  # Test successful read
  mockery::stub(read_profile, "file.exists", TRUE)
  mockery::stub(read_profile, "jsonlite::read_json", list(key = "value"))
  expect_equal(read_profile("config.json"), list(key = "value"))
})

test_that("read_profile handles non-existent file", {
  setup_profile_mocks()

  # Test non-existent file
  mockery::stub(read_profile, "file.exists", FALSE)
  expect_error(read_profile("non_existent.json"), "Config file not found")
})

test_that("read_profile handles file without .json extension", {
  setup_profile_mocks()

  # Test file without .json extension
  expect_error(read_profile("config.txt"), "Invalid file extension")
})

test_that("read_profile handles invalid JSON", {
  setup_profile_mocks()

  # Test invalid JSON
  mockery::stub(read_profile, "file.exists", TRUE)
  mockery::stub(read_profile, "jsonlite::read_json", function(...) stop("JSON parse error"))
  expect_error(read_profile("invalid.json"), "Error reading or parsing the JSON file")
})

test_that("read_profile handles valid JSON content", {
  mockery::stub(read_profile, "file.exists", TRUE)
  mockery::stub(read_profile, "jsonlite::read_json", list(key = "value", nested = list(a = 1, b = 2)))
  expect_equal(read_profile("valid.json"), list(key = "value", nested = list(a = 1, b = 2)))
})

test_that("read_profile handles invalid JSON content", {
  mockery::stub(read_profile, "file.exists", TRUE)
  mockery::stub(read_profile, "jsonlite::read_json", function(...) stop("JSON parse error: invalid token"))
  expect_error(read_profile("invalid.json"), "Error reading or parsing the JSON file")
})

test_that("set_active_profile sets the profile correctly", {
  local_profile()
  mockery::stub(set_active_profile, "read_profile", list(name = "test_profile"))

  expect_invisible(result <- set_active_profile("path/to/config.json"))

  expect_equal(
    get_active_profile(),
    list(
      profile_name = list(name = "test_profile"),
      profile_path = "path/to/config.json"
    )
  )
  expect_equal(result, list(name = "test_profile"))
})

test_that("set_active_profile correctly updates global state", {
  local_profile()
  mockery::stub(set_active_profile, "read_profile", list(name = "test_profile"))

  set_active_profile("path/to/config.json")

  expect_equal(
    get_active_profile(),
    list(
      profile_name = list(name = "test_profile"),
      profile_path = "path/to/config.json"
    )
  )
})

test_that("set_active_profile handles invalid file path", {
  # Mock read_profile to throw an error
  mockery::stub(set_active_profile, "read_profile", function(...) stop("Config file not found"))

  # Expect an error when setting an invalid profile
  expect_error(set_active_profile("invalid/path.json"), "Config file not found")
})

test_that("load_default_profile loads profile when config file exists", {
  local_profile() # Ensure global state is cleaned up after the test
  mockery::stub(load_default_profile, "fs::file_exists", TRUE)
  mockery::stub(load_default_profile, "set_active_profile", function(path) {
    .globals[["mf_profile"]] <- list(name = "default_profile")
    invisible(.globals[["mf_profile"]])
  })

  local_reproducible_output(crayon = TRUE)
  expect_snapshot(load_default_profile())

  expect_equal(
    get_active_profile(),
    list(
      profile_name = list(name = "default_profile"),
      profile_path = NULL
    )
  )
})

test_that("load_default_profile handles missing config file", {
  local_profile() # Ensure global state is cleaned up after the test
  mockery::stub(load_default_profile, "fs::file_exists", FALSE)

  local_reproducible_output(crayon = TRUE)
  expect_snapshot(load_default_profile())

  expect_equal(
    get_active_profile(),
    list(
      profile_name = NULL,
      profile_path = NULL
    )
  )
})
