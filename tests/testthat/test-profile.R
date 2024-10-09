test_config_path <- test_path("config_test.json")

# snapshots
test_that("reads a valid json file and returns its content", {
  result <- read_profile_json(test_config_path)

  expect_snapshot(result)
})

test_that("throws an error if the file does not exist", {
  expect_snapshot(error = TRUE, {
    read_profile_json("nonexistent.json")
  })
})

test_that("get_metaflow_home returns correct directory", {
  # METAFLOW_HOME is set
  withr::with_envvar(new = c("METAFLOW_HOME" = "/custom/metaflow/home"), {
    expect_snapshot(get_metaflow_home())
  })

  # METAFLOW_HOME is not set
  withr::with_envvar(new = c("METAFLOW_HOME" = NA), {
    mock_path_home <- function(...) "/mock/home"
    mockery::stub(get_metaflow_home, "fs::path_home", mock_path_home)
    mockery::stub(get_metaflow_home, "fs::dir_exists", function(...) TRUE)
    expect_snapshot(get_metaflow_home())
  })

  # default directory doesn't exist
  withr::with_envvar(new = c("METAFLOW_HOME" = NA), {
    mock_path_home <- function(...) "/nonexistent/home"
    mockery::stub(get_metaflow_home, "fs::path_home", mock_path_home)
    mockery::stub(get_metaflow_home, "fs::dir_exists", function(...) FALSE)
    expect_snapshot(get_metaflow_home())
  })
})

test_that("list_profiles returns a tibble with correct profiles", {
  temp_metaflow_home <- withr::local_tempdir()
  
  # mock get_metaflow_home()
  mockery::stub(list_profiles, "get_metaflow_home", function() temp_metaflow_home)
  
  # mock profile files
  writeLines('{"profile": "default"}', con = fs::path(temp_metaflow_home, "config.json"))
  writeLines('{"profile": "test"}', con = fs::path(temp_metaflow_home, "config_test.json"))
  writeLines('{"profile": "personal"}', con = fs::path(temp_metaflow_home, "config_personal.json"))
  
  result <- list_profiles()
  result$path <- fs::path_file(result$path)
  
  expect_snapshot(result)
})

test_that("list_profiles errors when no profiles are found", {
  # metaflow_home with no profiles
  temp_metaflow_home <- withr::local_tempdir()
  
  # mock get_metaflow_home()
  mockery::stub(list_profiles, "get_metaflow_home", function() temp_metaflow_home)
  
  expect_snapshot(
    error = TRUE,
    {
      list_profiles()
    },
    transform = function(x) {
      # to get rid of tmp paths
      x <- gsub(temp_metaflow_home, "<temp_dir>", x, fixed = TRUE)
      x <- gsub("\\\\", "/", x)
      x
    }
  )
})

# implementation tests
test_that("get_active_profile returns the active profile correctly", {
  withr::local_envvar(METAFLOW_HOME = NA)

  withr::with_environment(new.env(parent = emptyenv()), {
    test_profile <- read_profile_json(test_config_path)
    .globals[["mf_profile"]] <- test_profile
    .globals[["mf_profile_path"]] <- test_config_path
    result <- get_active_profile()
    expect_equal(result$name, "test")
    expect_equal(result$path, test_config_path)
    expect_equal(result$values, test_profile)
  })
})

test_that("is_valid_metaflow_home correctly validates directory", {
  # valid directory
  mockery::stub(is_valid_metaflow_home, "fs::dir_exists", function(...) TRUE)
  mockery::stub(is_valid_metaflow_home, "fs::dir_ls", function(...) c("config.json", "config_test.json"))
  expect_true(is_valid_metaflow_home("/valid/path"))

  # invalid directory
  mockery::stub(is_valid_metaflow_home, "fs::dir_exists", function(...) FALSE)
  expect_false(is_valid_metaflow_home("/invalid/path"))

  # directory without config files
  mockery::stub(is_valid_metaflow_home, "fs::dir_exists", function(...) TRUE)
  mockery::stub(is_valid_metaflow_home, "fs::dir_ls", function(...) character(0))
  expect_false(is_valid_metaflow_home("/empty/path"))
})

test_that("list_profiles returns a tibble with correct profiles (using mocks)", {
  # mock get_metaflow_home to return a fake directory
  mockery::stub(list_profiles, "get_metaflow_home", function() "/fake/metaflow/home")
  
  # mock checkmate::assert_directory_exists to do nothing
  mockery::stub(list_profiles, "checkmate::assert_directory_exists", function(...) TRUE)
  
  # mock fs::dir_ls to return mock file paths
  mockery::stub(list_profiles, "fs::dir_ls", function(...) {
    fs::path(c(
      "/fake/metaflow/home/config.json",
      "/fake/metaflow/home/config_test.json"
    ))
  })
  
  # mock make_profile_name to return profile names based on file paths
  mockery::stub(list_profiles, "make_profile_name", function(path) {
    if (grepl("config\\.json$", path)) return("default")
    sub("config_(.*)\\.json$", "\\1", basename(path))
  })
  
  result <- list_profiles()
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("profile_name", "path"))
  expect_equal(result$profile_name, c("default", "test"))
  expect_equal(as.character(result$path), c(
    "/fake/metaflow/home/config.json",
    "/fake/metaflow/home/config_test.json"
  ))
})

test_that("list_profiles handles no profiles found", {
  # temporary directory for metaflow_home
  temp_metaflow_home <- withr::local_tempdir()
  
  # mock get_metaflow_home() to return the temporary directory
  mockery::stub(list_profiles, "get_metaflow_home", function() temp_metaflow_home)

  expect_error(
    list_profiles(),
    regex = "No profiles found in"
  )
})