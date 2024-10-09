test_config_path <- test_path("config_test.json")

test_that("reads a valid json file and returns its content", {
  result <- read_profile_json_file(test_config_path)

  expect_snapshot(result)
})

test_that("throws an error if the file does not exist", {
  expect_snapshot(error = TRUE, {
    read_profile_json_file("nonexistent.json")
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

test_that("get_active_profile returns the active profile correctly", {
  withr::local_envvar(METAFLOW_HOME = NA)

  withr::with_environment(new.env(parent = emptyenv()), {
    test_profile <- read_profile_json_file(test_config_path)
    .globals[["mf_profile"]] <- test_profile
    .globals[["mf_profile_path"]] <- test_config_path
    result <- get_active_profile()
    expect_equal(result$name, "test")
    expect_equal(result$path, test_config_path)
    expect_equal(result$values, test_profile)
  })
})

test_that("is_valid_metaflow_home_dir correctly validates directory", {
  # valid directory
  mockery::stub(is_valid_metaflow_home_dir, "fs::dir_exists", function(...) TRUE)
  mockery::stub(is_valid_metaflow_home_dir, "fs::dir_ls", function(...) c("config.json", "config_test.json"))
  expect_true(is_valid_metaflow_home_dir("/valid/path"))

  # invalid directory
  mockery::stub(is_valid_metaflow_home_dir, "fs::dir_exists", function(...) FALSE)
  expect_false(is_valid_metaflow_home_dir("/invalid/path"))

  # directory without config files
  mockery::stub(is_valid_metaflow_home_dir, "fs::dir_exists", function(...) TRUE)
  mockery::stub(is_valid_metaflow_home_dir, "fs::dir_ls", function(...) character(0))
  expect_false(is_valid_metaflow_home_dir("/empty/path"))
})

test_that("list_metaflow_home_profiles returns correct profiles", {
  mockery::stub(list_metaflow_home_profiles, "get_metaflow_home", function() "/mock/metaflow/home")
  mockery::stub(list_metaflow_home_profiles, "fs::dir_ls", function(...) c("/mock/metaflow/home/config.json", "/mock/metaflow/home/config_test.json"))

  result <- list_metaflow_home_profiles()
  expect_equal(length(result), 2)
  expect_true(all(grepl("^/mock/metaflow/home/config", result)))
})

test_that("get_all_metaflow_profiles returns correct profile list", {
  mockery::stub(get_all_metaflow_profiles, "list_metaflow_home_profiles", function() c("/mock/metaflow/home/config.json", "/mock/metaflow/home/config_test.json"))

  result <- get_all_metaflow_profiles()
  expect_equal(length(result), 2)
  expect_equal(names(result), c("default", "test"))
  expect_true(all(grepl("^/mock/metaflow/home/config", unlist(result))))
})
