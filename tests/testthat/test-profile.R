# helper for transforming snapshot output to remove temp paths
transform_temp_paths <- function(x) {
  x <- gsub("\\\\", "/", x) # Normalize path separators
  x <- gsub("/var/folders/[^/]+/[^/]+/[^/]+/[^/]+/", "<temp_dir>/", x)
  x <- gsub("/tmp/Rtmp[A-Za-z0-9]+/", "<temp_dir>/", x)
  x <- gsub("file[a-f0-9]+", "<temp_file>", x)
  x <- gsub("/private/var/", "/var/", x)
  x <- gsub("Rtmp[A-Za-z0-9]+", "<temp_dir>", x)
  x <- gsub("/Users/[^/]+/", "<home_dir>/", x)
  x
}

test_config_path <- test_path("_fixtures/config_test.json")

# system checks
test_that("reads a valid json file and returns its content", {
  expect_snapshot(read_profile_json(test_config_path))
})

test_that("throws an error if the file does not exist", {
  expect_snapshot(read_profile_json("nonexistent.json"), error = TRUE)
})

# metaflow home directory
test_that("get_metaflow_home handles various scenarios correctly", {
  # METAFLOW_HOME is set to a valid directory
  withr::with_envvar(new = c("METAFLOW_HOME" = "/custom/metaflow/home"), {
    testthat::local_mocked_bindings(
      is_valid_metaflow_home = function(...) TRUE,
      .package = "metaflow"
    )
    expect_snapshot(get_metaflow_home(), transform = transform_temp_paths)
  })

  # METAFLOW_HOME is set but directory doesn't exist
  withr::with_envvar(new = c("METAFLOW_HOME" = "/nonexistent/metaflow/home"), {
    testthat::local_mocked_bindings(
      path_home = function(...) "/mock/home",
      .package = "fs"
    )
    testthat::local_mocked_bindings(
      is_valid_metaflow_home = function(path) path == "/mock/home/.metaflowconfig",
      .package = "metaflow"
    )
    expect_snapshot(
      {
        withr::with_options(
          list(warn = 1),
          get_metaflow_home()
        )
      },
      transform = transform_temp_paths
    )
  })

  # METAFLOW_HOME not set; default home is valid
  withr::with_envvar(new = c("METAFLOW_HOME" = NA), {
    testthat::local_mocked_bindings(
      path_home = function(...) "/mock/home",
      is_valid_metaflow_home = function(path) path == "/mock/home/.metaflowconfig",
      .package = "metaflow"
    )
    expect_snapshot(get_metaflow_home(), transform = transform_temp_paths)
  })

  # METAFLOW_HOME not set; default home is invalid
  withr::with_envvar(new = c("METAFLOW_HOME" = NA), {
    testthat::local_mocked_bindings(
      path_home = function(...) "/mock/home",
      is_valid_metaflow_home = function(...) FALSE,
      .package = "metaflow"
    )
    expect_snapshot(
      {
        withr::with_options(
          list(warn = 1),
          get_metaflow_home()
        )
      },
      transform = transform_temp_paths
    )
  })
})

# profile listing
test_that("list_profiles returns a tibble with correct profiles", {
  temp_metaflow_home <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    get_metaflow_home = function() temp_metaflow_home,
    .package = "metaflow"
  )

  writeLines('{"profile": "default"}', con = fs::path(temp_metaflow_home, "config.json"))
  writeLines('{"profile": "test"}', con = fs::path(temp_metaflow_home, "config_test.json"))
  writeLines('{"profile": "personal"}', con = fs::path(temp_metaflow_home, "config_personal.json"))

  result <- list_profiles()
  result$path <- fs::path_file(result$path)

  expect_snapshot(result, transform = transform_temp_paths, variant = "profiles")
})

test_that("list_profiles errors when no profiles are found", {
  temp_metaflow_home <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    get_metaflow_home = function() temp_metaflow_home,
    .package = "metaflow"
  )

  expect_snapshot(list_profiles(), error = TRUE, transform = transform_temp_paths)
})

# active profile
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

# metaflow home validation
test_that("is_valid_metaflow_home correctly validates directory", {
  testthat::local_mocked_bindings(
    dir_exists = function(...) TRUE,
    dir_ls = function(...) c("config.json", "config_test.json"),
    .package = "fs"
  )
  expect_true(is_valid_metaflow_home("/valid/path"))

  testthat::local_mocked_bindings(
    dir_exists = function(...) FALSE,
    .package = "fs"
  )
  expect_false(is_valid_metaflow_home("/invalid/path"))

  testthat::local_mocked_bindings(
    dir_exists = function(...) TRUE,
    dir_ls = function(...) character(0),
    .package = "fs"
  )
  expect_false(is_valid_metaflow_home("/empty/path"))
})

# profile updating
test_that("update_profile handles name and path correctly", {
  temp_metaflow_home <- withr::local_tempdir()
  test_profile_path <- fs::path(temp_metaflow_home, "config_test.json")
  writeLines('{"profile": "test"}', con = test_profile_path)

  testthat::local_mocked_bindings(
    get_metaflow_home = function() temp_metaflow_home,
    .package = "metaflow"
  )

  expect_snapshot(update_profile(name = "test"), transform = transform_temp_paths)
  expect_snapshot(update_profile(path = test_profile_path), transform = transform_temp_paths)
  expect_snapshot(update_profile(), error = TRUE)
  expect_snapshot(update_profile(name = "test", path = test_profile_path), error = TRUE)
})
