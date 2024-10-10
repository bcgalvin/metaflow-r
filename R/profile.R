#' List all profiles in Metaflow home directory
#'
#' @export
list_profiles <- function() {
  metaflow_home <- get_metaflow_home()
  checkmate::assert_directory_exists(metaflow_home)
  profile_paths <- fs::dir_ls(metaflow_home, type = "file", glob = "*config*.json")

  # Check if profiles are found
  if (length(profile_paths) == 0L) {
    cli::cli_abort(c(
      "No profiles found in {.file {metaflow_home}}.",
      i = "Ensure that the directory exists and contains configuration json files."
    ))
  }

  profile_names <- purrr::map_chr(profile_paths, make_profile_name)

  tibble::tibble(
    profile_name = profile_names,
    path = profile_paths
  )
}

#' Get Active Metaflow Profile
#'
#' @export
get_active_profile <- function() {
  if (is.null(.globals[["mf_profile"]])) {
    cli::cli_warn(c(
      "No Metaflow configuration file found",
      i = "Backend set to run in local mode."
    ))
    return(invisible(NULL))
  }
  profile_path <- .globals[["mf_profile_path"]]
  list(
    name = make_profile_name(profile_path),
    path = profile_path,
    values = read_profile_json(profile_path)
  )
}

#' Updates the Metaflow profile by name or path
#'
#' @param name Optional. A string representing the profile name.
#' @param path Optional. A string representing the path to the profile JSON file.
#' @return Invisibly returns the updated profile
#' @export
update_profile <- function(name = NULL, path = NULL) {
  if (!is.null(name) && !is.null(path)) {
    cli::cli_abort("Please provide either `name` or `path`, not both.")
  }
  if (is.null(name) && is.null(path)) {
    cli::cli_abort("Please provide either `name` or `path`.")
  }

  update_fn <- if (!is.null(name)) update_profile_name else update_profile_path
  update_fn(name %||% path)
}

#' Updates the Metaflow profile by path
#'
#' @keywords internal
update_profile_path <- function(path) {
  checkmate::assert_file(path, extension = "json")
  profile <- read_profile_json(path)
  .globals[["mf_profile"]] <- profile
  .globals[["mf_profile_name"]] <- make_profile_name(path)
  .globals[["mf_profile_path"]] <- path
  Sys.setenv(METAFLOW_PROFILE = .globals[["mf_profile_name"]])
  invisible(profile)
}

#' Updates the Metaflow profile by name
#'
#' @keywords internal
update_profile_name <- function(name) {
  checkmate::assert_string(name)
  profiles <- list_profiles()
  profile_path <- profiles[["path"]][[which(profiles[["profile_name"]] == name)]]
  update_profile_path(profile_path)
  invisible(profile_path)
}

#' Read a Metaflow profile configuration file
#'
#' @keywords internal
read_profile_json <- function(path) {
  checkmate::assert_file(path, extension = "json")

  tryCatch(
    {
      config <- jsonlite::read_json(path)
      config
    },
    error = function(e) {
      cli::cli_abort(c(
        "Error reading or parsing the JSON file.",
        x = "Failed to read or parse {path}.",
        i = c(
          "Please check that the file contains valid JSON.",
          "Error message: {conditionMessage(e)}"
        )
      ))
    }
  )
}

#' Check if a provided directory exists and contains valid config files
#'
#' @keywords internal
is_valid_metaflow_home <- function(dir_path) {
  fs::dir_exists(dir_path) &&
    length(fs::dir_ls(dir_path, type = "file", glob = "*config*.json")) >= 1L
}

#' Get Metaflow home directory
#'
#' @keywords internal
get_metaflow_home <- function() {
  provided_home <- Sys.getenv("METAFLOW_HOME")
  default_home <- as.character(fs::path_home(".metaflowconfig"))

  home_dirs <- c(provided = provided_home, default = default_home)
  valid_dirs <- vapply(home_dirs, is_valid_metaflow_home, logical(1L))

  if (valid_dirs[["provided"]]) {
    return(provided_home)
  }

  if (provided_home != "" && !valid_dirs[["provided"]]) {
    cli::cli_warn(c(
      "!" = "METAFLOW_HOME environment variable is set to {provided_home}, but directory does not exist."
    ))
  }

  if (valid_dirs[["default"]]) {
    return(default_home)
  }

  cli::cli_warn(c(
    "!" = "Default metaflow home location {default_home} does not exist on this system or",
    "does not have valid config files matching the glob pattern `*config*.json`"
  ))
  NULL
}

#' Generate profile name from a file path
#'
#' @keywords internal
make_profile_name <- function(path) {
  filename <- tools::file_path_sans_ext(basename(path))

  switch(startsWith(filename, "config"),
    if (filename == "config") "default" else sub("^config_", "", filename),
    filename
  )
}

#' Set the default Metaflow profile
#'
#' @keywords internal
load_default_profile <- function() {
  metaflow_home <- suppressWarnings(get_metaflow_home())
  if (!is.null(metaflow_home)) {
    default_config_path <- fs::path(metaflow_home, "config.json")
    update_profile_path(default_config_path)
  }
}
