#' Read a Metaflow profile configuration file
#'
#' @keywords internal
read_profile_json_file <- function(path) {
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
is_valid_metaflow_home_dir <- function(dir_path) {
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
  valid_dirs <- vapply(home_dirs, is_valid_metaflow_home_dir, logical(1L))

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

#' Overwrites the global profile
#'
#' @keywords internal
update_profile_globals <- function(path) {
  checkmate::assert_string(path)
  profile <- read_profile_json_file(path)
  .globals[["mf_profile"]] <- profile
  .globals[["mf_profile_path"]] <- path
  invisible(profile)
}

#' Set the default Metaflow profile
#'
#' @keywords internal
load_default_profile <- function() {
  metaflow_home <- suppressWarnings(get_metaflow_home())
  default_config_path <- fs::path(metaflow_home, "config.json")
  update_profile_globals(default_config_path)
}

#' List all profiles in Metaflow home directory
#'
#' @keywords internal
list_metaflow_home_profiles <- function() {
  metaflow_home <- get_metaflow_home()
  if (is.null(metaflow_home)) {
    return(NULL)
  }
  fs::dir_ls(metaflow_home, type = "file", glob = "*config*.json")
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
    values = read_profile_json_file(profile_path)
  )
}

#' Get all Metaflow profile configurations
#'
#' @keywords internal
get_all_metaflow_profiles <- function() {
  metaflow_home <- get_metaflow_home()
  profile_json_files <- list_metaflow_home_profiles()

  # Check if profiles are found
  if (is.null(profile_json_files)) {
    cli::cli_abort(c(
      "No profiles found in {.file {metaflow_home}}.",
      i = "Ensure that the directory exists and contains profile configuration files."
    ))
  }

  # Convert fs_path to a named list of strings
  config_list <- as.list(as.character(profile_json_files))

  # Use purrr to iterate through the list and set names using make_profile_name
  config_list <- purrr::set_names(config_list, purrr::map_chr(config_list, make_profile_name))

  return(config_list)
}
