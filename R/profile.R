#' Get Active Metaflow Profile
#'
#' This function retrieves the name or data of the currently active Metaflow profile.
#'
#' @param return_data Logical, if TRUE, returns the full profile data. Default is FALSE.
#' @return If return_data is FALSE (default), returns a string containing the name of the active profile.
#'         If return_data is TRUE, returns the full profile data.
#' @export
get_active_profile <- function(return_data = FALSE) {
  profile_path <- .globals[["mf_profile_path"]]
  return(make_profile_name(profile_path))
}

#' Get all Metaflow profile configurations
#'
#' This function reads all Metaflow profile configurations from the METAFLOW_HOME directory.
#'
#' @return A named list of file paths for all Metaflow profile configurations.
#' @examples
#' \dontrun{
#' profiles <- read_all_home_profiles()
#' }
#' @keywords internal
read_all_home_profiles <- function() {
  # Get METAFLOW_HOME environment variable or set to default
  metaflow_home <- Sys.getenv(
    "METAFLOW_HOME",
    unset = fs::path_home(".metaflowconfig")
  )
  config_dir <- metaflow_home

  # Check if the directory exists
  if (!fs::dir_exists(config_dir)) {
    cli::cli_abort(c(
      "No profiles found in {.file {metaflow_home}}.",
      i = "Ensure that the directory exists and contains profile",
      "  configuration files."
    ))
  }

  # List all files in the directory
  all_files <- fs::dir_ls(metaflow_home, type = "file")
  config_files <- all_files[grepl("^config.*\\.json$", basename(all_files))]

  # Check if any config files are found
  if (length(config_files) == 0L) {
    cli::cli_abort(c(
      "No profiles found in {.file {metaflow_home}}.",
      i = "Ensure that the directory contains profile configuration files."
    ))
  }

  # Convert fs_path to a named list of strings
  config_list <- as.list(as.character(config_files))
  names(config_list) <- basename(config_files)

  return(config_list)
}

#' Read a Metaflow profile configuration file
#'
#' This function reads and parses a JSON file containing a Metaflow profile configuration.
#'
#' @param path A string specifying the path to the JSON configuration file.
#' @return A list containing the parsed JSON data.
#' @keywords internal
read_profile_json_file <- function(path) {
  if (!endsWith(path, ".json")) {
    cli::cli_abort("Invalid file extension")
  }
  if (!file.exists(path)) {
    cli::cli_abort("Config file not found")
  }
  # Attempt to read and parse the JSON file
  tryCatch(
    {
      config <- jsonlite::read_json(path)
      return(config)
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

#' Overwrites the global profile
#'
#' This function activates a new profile by overwriting the global profile settings.
#'
#' @param path A string specifying the path to the new profile configuration file.
#' @return Invisibly returns the new profile settings.
#' @keywords internal
activate_profile_globals <- function(path) {
  checkmate::assert_string(path)
  profile <- read_profile_json(path)
  .globals[["mf_profile"]] <- profile
  .globals[["mf_profile_path"]] <- path
  invisible(profile)
}

#' Updates the default Metaflow profile
#'
#' This function updates the default Metaflow profile by reading the config.json file
#' from the METAFLOW_HOME directory and setting it as the active profile.
#'
#' @return Invisibly returns the new profile settings, or NULL if no config file was found.
#' @keywords internal
update_profile <- function() {
  # Get METAFLOW_HOME environment variable or set to default
  metaflow_home <- Sys.getenv(
    "METAFLOW_HOME",
    unset = fs::path_home(".metaflowconfig")
  )

  # Define the default config file path using METAFLOW_HOME
  default_config_path <- fs::path(metaflow_home, "config.json")

  if (fs::file_exists(default_config_path)) {
    # Set the active profile using the existing function
    set_active_profile(default_config_path)
    # Report success message
    cli::cli_alert_success(
      "Loaded default Metaflow profile from {.file {default_config_path}}."
    )
    cli::cli_alert_info("Using the default profile.")
    invisible(.globals[["mf_profile"]])
  } else {
    # Report that no existing config was found
    cli::cli_alert_warning(
      "No existing Metaflow config found at {.file {default_config_path}}."
    )
    cli::cli_alert_info("Metaflow will be configured for local execution.")
    invisible(NULL)
  }
}

#' Create a profile name from a file path
#'
#' This function extracts the profile name from a given file path by removing the file extension.
#'
#' @param path A string specifying the file path.
#' @return A string containing the profile name (filename without extension).
#' @keywords internal
make_profile_name <- function(path) {
  filename_with_ext <- basename(path)
  return(tools::file_path_sans_ext(filename_with_ext))
}
