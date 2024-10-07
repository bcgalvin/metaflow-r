#' Get Active Metaflow Profile
#'
#' Retrieve and display information about the currently active Metaflow profile.
#'
#' @param return_data Logical value indicating whether to return the profile data as a data frame.
#' @return If `return_data` is `TRUE`, a data frame with the profile details is returned.
#'   Otherwise, `NULL` is returned invisibly after displaying the profile information in the console.
#' @examples
#' \dontrun{
#' # Display the active profile in the console
#' get_active_metaflow_profile()
#'
#' # Get the active profile data as a data frame
#' profile_data <- get_active_metaflow_profile(return_data = TRUE)
#' }
#' @seealso [get_active_profile()]
#' @export
get_active_metaflow_profile <- function(return_data = FALSE) {
  active_profile <- get_active_profile()
  profile <- active_profile[["profile_name"]]
  process_metaflow_profile_data(profile, "Active Metaflow Profile", return_data)
}

#' Get Available Metaflow Profiles
#'
#' Retrieve and display a list of all available Metaflow profiles.
#'
#' @param return_data Logical value indicating whether to return the profiles data as a data frame.
#' @return If `return_data` is `TRUE`, a data frame with the profiles is returned.
#'   Otherwise, `NULL` is returned invisibly after displaying the profiles in the console.
#' @examples
#' \dontrun{
#' # Display all available profiles in the console
#' get_available_metaflow_profiles()
#'
#' # Get the profiles data as a data frame
#' profiles_data <- get_available_metaflow_profiles(return_data = TRUE)
#' }
#' @seealso [get_profiles()]
#' @export
get_available_metaflow_profiles <- function(return_data = FALSE) {
  data <- get_profiles()
  profiles <- setNames(data, basename(data))
  process_metaflow_profile_data(profiles, "Available Metaflow Profiles", return_data, "profile", "path")
}

#' Get the current active Metaflow profile
#' @keywords internal
get_active_profile <- function() {
  list(
    profile_name = .globals[["mf_profile"]],
    profile_path = .globals[["mf_profile_path"]]
  )
}

#' Get all Metaflow profile configurations
#' @keywords internal
get_profiles <- function() {
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

  return(config_files)
}

#' Read a Metaflow profile configuration file
#' @keywords internal
read_profile <- function(path) {
  checkmate::assert_string(path)
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

#' Set the active Metaflow profile
#' @keywords internal
set_active_profile <- function(path) {
  checkmate::assert_string(path)
  profile <- read_profile(path)
  .globals[["mf_profile"]] <- profile
  .globals[["mf_profile_path"]] <- path
  invisible(profile)
}

#' Load the default Metaflow profile
#' @keywords internal
load_default_profile <- function() {
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

#' Helper function to process and display Metaflow profile data
#' @keywords internal
process_metaflow_profile_data <- function(data, title, return_data = FALSE, key_col = "Key", value_col = "Value") {
  if (is.null(data) || length(data) == 0L) {
    cli::cli_alert_warning(paste("No", tolower(title), "found."))
    return(invisible(NULL))
  }

  data_df <- data.frame(
    Key = names(data),
    Value = unlist(data),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  names(data_df) <- c(key_col, value_col)

  if (return_data) {
    names(data_df) <- tolower(names(data_df))
    return(data_df)
  }

  cli::cli_h1(title)
  cli::cli_ul(
    paste0("{.strong ", data_df[[1L]], "}: ", data_df[[2L]])
  )

  invisible(NULL)
}
