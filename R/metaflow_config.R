#' MetaflowConfig Class
#'
#' @description
#' An R6 class for managing Metaflow configurations in-memory and through environment variables.
#'
#' @import R6
#' @import cli
#' @export
# nolint start
# Justification for keeping PascalCase: This follows R6 class naming conventions.
# Justification for cyclomatic complexity: The complexity is necessary for the
# full functionality of this configuration management class. Refactoring would
# require significant changes to the class structure and API.
MetaflowConfig <- R6::R6Class("MetaflowConfig",
  # nolint end
  public = list(
    #' @description
    #' Initialize a new MetaflowConfig object.
    #' @param active_env The initial active environment.
    #' @return A new MetaflowConfig object.
    initialize = function(active_env = NULL) {
      private$.config <- .globals[["mf_config"]] %||% list()
      private$.config_spec <- yaml::read_yaml(
        system.file("config.yml", package = "metaflow")
      )
      if (is.null(active_env)) {
        active_env <- Sys.getenv("R_CONFIG_ACTIVE", "default")
      }
      private$.active_env <- active_env

      # Ensure default environment exists
      if (!("default" %in% names(private$.config))) {
        private$.config[["default"]] <- list()
      }

      # Ensure active environment exists
      if (!private$.active_env %in% names(private$.config)) {
        private$.config[[private$.active_env]] <- list()
      }

      # Merge default and active environment configurations
      if (private$.active_env != "default") {
        private$.config[[private$.active_env]] <- utils::modifyList(
          private$.config[["default"]],
          private$.config[[private$.active_env]],
          keep.null = TRUE
        )
      }

      # Update .globals to reflect the changes
      .globals[["mf_config"]] <- private$.config

      # Ensure environment variables take precedence over config values
      self$update_from_env()

      invisible(self)
    },
    update_from_env = function() {
      for (key in names(private$.config[[private$.active_env]])) {
        env_var <- paste0("METAFLOW_", toupper(gsub("[^A-Za-z0-9]", "_", key)))
        env_value <- Sys.getenv(env_var, unset = NA)
        if (!is.na(env_value)) {
          private$.config[[private$.active_env]][[key]] <- env_value
        }
      }
    },

    #' @description
    #' Get a configuration value.
    #' @param key The configuration key to retrieve.
    #' @param default The default value if the key is not found.
    #' @return The configuration value.
    get = function(key = NULL, default = NULL) {
      if (is.null(key)) {
        return(private$.config[[private$.active_env]])
      }

      # Check for environment variable override
      env_var <- paste0("METAFLOW_", toupper(gsub("[^A-Za-z0-9]", "_", key)))
      env_value <- Sys.getenv(env_var, unset = NA)
      if (!is.na(env_value)) {
        return(env_value)
      }

      # Get value from active environment
      value <- private$.config[[private$.active_env]][[key]]

      if (is.null(value)) {
        # If not found in active environment, check default
        value <- private$.config[["default"]][[key]]
      }

      if (is.null(value)) {
        if (is.null(default)) {
          cli::cli_warn(c(
            "!" = "Key '{key}' not found in environment '{private$.active_env}' or default.",
            "i" = "Available keys in active env: {toString(names(private$.config[[private$.active_env]]))}"
          ))
        }
        value <- default
      }
      return(value)
    },

    #' @description
    #' Set a configuration value.
    #' @param key The configuration key to set.
    #' @param value The value to set.
    #' @return Invisibly returns self for method chaining.
    set = function(key, value) {
      if (!is.character(key) || length(key) != 1L || is.na(key) || key == "") {
        cli::cli_abort(c(
          "x" = "Invalid key: Key must be a non-empty character string of length 1.",
          "i" = "Provided key is of type '{typeof(key)}' with length {length(key)}."
        ))
      }
      # Validate the key and value
      if (!self$validate_value(key, value)) {
        cli::cli_abort(c(
          "x" = "Invalid value for key '{key}'.",
          "i" = "Please provide a value that matches the expected type."
        ))
      }
      private$.config[[private$.active_env]][[key]] <- value
      .globals[["mf_config"]] <- private$.config
      invisible(self)
    },

    #' @description
    #' Validate whether a supplied configuration value is valid according to
    #' inst/config.yml.
    #' @param key The configuration key to validate.
    #' @param value The value to validate.
    #' @return TRUE if the input configuration value is valid; FALSE otherwise.
    validate_value = function(key, value) {
      # Check if the key exists in the config specification
      config_item <- NULL
      for (item in private$.config_spec) {
        if (item$name == key) {
          config_item <- item
          break
        }
      }
      if (is.null(config_item)) {
        # If the key is not in the config specification, assume it's valid
        return(TRUE)
      }

      # Validate the value according to the type specified
      type <- config_item$type
      is_valid <- FALSE

      if (type == "character[1]") {
        is_valid <- is.character(value) && length(value) == 1L
      } else if (type == "character[*]") {
        is_valid <- is.character(value)
      } else if (type == "logical[1]") {
        is_valid <- is.logical(value) && length(value) == 1L
      } else if (type == "integer[1]") {
        is_valid <- (is.integer(value) ||
          (is.numeric(value) && value == as.integer(value))) &&
          length(value) == 1L
      } else if (type == "*") {
        # Allow any type for "*"
        is_valid <- TRUE
      } else {
        cli::cli_warn(c(
          `!` = "Unknown type '{type}' for key '{key}'.",
          i = "This may lead to unexpected behavior."
        ))
        return(FALSE)
      }

      return(is_valid)
    },

    #' @description
    #' Switch the active environment.
    #' @param env_name The name of the environment to switch to.
    #' @return Invisibly returns self for method chaining.
    switch_env = function(env_name) {
      if (!env_name %in% names(private$.config)) {
        private$.config[[env_name]] <- list()
      }
      private$.active_env <- env_name
      # Ensure the new environment inherits from default
      private$.config[[env_name]] <- utils::modifyList(private$.config[["default"]], private$.config[[env_name]])
      # Update .globals to reflect the changes
      .globals[["mf_config"]] <- private$.config
      # Ensure environment variables take precedence over config values
      self$update_from_env()
      invisible(self)
    },

    #' @description
    #' Validate if a key exists in the current environment.
    #' @param key The key to validate.
    #' @return TRUE if the key is valid, FALSE otherwise.
    validate_key = function(key) {
      if (!is.character(key) || length(key) != 1L || is.na(key) || key == "") {
        cli::cli_abort(c(
          "x" = "Invalid key: Key must be a non-empty character string of length 1.",
          "i" = "Provided key is of type '{typeof(key)}' with length {length(key)}."
        ))
      }
      # Check if the key exists in the active environment, default environment, or config specification
      exists <- !is.null(private$.config[[private$.active_env]][[key]]) ||
        !is.null(private$.config[["default"]][[key]]) ||
        any(vapply(private$.config_spec,
          function(item) item$name == key,
          FUN.VALUE = logical(1L)
        ))
      return(exists)
    },

    #' @description
    #' Save the current configurations to environment variables.
    #' @return Invisibly returns self for method chaining.
    save = function() {
      # Set environment variables based on the active environment configuration
      config <- private$.config[[private$.active_env]]
      if (length(config)) {
        for (key in names(config)) {
          env_var <- paste0(
            "METAFLOW_",
            toupper(gsub(".", "_", key, fixed = TRUE))
          )
          value <- as.character(config[[key]])
          do.call(Sys.setenv, setNames(list(value), env_var))
        }
      }
      invisible(self)
    },

    #' @description
    #' Print the configurations of the active environment.
    #' @return Invisibly returns self.
    print = function() {
      cli::cli_h1("Configurations for environment '{private$.active_env}'")
      config <- private$.config[[private$.active_env]]
      if (length(config) > 0L) {
        cli::cli_dl(
          setNames(
            lapply(config, as.character),
            names(config)
          )
        )
      } else {
        cli::cli_alert_info("No configurations found for this environment.")
      }
      invisible(self)
    },
    finalize = function() {
      # Set environment variables when the object is garbage collected
      self$save()
    }
  ),
  private = list(
    .config = NULL,
    .config_spec = NULL,
    .active_env = NULL,
    deep_clone = function(name, value) {
      if (name == ".config") {
        return(utils::modifyList(list(), value, keep.null = TRUE))
      }
      value
    }
  ),
  active = list(
    active_env = function(value) {
      if (missing(value)) {
        return(private$.active_env)
      }
      stop("active_env is read-only")
    }
  ),
  cloneable = FALSE
)
