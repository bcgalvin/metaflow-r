# install_metaflow installs correct packages [plain]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      v Metaflow installation complete.

# install_metaflow installs correct packages [ansi]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      [32mv[39m [34m[34mMetaflow[34m[39m installation complete.

# install_metaflow installs correct packages [unicode]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      ✔ Metaflow installation complete.

# install_metaflow installs correct packages [fancy]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      [32m✔[39m [34m[34mMetaflow[34m[39m installation complete.

# install_metaflow handles conda installations correctly [plain]

    Code
      install_metaflow(method = "conda", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      v Metaflow installation complete.

# install_metaflow handles conda installations correctly [ansi]

    Code
      install_metaflow(method = "conda", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      [32mv[39m [34m[34mMetaflow[34m[39m installation complete.

# install_metaflow handles conda installations correctly [unicode]

    Code
      install_metaflow(method = "conda", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      ✔ Metaflow installation complete.

# install_metaflow handles conda installations correctly [fancy]

    Code
      install_metaflow(method = "conda", version = "2.7.3", extra_packages = "numpy",
        restart_session = FALSE)
    Message
      [32m✔[39m [34m[34mMetaflow[34m[39m installation complete.

# install_metaflow installs latest version when 'default' is specified [plain]

    Code
      install_metaflow(method = "virtualenv", version = "default", restart_session = FALSE)
    Message
      v Metaflow installation complete.

# install_metaflow installs latest version when 'default' is specified [ansi]

    Code
      install_metaflow(method = "virtualenv", version = "default", restart_session = FALSE)
    Message
      [32mv[39m [34m[34mMetaflow[34m[39m installation complete.

# install_metaflow installs latest version when 'default' is specified [unicode]

    Code
      install_metaflow(method = "virtualenv", version = "default", restart_session = FALSE)
    Message
      ✔ Metaflow installation complete.

# install_metaflow installs latest version when 'default' is specified [fancy]

    Code
      install_metaflow(method = "virtualenv", version = "default", restart_session = FALSE)
    Message
      [32m✔[39m [34m[34mMetaflow[34m[39m installation complete.

# install_metaflow handles unsupported Python version correctly [plain]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      Error in `check_python_version()`:
      ! Unsupported Python version

# install_metaflow handles unsupported Python version correctly [ansi]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      [1m[33mError[39m in `check_python_version()`:[22m
      [33m![39m Unsupported Python version

# install_metaflow handles unsupported Python version correctly [unicode]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      Error in `check_python_version()`:
      ! Unsupported Python version

# install_metaflow handles unsupported Python version correctly [fancy]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      [1m[33mError[39m in `check_python_version()`:[22m
      [33m![39m Unsupported Python version

# install_metaflow handles environment preparation errors [plain]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      Error in `prepare_environment()`:
      ! Failed to prepare environment

# install_metaflow handles environment preparation errors [ansi]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      [1m[33mError[39m in `prepare_environment()`:[22m
      [33m![39m Failed to prepare environment

# install_metaflow handles environment preparation errors [unicode]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      Error in `prepare_environment()`:
      ! Failed to prepare environment

# install_metaflow handles environment preparation errors [fancy]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      [1m[33mError[39m in `prepare_environment()`:[22m
      [33m![39m Failed to prepare environment

# install_metaflow handles package installation errors [plain]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      Error in `reticulate::py_install()`:
      ! Failed to install packages

# install_metaflow handles package installation errors [ansi]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      [1m[33mError[39m in `reticulate::py_install()`:[22m
      [33m![39m Failed to install packages

# install_metaflow handles package installation errors [unicode]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      Error in `reticulate::py_install()`:
      ! Failed to install packages

# install_metaflow handles package installation errors [fancy]

    Code
      install_metaflow(method = "virtualenv", version = "2.7.3")
    Condition
      [1m[33mError[39m in `reticulate::py_install()`:[22m
      [33m![39m Failed to install packages

# metaflow_version provides informative error message [plain]

    Code
      metaflow_version()
    Condition
      Error in `metaflow_version()`:
      ! Metaflow is not available. Please install Metaflow using install_metaflow().

# metaflow_version provides informative error message [ansi]

    Code
      metaflow_version()
    Condition
      [1m[33mError[39m in `metaflow_version()`:[22m
      [33m![39m Metaflow is not available. Please install Metaflow using install_metaflow().

# metaflow_version provides informative error message [unicode]

    Code
      metaflow_version()
    Condition
      Error in `metaflow_version()`:
      ! Metaflow is not available. Please install Metaflow using install_metaflow().

# metaflow_version provides informative error message [fancy]

    Code
      metaflow_version()
    Condition
      [1m[33mError[39m in `metaflow_version()`:[22m
      [33m![39m Metaflow is not available. Please install Metaflow using install_metaflow().

