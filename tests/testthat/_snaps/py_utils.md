# check_system handles Windows correctly

    Code
      check_system()
    Condition
      Error in `check_system()`:
      ! Metaflow installation is not supported on Windows.
      i Installation is only available for Mac and Linux systems.

# check_system allows non-Windows systems

    Code
      check_system()

# check_python_version handles unsupported Python versions correctly

    Code
      check_python_version("3.7.0")
    Condition
      Error in `check_python_version()`:
      ! Python version must be 3.8 or higher for Metaflow.

---

    Code
      check_python_version("3.6.0")
    Condition
      Error in `check_python_version()`:
      ! Python version must be 3.8 or higher for Metaflow.

---

    Code
      check_python_version("2.7.0")
    Condition
      Error in `check_python_version()`:
      ! Python version must be 3.8 or higher for Metaflow.

# check_python_version accepts supported Python versions

    Code
      check_python_version("3.8.0")
    Output
      [1] "3.8.0"

---

    Code
      check_python_version("3.9.0")
    Output
      [1] "3.9.0"

---

    Code
      check_python_version(NULL)
    Output
      [1] ">=3.8"

# prepare_environment handles conda environments correctly [plain]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Message
      Removing existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "conda")
    Message
      Using existing environment: test_env

# prepare_environment handles conda environments correctly [ansi]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "conda")
    Message
      [1m[22mUsing existing environment: test_env

# prepare_environment handles conda environments correctly [unicode]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Message
      Removing existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "conda")
    Message
      Using existing environment: test_env

# prepare_environment handles conda environments correctly [fancy]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "conda")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "conda")
    Message
      [1m[22mUsing existing environment: test_env

# prepare_environment handles virtualenv environments correctly [plain]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Message
      Removing existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "virtualenv")
    Message
      Using existing environment: test_env

# prepare_environment handles virtualenv environments correctly [ansi]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "virtualenv")
    Message
      [1m[22mUsing existing environment: test_env

# prepare_environment handles virtualenv environments correctly [unicode]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Message
      Removing existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "virtualenv")
    Message
      Using existing environment: test_env

# prepare_environment handles virtualenv environments correctly [fancy]

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Output
      NULL

---

    Code
      prepare_environment(TRUE, "test_env", "3.8", "virtualenv")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(FALSE, "test_env", "3.8", "virtualenv")
    Message
      [1m[22mUsing existing environment: test_env

# prepare_environment removes existing environment correctly [plain]

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      Removing existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      Removing existing environment: test_env
    Output
      NULL

# prepare_environment removes existing environment correctly [ansi]

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

# prepare_environment removes existing environment correctly [unicode]

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      Removing existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      Removing existing environment: test_env
    Output
      NULL

# prepare_environment removes existing environment correctly [fancy]

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

---

    Code
      prepare_environment(new_env = TRUE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      [1m[22mRemoving existing environment: test_env
    Output
      NULL

# prepare_environment uses existing environment correctly [plain]

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      Using existing environment: test_env

---

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      Using existing environment: test_env

# prepare_environment uses existing environment correctly [ansi]

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      [1m[22mUsing existing environment: test_env

---

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      [1m[22mUsing existing environment: test_env

# prepare_environment uses existing environment correctly [unicode]

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      Using existing environment: test_env

---

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      Using existing environment: test_env

# prepare_environment uses existing environment correctly [fancy]

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "conda")
    Message
      [1m[22mUsing existing environment: test_env

---

    Code
      prepare_environment(new_env = FALSE, envname = "test_env", python_version = "3.8",
        method = "virtualenv")
    Message
      [1m[22mUsing existing environment: test_env

# ensure_metaflow handles unsupported Python version correctly

    Code
      ensure_metaflow()
    Condition
      Error in `ensure_metaflow()`:
      ! The Python specified by `METAFLOW_PYTHON` has version
      3.7.0, which is unsupported.

# ensure_metaflow handles incompatible Metaflow version correctly [plain]

    Code
      ensure_metaflow()
    Condition
      Error in `ensure_metaflow()`:
      ! Metaflow version 1.9.0 is installed, but
      version >= 2.0 is required.

# ensure_metaflow handles incompatible Metaflow version correctly [ansi]

    Code
      ensure_metaflow()
    Condition
      [1m[33mError[39m in `ensure_metaflow()`:[22m
      [1m[22m[33m![39m Metaflow version 1.9.0 is installed, but
      version >= 2.0 is required.

# ensure_metaflow handles incompatible Metaflow version correctly [unicode]

    Code
      ensure_metaflow()
    Condition
      Error in `ensure_metaflow()`:
      ! Metaflow version 1.9.0 is installed, but
      version >= 2.0 is required.

# ensure_metaflow handles incompatible Metaflow version correctly [fancy]

    Code
      ensure_metaflow()
    Condition
      [1m[33mError[39m in `ensure_metaflow()`:[22m
      [1m[22m[33m![39m Metaflow version 1.9.0 is installed, but
      version >= 2.0 is required.

# ensure_metaflow handles invalid METAFLOW_PYTHON path correctly [plain]

    Code
      ensure_metaflow()
    Condition
      Error in `value[[3L]]()`:
      ! Python executable not found at path: ''/invalid/path/to/python''.

# ensure_metaflow handles invalid METAFLOW_PYTHON path correctly [ansi]

    Code
      ensure_metaflow()
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Python executable not found at path: '[34m/invalid/path/to/python[39m'.

# ensure_metaflow handles invalid METAFLOW_PYTHON path correctly [unicode]

    Code
      ensure_metaflow()
    Condition
      Error in `value[[3L]]()`:
      ! Python executable not found at path: ''/invalid/path/to/python''.

# ensure_metaflow handles invalid METAFLOW_PYTHON path correctly [fancy]

    Code
      ensure_metaflow()
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Python executable not found at path: '[34m/invalid/path/to/python[39m'.

# ensure_metaflow handles missing Metaflow module correctly [plain]

    Code
      ensure_metaflow()
    Condition
      Error in `ensure_metaflow()`:
      ! Metaflow is not installed in the Python environment
      specified by `METAFLOW_PYTHON`.

# ensure_metaflow handles missing Metaflow module correctly [ansi]

    Code
      ensure_metaflow()
    Condition
      [1m[33mError[39m in `ensure_metaflow()`:[22m
      [1m[22m[33m![39m Metaflow is not installed in the Python environment
      specified by `METAFLOW_PYTHON`.

# ensure_metaflow handles missing Metaflow module correctly [unicode]

    Code
      ensure_metaflow()
    Condition
      Error in `ensure_metaflow()`:
      ! Metaflow is not installed in the Python environment
      specified by `METAFLOW_PYTHON`.

# ensure_metaflow handles missing Metaflow module correctly [fancy]

    Code
      ensure_metaflow()
    Condition
      [1m[33mError[39m in `ensure_metaflow()`:[22m
      [1m[22m[33m![39m Metaflow is not installed in the Python environment
      specified by `METAFLOW_PYTHON`.

