# install_metaflow handles different methods

    Code
      install_metaflow(method = "auto", version = "2.2.5")
    Message
      [32mv[39m [34m[34mMetaflow[34m[39m installation complete.

---

    Code
      install_metaflow(method = "virtualenv", version = "2.2.5")
    Message
      [32mv[39m [34m[34mMetaflow[34m[39m installation complete.

---

    Code
      install_metaflow(method = "conda", version = "2.2.5")
    Message
      [32mv[39m [34m[34mMetaflow[34m[39m installation complete.

---

    'arg' should be one of "virtualenv", "conda", "auto"

---

    Assertion on 'version' failed: Must be TRUE.

# install_metaflow emits success message upon completion

    Code
      install_metaflow()
    Message
      [32mv[39m [34m[34mMetaflow[34m[39m installation complete.

