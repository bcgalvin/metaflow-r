skip_if_no_metaflow <- function(required_version = NULL) {
  if (!reticulate::py_module_available("metaflow")) {
    skip("Metaflow not available for testing")
  }
}
