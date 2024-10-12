standard_mock_data <- list(
  scenario1 = c("folder1/file1", "folder1/file2", "folder1/subfolder/file3", "folder2/file4", "file5"),
  scenario2 = c("path1/file1", "path1/file2", "path2/file3")
)

create_s3_mock <- function(list_paths_fn = NULL, list_recursive_fn = NULL, ...) {
  structure(list(
    list_paths = list_paths_fn %||% function(...) stop("list_paths not implemented"),
    list_recursive = list_recursive_fn %||% function(...) stop("list_recursive not implemented"),
    ...
  ), class = "mock_s3_client")
}

mock_list_paths <- function(scenario = "scenario1") {
  keys <- standard_mock_data[[scenario]]
  function(keys_input = NULL) {
    if (is.null(keys_input) || length(keys_input) == 0) {
      prefixes <- unique(sub("/.*", "", keys))
      return(lapply(prefixes, function(key) {
        mock_py_s3object(key = key, url = paste0("s3://bucket/", key), exists = FALSE)
      }))
    } else if (length(keys_input) == 1) {
      prefix <- keys_input
      matching_keys <- keys[startsWith(keys, prefix)]
      if (length(matching_keys) == 0) {
        return(list())
      }

      immediate_children <- unique(sub(paste0("^", prefix, "/([^/]+).*"), "\\1", matching_keys))
      immediate_children <- immediate_children[immediate_children != ""]

      return(lapply(immediate_children, function(child) {
        full_key <- if (prefix == "") child else paste0(prefix, "/", child)
        is_file <- any(matching_keys == full_key)
        mock_py_s3object(key = full_key, url = paste0("s3://bucket/", full_key), exists = is_file)
      }))
    } else {
      return(lapply(keys_input, function(key) {
        matching_key <- keys[startsWith(keys, key)]
        if (length(matching_key) == 0L) {
          return(NULL)
        }
        mock_py_s3object(key = matching_key[1], url = paste0("s3://bucket/", matching_key[1]))
      }))
    }
  }
}

mock_list_recursive <- function(scenario = "scenario1") {
  keys <- standard_mock_data[[scenario]]
  function(keys_input = NULL) {
    matching_keys <- if (is.null(keys_input)) keys else keys[startsWith(keys, keys_input)]
    lapply(matching_keys, function(key) {
      mock_py_s3object(
        key = key,
        url = paste0("s3://bucket/", key),
        size = if (grepl("file1", key)) 100 else if (grepl("file2", key)) 150 else 200
      )
    })
  }
}

mock_py_s3object <- function(
    url = "s3://bucket/key",
    prefix = "prefix",
    key = "key",
    size = 100,
    exists = TRUE,
    has_info = TRUE,
    metadata = list(key = "value"),
    content_type = "text/plain",
    last_modified = as.numeric(Sys.time()),
    encryption = "AES256",
    range_info = NULL) {
  reticulate::py_run_string("class MockS3Object:
    def __init__(self, exists, url, prefix, key, size, has_info, metadata, content_type, last_modified, encryption, range_info):
      self.exists = exists
      self.url = url
      self.prefix = prefix
      self.key = key
      self.size = size
      self.has_info = has_info
      self.metadata = metadata
      self.content_type = content_type
      self.last_modified = last_modified
      self.encryption = encryption
      self.range_info = range_info
  ")

  if (!is.null(range_info)) {
    range_info <- py_range_info(
      total_size = range_info$total_size,
      request_offset = range_info$request_offset,
      request_length = range_info$request_length
    )
  }

  py_obj <- reticulate::py$MockS3Object(
    exists = exists,
    url = url,
    prefix = prefix,
    key = key,
    size = size,
    has_info = has_info,
    metadata = reticulate::dict(metadata),
    content_type = content_type,
    last_modified = last_modified,
    encryption = encryption,
    range_info = range_info
  )

  return(py_obj)
}

mock_mf <- function(list_paths_fn = NULL, list_recursive_fn = NULL, fail = FALSE, ...) {
  S3_function <- function(tmproot, bucket, prefix, run, s3root, external_client) {
    if (fail) {
      stop("Simulated connection error")
    } else {
      create_s3_mock(
        list_paths_fn = list_paths_fn,
        list_recursive_fn = list_recursive_fn
      )
    }
  }

  structure(list(
    S3 = S3_function
  ), class = "mock_mf")
}
py_range_info <- function(total_size, request_offset, request_length) {
  reticulate::py_run_string("
class MockRangeInfo:
    def __init__(self, total_size, request_offset, request_length):
        self.total_size = total_size
        self.request_offset = request_offset
        self.request_length = request_length
")
  reticulate::py$MockRangeInfo(
    total_size = as.integer(total_size),
    request_offset = as.integer(request_offset),
    request_length = as.integer(request_length)
  )
}
