transform_temp_paths <- function(x) {
  x <- gsub("\\\\", "/", x)
  x <- gsub("/var/folders/[^/]+/[^/]+/[^/]+/[^/]+/", "<temp_dir>/", x)
  x <- gsub("/tmp/Rtmp[A-Za-z0-9]+/", "<temp_dir>/", x)
  x <- gsub("file[a-f0-9]+", "<temp_file>", x)
  x <- gsub("/private/var/", "/var/", x)
  x <- gsub("Rtmp[A-Za-z0-9]+", "<temp_dir>", x)
  x <- gsub("/Users/[^/]+/", "<home_dir>/", x)
  x <- gsub("tmproot: .+$", "tmproot: <temp_dir>", x)
  x <- gsub("tmproot: mocked_tmproot", "tmproot: <mocked_tmproot>", x)
  x <- gsub("bucket: mocked_bucket", "bucket: <mocked_bucket>", x)
  x <- gsub("prefix: mocked_prefix", "prefix: <mocked_prefix>", x)
  x
}

mock_mf <- function(fail = FALSE) {
  if (fail) {
    list(
      S3 = function(...) {
        cli::cli_abort(c(
          "Error in connect()",
          "i" = "Simulated connection error"
        ))
      }
    )
  } else {
    s3_mock <- reticulate::py_eval("type('S3Mock', (object,), {
      '__init__': lambda self, **kwargs: None,
      'list_paths': lambda self, keys=None: [
        type('MockS3Object', (object,), {
          'key': key,
          'exists': True,
          'url': 's3://bucket/' + key,
          'prefix': 'prefix',
          'size': 100 if key == 'path1' else 200,
          'has_info': True,
          'metadata': {'key': 'value'},
          'content_type': 'text/plain',
          'last_modified': 1234567890,
          'encryption': 'AES256',
          'range_info': None
        })() for key in (keys if isinstance(keys, list) else ([keys] if keys is not None else ['path1', 'path2']))
      ],
      'list_recursive': lambda self, keys=None: [
        type('MockS3Object', (object,), {
          'key': key,
          'exists': True,
          'url': 's3://bucket/' + key,
          'prefix': 'prefix',
          'size': 100 if 'file1' in key else (150 if 'file2' in key else 200),
          'has_info': True,
          'metadata': {'key': 'value'},
          'content_type': 'text/plain',
          'last_modified': 1234567890,
          'encryption': 'AES256',
          'range_info': None
        })() for key in (keys if isinstance(keys, list) else ([keys] if keys is not None else ['path1/file1', 'path1/file2', 'path2/file3']))
      ]
    })()")

    list(
      S3 = function(...) s3_mock,
      S3Client = function(...) {
        s3_client_mock
      },
      get_s3_client = function(...) {
        list(client = s3_client_mock, error = NULL)
      }
    )
  }
}

mock_py_s3object <- function(
    exists = TRUE,
    url = "s3://bucket/key",
    prefix = "prefix",
    key = "key",
    size = 100,
    has_info = TRUE,
    metadata = list(key = "value"),
    content_type = "text/plain",
    last_modified = as.numeric(Sys.time()),
    encryption = "AES256",
    range_info = NULL) {
  if (!is.null(range_info)) {
    range_info <- reticulate::py_eval(sprintf(
      "type('RangeInfo', (), {'total_size': %d, 'request_offset': %d, 'request_length': %d})()",
      range_info$total_size,
      range_info$request_offset,
      range_info$request_length
    ))
  }

  reticulate::py_run_string("
class MockS3Object:
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

  py_obj
}
