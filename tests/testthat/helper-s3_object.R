# Create mock Python S3Object
mock_s3object <- reticulate::py_run_string("
class MockS3Object:
    def __init__(self):
        self.url = 's3://bucket/key'
        self.key = 'key'
        self.size = 1000
        self.path = '/local/path/to/file'
        self.blob = b'mock blob content'
        self.text = 'mock text content'
        self.metadata = {'key1': 'value1', 'key2': 'value2'}
        self.content_type = 'text/plain'
        self.downloaded = False
        self.exists = True
        self.last_modified = '2023-05-01 12:00:00'
        self.prefix = 'prefix'
        self._has_info = True  # Internal attribute for the read-only has_info property
        self.encryption = 'AES256'
        self.range_info = {'total_size': 1000, 'request_offset': 0, 'request_length': 1000}

    @property
    def has_info(self):
        \"\"\"Read-only property that reflects the state of _has_info\"\"\"
        return self._has_info

mock_obj = MockS3Object()
")

# Create a function to get a new S3Object instance for testing
get_mock_S3Object <- function() {
  S3Object$new(mock_s3object$mock_obj)
}

# Function to get Python True
get_py_true <- function() {
  reticulate::py_eval("True")
}

# Function to get Python False
get_py_false <- function() {
  reticulate::py_eval("False")
}

# Function to convert R logical to Python boolean
r_to_py_bool <- function(x) {
  reticulate::py_eval(ifelse(x, "True", "False"))
}
