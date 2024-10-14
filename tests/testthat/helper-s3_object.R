# Create mock Python S3Object
mock_s3object <- reticulate::py_run_string("
class MockS3Object:
    def __init__(self):
        self.url = 's3://bucket/key'
        self.prefix = 'prefix'
        self.key = 'key'
        self.exists = True
        self.size = 1000
        self.downloaded = False
        self.content_type = 'text/plain'
        self.last_modified = '2023-05-01 12:00:00'
        self._has_info = True  # Internal attribute for the read-only has_info property
        self.encryption = 'AES256'
        self.range_info = {'total_size': 1000, 'request_offset': 0, 'request_length': 1000}

    @property
    def has_info(self):
        \"\"\"Read-only property that reflects the state of _has_info\"\"\"
        return self._has_info

mock_obj = MockS3Object()
")

# Create a function to get a new s3_object instance for testing
get_mock_s3_object <- function() {
  s3_object$new(mock_s3object$mock_obj)
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
