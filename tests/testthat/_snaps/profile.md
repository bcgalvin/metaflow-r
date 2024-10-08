# reads a valid json file and returns its content

    Code
      read_profile_json(test_config_path)
    Output
      $METAFLOW_BATCH_JOB_QUEUE
      [1] "test-queue"
      
      $METAFLOW_DATASTORE_SYSROOT_S3
      [1] "s3:://test/test"
      
      $METAFLOW_DATATOOLS_S3ROOT
      [1] "s3:://test/test/data"
      
      $METAFLOW_DEFAULT_DATASTORE
      [1] "s3"
      
      $METAFLOW_DEFAULT_METADATA
      [1] "service"
      
      $METAFLOW_ECS_S3_ACCESS_IAM_ROLE
      [1] "ecs-role"
      
      $METAFLOW_EVENTS_SFN_ACCESS_IAM_ROLE
      [1] "events-role"
      
      $METAFLOW_SERVICE_AUTH_KEY
      [1] "testkey"
      
      $METAFLOW_SERVICE_INTERNAL_URL
      [1] "https://test"
      
      $METAFLOW_SERVICE_URL
      [1] "https://test"
      
      $METAFLOW_SFN_DYNAMO_DB_TABLE
      [1] "dynamo-table"
      
      $METAFLOW_SFN_IAM_ROLE
      [1] "sfn-role"
      

# throws an error if the file does not exist

    Code
      read_profile_json("nonexistent.json")
    Condition
      Error in `read_profile_json()`:
      ! Assertion on 'path' failed: File does not exist: 'nonexistent.json'.

# get_metaflow_home handles various scenarios correctly

    Code
      get_metaflow_home()
    Output
      [1] "/custom/metaflow/home"

---

    Code
      withr::with_options(list(warn = 1), get_metaflow_home())
    Condition
      Warning:
      ! METAFLOW_HOME environment variable is set to /nonexistent/metaflow/home, but directory does not exist.
      Warning:
      ! Default metaflow home location /mock/home does not exist on this system or
      does not have valid config files matching the glob pattern `*config*.json`
    Output
      NULL

---

    Code
      get_metaflow_home()
    Condition
      Warning:
      ! Default metaflow home location /mock/home does not exist on this system or
      does not have valid config files matching the glob pattern `*config*.json`
    Output
      NULL

---

    Code
      withr::with_options(list(warn = 1), get_metaflow_home())
    Condition
      Warning:
      ! Default metaflow home location /mock/home does not exist on this system or
      does not have valid config files matching the glob pattern `*config*.json`
    Output
      NULL

# list_profiles errors when no profiles are found

    Code
      list_profiles()
    Condition
      Error in `list_profiles()`:
      ! No profiles found in '/var/folders/lw/6x90887x30l10v8tslljk_cw0000gn/T//<temp_dir>/<temp_file>'.
      i Ensure that the directory exists and contains configuration json files.

# update_profile handles name and path correctly

    Code
      update_profile(name = "test")

---

    Code
      update_profile(path = test_profile_path)

---

    Code
      update_profile()
    Condition
      Error in `update_profile()`:
      ! Please provide either `name` or `path`.

---

    Code
      update_profile(name = "test", path = test_profile_path)
    Condition
      Error in `update_profile()`:
      ! Please provide either `name` or `path`, not both.

