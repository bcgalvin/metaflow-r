# reads a valid json file and returns its content

    Code
      result
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

# get_metaflow_home returns correct directory

    Code
      get_metaflow_home()
    Condition
      Warning:
      ! METAFLOW_HOME environment variable is set to /custom/metaflow/home, but directory does not exist.
    Output
      [1] "/Users/bryangalvin/.metaflowconfig"

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
      get_metaflow_home()
    Condition
      Warning:
      ! Default metaflow home location /nonexistent/home does not exist on this system or
      does not have valid config files matching the glob pattern `*config*.json`
    Output
      NULL

# list_profiles returns a tibble with correct profiles

    Code
      result
    Output
      # A tibble: 3 x 2
        profile_name path                
        <chr>        <chr>               
      1 default      config.json         
      2 personal     config_personal.json
      3 test         config_test.json    

# list_profiles errors when no profiles are found

    Code
      list_profiles()
    Condition
      Error in `list_profiles()`:
      ! No profiles found in '<temp_dir>'.
      i Ensure that the directory exists and contains configuration json files.

