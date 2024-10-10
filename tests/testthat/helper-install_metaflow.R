# Helper function to manage .globals[["mf"]] within tests
local_mf <- function(value = NULL, envir = parent.frame()) {
  old_mf <- .globals[["mf"]]
  .globals[["mf"]] <- value
  withr::defer(
    {
      .globals[["mf"]] <- old_mf
    },
    envir = envir
  )
}
