# Test whether API key is stored in environment
assert_pac_stored <- function() { # nolint start
  # Key should be stored by "registerApiKey"
  assertthat::assert_that(
    Sys.getenv("ASANA_PERSONAL_ACCESS_TOKEN") != "",
    msg = "You need to register your Asana Personal Access Token using the 'registerOptions()' function."
  )
}
