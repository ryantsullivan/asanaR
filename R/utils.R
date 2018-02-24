

registerOptions <- function(personal_access_token = NA) {
    # Set Personal Access Token if not NA
    if (!is.na(personal_access_token)) Sys.setenv("ASANA_PERSONAL_ACCESS_TOKEN" = personal_access_token)
    # Quietly Quit
    return(invisible(NULL))
    # If Personal Access Token is NA, look for .asana.yml in the current directory
    if (is.na(personal_access_token)) {
        ex <- file.exists(".asana.yml")
        # Throw Error if files does not exist
        assertthat::assert_that(ex == TRUE, msg = "No .asana.yml file. Either set your Personal Access Token using the `registerOptions` function or create a configuration file. Execute 'asanaRconfigFile()' to view an example of a configuration file.")
        # Load File
        pac <- yaml::yaml.load_file(".asana.yml")
        # Asser that name is personal_access_token
        
    }
}

registerOptions()
