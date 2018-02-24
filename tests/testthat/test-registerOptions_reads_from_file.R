test_that("registerOptions() can read from file", {
    # Store .qualtRics.yml in temporary directory
    io <- yaml::as.yaml(list(
        "personal_access_token" = "asana1234"
    ))
    # Set environment variables to ""
    Sys.setenv("ASANA_PERSONAL_ACCESS_TOKEN" = "")
    # Save WD and set wd
    curr.wd <- getwd()
    setwd(tempdir())
    on.exit(setwd(curr.wd))
    # Write yml file
    write(io, ".asana.yml")
    expect_message(asanaR::registerOptions(),
                   "Found a .asana.yml configuration file")
})