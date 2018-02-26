context("test-asanaRconfigFile.R")

test_that("asanaRconfigFile() cats a message to console", {
    # Call getsurvey
    expect_output(asanaR::asanaRconfigFile(),
                  "Copy-paste the lines between the dashes into a new plain text file, replace the value for the personal_access_token")
})