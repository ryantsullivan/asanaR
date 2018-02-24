# Asana API Client
# Copyright (c) 2018 Ryan Sullivan
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#     
#     The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Register Asana Personal Access Token
#'
#' This function registers the user's Asana Personal Access Token.
#' @param verbose Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
#' @param ... A 'personal_access_token' to register with Asana Connect. (see example). See also \code{\link{asanaRconfigFile}} for an explanation of the personal_access_token parameters.
#'
#' @seealso  See \url{https://asana.com/developers/documentation/getting-started/auth#personal-access-token}
#'
#' @author  Ryan Sullivan
#' @importFrom yaml yaml.load_file
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples \dontrun{
#' #Register your Asana Personal Access Token if you haven't already
#' #Note that you need to pass the 'personal_access_token'
#' #parameter if you call this function for the first time.
#' registerOptions(personal_access_token="<YOUR-PA-TOKEN>" )
#' } 
registerOptions <- function(verbose = TRUE,
                            ...) {
  args <- list(...)
  personal_access_token <- ifelse("personal_access_token" %in% names(args), args$personal_access_token, NA)
  # Set Personal Access Token if not NA
  if (!is.na(personal_access_token)) Sys.setenv("ASANA_PERSONAL_ACCESS_TOKEN" = personal_access_token)
  # If Personal Access Token is NA, look for .asana.yml in the current directory
  if (is.na(personal_access_token)) {
    ex <- file.exists(".asana.yml")
    # Throw Error if files does not exist
    assertthat::assert_that(ex == TRUE, msg = "No .asana.yml file. Either set your Personal Access Token using the `registerOptions` function or create a configuration file. Execute 'asanaRconfigFile()' to view an example of a configuration file.")
    # Load File
    pac <- yaml::yaml.load_file(".asana.yml")
    # Assert that name is personal_access_token
    assertthat::assert_that("personal_access_token" %in% names(pac), msg = "The 'personal_access_token' argument is missing in your .asana.yml configuration file. Execute 'asanaRconfigFile()' to view an example of the configuration file. Execute 'file.edit('.asana.yml')' to edit your configuration file.")
    if (verbose) message(paste0("Found a .asana.yml configuration file in ", getwd(), ". Using this Personal Access Token."))
    # Set Vars
    personal_access_token <- pac$personal_access_token
  }
  # Check Args
  # If personal_access_token is still NA and environment is empty, throw error
  assertthat::assert_that(!is.na(personal_access_token) | Sys.getenv("ASANA_PERSONAL_ACCESS_TOKEN") != "", msg = "'personal_access_token' parameter must either be specified in the .asana.yml configuration file or passed to the 'registerOptions' function. To view an example of a configuration file, execute 'asanaRconfigFile()'.")
  # Set Environment Variable
  if (!is.na(personal_access_token)) Sys.setenv("ASANA_PERSONAL_ACCESS_TOKEN" = personal_access_token)
}
