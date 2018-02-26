#' Prints an example of a asanaR configuration file to the console.
#'
#' @param personal_access_token This is your personal access token from Asana
#'
#' @author Ryan Sullivan
#'
#' @export
#' @examples
#' \dontrun{
#' # Execute this line to get instructions on how to make a .asana.yml config file.
#' asanaRconfigFile()
#' }
#'

asanaRconfigFile <- function(personal_access_token = NULL) {
  msg <- paste0(
    "Copy-paste the lines between the dashes into a new plain text file, replace the value for the personal_access_token if it is not yet filled out. Save it in your working directory as '.asana.yml'.", "\n\n", # nolint
    "--------------", "\n",
    "personal_access_token: ", ifelse(is.null(personal_access_token), "<YOUR-PA-TOKEN-HERE>",
      paste0(personal_access_token)
    ), "\n",
    "--------------"
  )
  cat(msg)
}
