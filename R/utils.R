# utils.R contains helper functions for the asanaR package. These functions should not be called directly by the user and should not be exported.

#' Construct a header to send to qualtrics API
#'
#' @param personal_access_token Personal Access Token. Available in your Asana account.
#'
construct_header <- function(personal_access_token) {
  # Construct and return
  headers <- c(
    "Authorization" = paste0("Bearer ", personal_access_token),
    "Content-Type" = "application/json",
    "Accept" = "*/*",
    "accept-encoding" = "gzip, deflate"
  )
  return(headers)
}


#' Send httr requests to Asana API
#'
#' @param verb verb type of request to be sent (@seealso ?httr::VERB)
#' @param url url Asana endpoint url created by and passed to this function
#' @param body body options created by and passed to this function
#'
#' @author Ryan Sullivan
asana_api_request <- function(verb = c("GET", "POST", "PUT", "DELETE"),
                              url = url, body = NULL) {
  # Match arg
  verb <- match.arg(verb)
  # Construct header
  headers <- construct_header(Sys.getenv("ASANA_PERSONAL_ACCESS_TOKEN"))
  # Send request to Asana API
  resp <- httr::VERB(
    verb,
    url = url,
    httr::add_headers(
      headers
    ),
    body = body
  )
  # Check if response type is OK
  if (httr::http_type(resp) != "application/json") {
      stop('Asana API did not return JSON', call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
  
  if (httr::http_error(resp)) {
      asana_errors(resp, parsed)
  }
  
  return(
      list(
          data = parsed$data,
          next_page = parsed$next_page$uri
      )
  )
}




#' Handle Asana API Response Codes
#'
#' @param resp A Response object from the Asana API
#' @param parsed The Parsed Response
#'
#' @author Ryan Sullivan
asana_errors <- function(resp, parsed) {
    stop(
        sprintf(
            paste0("Asana API Request failed ",
                   httr::status_code(resp),".\n",
                   parsed$errors$message,"\n",
                   parsed$errors$help,"\n",
                   parsed$errors$phrase)
        ),
        call. = FALSE
    )
}

#' Construct Asana API Url 
#' @param end_point The Resource that the user is trying to reach
#' @param limit A value between 1 & 100.
#' 
#' @author Ryan Sullivan
construct_url <- function(end_point = NULL,  limit = 10) {
    root_url <- "https://app.asana.com/"
    path <- paste0("api/1.0/", end_point)
    query = list(limit = limit)
    url <- httr::modify_url(root_url, path = path, query = query)
    return(url)
}

