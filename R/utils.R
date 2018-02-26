#' Checks responses against Asana response codes and returns an error message.
#'
#' @param res response from httr::GET 
#' @param raw raw if TRUE, add 'raw' flag to httr::content() function.
#'
#' @author Ryan Sullivan
 
 
asanaResponseCodes <- function(res, raw=FALSE) {
    # Check status code and raise error/warning
    if(res$status_code == 200 | 201) {
        if(raw) {
            result <- httr::content(res, "raw")
        } else {
            result <- httr::content(res)
        }
        return(list(
            "content" = result,
            "OK" = TRUE
        )
        )
    } else if(res$status_code == 401) {
        stop("Asana API raised an authentication (401) error - you may not have the required authorization. Please check your Personal Access Token.") # nolint
    } else if(res$status_code == 400) {
        stop("Asana API raised a bad request (400) error - Please report this on https://github.com/ryantsullivan/asanaR/issues") # nolint
    } else if(res$status_code == 404) {
        stop("Asana API complains that the requested resource cannot be found (404 error). Either the request method and path supplied do not specify a known action in the API, or the object specified by the request does not exist.") # nolint
    } else if(res$status_code == 403) {
        stop("Asana API raised a forbidden (403) error. The authentication and request syntax was valid but the server is refusing to complete the request. This can happen if you try to read or write to objects or properties that the user does not have access to.") # nolint
    } else if(res$status_code == 434) {
        stop("Asana API raised a deprecated (434) error. An aspect of the request has been deprecated. See the response message for more details.") # nolint
    }else if(res$status_code == 429) {
        stop("Asana API raised a Rate Limit Enforced (429) error.") # nolint
    }else if(res$status_code == 500) {
        stop("Asana API reports an internal server (500) error.") # nolint
    }
}

#' Consstruct a header to send to the Asana API
#'
#' @param personal_access_token Personal Access Token. Available in your Asana account
#'
#'
#' @author Ryan Sullivan
 
 
construct_header <- function(personal_access_token) {
    # Construct and return
    headers <- c(
        'Authorization' = paste0("Bearer ", personal_access_token),
        'Content-Type' = "application/json",
        'Accept' = '*/*',
        'accept-encoding' = 'gzip, deflate'
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
    res <- httr::VERB(verb,
                      url = url,
                      httr::add_headers(
                          headers
                      ),
                      body = body)
    # Check if response type is OK
    cnt <- asanaResponseCodes(res)
    # Check if OK
    if (cnt$OK) {
        #return content
        return(cnt$content)
    }
}