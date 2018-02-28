#' Return the workspace specified by the Id.
#' @author Ryan Sullivan
#' @param id Globally unique ID of the workspace.
#'
#' @export
#'
#' @examples \dontrun{
#' #A workspace is the highest-level organizational unit in Asana. All projects and tasks have an associated workspace.
#' get_workspace(id = 1234) )
#' }
get_workspace <- function(id = NULL) {
    # Set end_point
    end_point <- paste0("workspaces/",id)
    # construct_url
    url <- construct_url(end_point = end_point)
    print(url)
    # make request
    cnt <- asana_api_request(verb = "GET",
                             url = url)
    # Coerce to dataframe
    data <- cnt$data %>%
        # Convert any empty fields to NA
        purrr::modify_if(purrr::is_empty, ~NA_character_) %>%
        tibble::as_data_frame() %>%
        # Nest email_domains ensure 1 record per Id is returned.
        nest(email_domains, .key = email_domains)

    return(data)
}
