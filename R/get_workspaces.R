#' Get workspaces from your Asana account
#'
#' @param limit Test
#' @param all_pages Test
#'
#' @export
#'
#' @examples \dontrun{
#' #Use this function 
#' #A workspace is the highest-level organizational unit in Asana. All projects and tasks have an associated workspace.
#'  
#' get_workspaces(limit = 10, all_pages = TRUE )
#' }

get_workspaces <- function(limit = 10, all_pages = TRUE) {
    # Set end_point
    end_point <- "workspaces"
    # construct_url
    url <- construct_url(end_point = end_point, limit = limit)
    # make request
    cnt <- asana_api_request(verb = "GET",
                      url = url)
    # Coerce to dataframe
    data <- cnt$data %>% as_data_frame()
    
    # Capture all pages
    if (all_pages) {
        # Set the initial uri provided by initial call
        next_page <- cnt$next_page
        # Continue while next_page is not null
        while (!is.null(next_page)) {
            nxt_cnt <- asana_api_request(verb = "GET",
                                         url = cnt$next_page)
            # Coerce next_page to dataframe
            nxt_data <- nxt_cnt$data %>% as_data_frame()
            # bind_rows together
            data <- bind_rows(data, nxt_data)
            # set next_page
            next_page <- nxt_cnt$next_page
            return(data)
        }
    }
    return(data)
}

get_workspaces()
