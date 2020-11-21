
#' tk_info
#' @description Function to get information on a given user/hashtag/piece of music
#' @param scope Character indicating the endpoint to scrape (must be "hashtag", "user"
#'   or "music")
#' @param query Character indicating the username/hashtag/music_id to scrape
#' @export
tk_info <- function(scope, query, ...){

   res <- switch(
    scope,
    "user" = {
      url <- get_url("username", query_1 = query)
      get_data(url, ...)$userInfo
    },
    "hashtag" = {
      url <- get_url("hashtag", query_1 = query)
      get_data(url, ...)
    },
    "music" = {
      tk_posts(scope = "music", query = query, n = 1, ...)
    },
    "post" = {
      url <- get_url("post", query_1 = query)
      get_data(url, ...)
    }
  )

  if(is.null(res)){
    return(tibble::tibble())
  }

  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~{
      if(length(.x) == 1){
        return(tibble::tibble(.x) %>% purrr::set_names(.y))
      } else {
        return(tibble::tibble(list(.x)) %>% purrr::set_names(.y))
      }
    })

  return(out)
}
