
#' tk_info
#' @description Function to get information on a given user/hashtag/piece of music
#' @param scope Character indicating the endpoint to scrape (must be "hashtag", "user"
#' @param query Character indicating the username/hashtag/music_id to scrape
#'   or "music")
#' @export
tk_info <- function(scope, query, ua = default_ua){

  res <- switch(
    scope,
    "user" = {
      url <- get_url("username", username = query)
      get_data(url, ua = ua)$userInfo$user
    },
    "hashtag" = {
      url <- get_url("hashtag", hashtag = query)
      get_data(url, ua = ua)
    },
    "music" = {
      url <- get_url("music", music_id = query)
      get_data(url, ua = ua)
    }
  )

  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}
