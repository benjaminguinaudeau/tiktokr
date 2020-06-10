#' get_username
#' @description Function to get information on a given username
#' @export
get_username <- function(username, ua = ua_default, port = NULL){
  url <- get_url("username", username = username)

  res <- get_data(url, port = port, ua = ua)

  if(!"userInfo" %in% names(res)){return(tibble::tibble())}

  out <- res$userInfo$user %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_hashtag
#' @description Function to get information on a given hashtag
#' @export
get_hashtag <- function(hashtag, port = NULL, ua = ua_default){
  url <- get_url("hashtag", hashtag = hashtag)

  res <- get_data(url, port = port, ua = ua)

  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_music
#' @description Function to get information on a given piece of music
#' @export
get_music <- function(music_id, port = NULL, ua = ua_default){
  url <- get_url("music", music_id = music_id)

  res <- get_data(url, port = port, ua = ua)

  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

