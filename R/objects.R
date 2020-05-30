#' get_username
#' @description Function to get information on a given username
#' @export
get_username <- function(username){
  url <- get_url("username", username = username)
  br <- py$browser(url)
  res <- get_data(url, br$signature, br$userAgent)
  out <- res$userInfo$user %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_hashtag
#' @description Function to get information on a given hashtag
#' @export
get_hashtag <- function(hashtag){
  url <- get_url("hashtag", hashtag = hashtag)
  br <- py$browser(url)
  res <- get_data(url, br$signature, br$userAgent)
  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_music
#' @description Function to get information on a given piece of music
#' @export
get_hashtag <- function(music_id){
  url <- get_url("music", music_id = music_id)
  br <- py$browser(url)
  res <- get_data(url, br$signature, br$userAgent)
  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

