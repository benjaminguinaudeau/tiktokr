#' get_username
#' @description Function to get information on a given username
#' @export
get_username <- function(username, port = NULL){
  url <- get_url("username", username = username)
  # br <- py$browser(url)
  res <- get_data(url, port = port)

  if(!"userInfo" %in% names(res)){return(tibble::tibble())}

  out <- res$userInfo$user %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_hashtag
#' @description Function to get information on a given hashtag
#' @export
get_hashtag <- function(hashtag, port = NULL){
  url <- get_url("hashtag", hashtag = hashtag)
  # br <- py$browser(url)
  res <- get_data(url, port = port)
  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_music
#' @description Function to get information on a given piece of music
#' @export
get_music <- function(music_id, port = NULL){
  url <- get_url("music", music_id = music_id)
  # br <- py$browser(url)
  res <- get_data(url, port = port)
  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

