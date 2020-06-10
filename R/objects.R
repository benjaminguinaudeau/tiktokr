#' get_username
#' @description Function to get information on a given username
#' @export
<<<<<<< HEAD
<<<<<<< HEAD
get_username <- function(username, ua = ua_default, port = NULL){
  url <- get_url("username", username = username)

  res <- get_data(url, port = port, ua = ua)

  if(!"userInfo" %in% names(res)){return(tibble::tibble())}

=======
get_username <- function(username){
  url <- get_url("username", username = username)
  # br <- py$browser(url)
  res <- get_data(url)
>>>>>>> 16eb9ca88d97851895eba630e32e62f679ed8cf8
=======
get_username <- function(username, port = NULL){
  url <- get_url("username", username = username)
  # br <- py$browser(url)
  res <- get_data(url, port = port)

  if(!"userInfo" %in% names(res)){return(tibble::tibble())}

>>>>>>> 5aea75f85e35f9e94fcecf3b8ebdf3d43888f62d
  out <- res$userInfo$user %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_hashtag
#' @description Function to get information on a given hashtag
#' @export
<<<<<<< HEAD
<<<<<<< HEAD
get_hashtag <- function(hashtag, port = NULL, ua = ua_default){
  url <- get_url("hashtag", hashtag = hashtag)

  res <- get_data(url, port = port, ua = ua)

=======
get_hashtag <- function(hashtag){
  url <- get_url("hashtag", hashtag = hashtag)
  # br <- py$browser(url)
  res <- get_data(url)
>>>>>>> 16eb9ca88d97851895eba630e32e62f679ed8cf8
=======
get_hashtag <- function(hashtag, port = NULL){
  url <- get_url("hashtag", hashtag = hashtag)
  # br <- py$browser(url)
  res <- get_data(url, port = port)
>>>>>>> 5aea75f85e35f9e94fcecf3b8ebdf3d43888f62d
  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

#' get_music
#' @description Function to get information on a given piece of music
#' @export
<<<<<<< HEAD
<<<<<<< HEAD
get_music <- function(music_id, port = NULL, ua = ua_default){
  url <- get_url("music", music_id = music_id)

  res <- get_data(url, port = port, ua = ua)

=======
get_music <- function(music_id){
  url <- get_url("music", music_id = music_id)
  # br <- py$browser(url)
  res <- get_data(url)
>>>>>>> 16eb9ca88d97851895eba630e32e62f679ed8cf8
=======
get_music <- function(music_id, port = NULL){
  url <- get_url("music", music_id = music_id)
  # br <- py$browser(url)
  res <- get_data(url, port = port)
>>>>>>> 5aea75f85e35f9e94fcecf3b8ebdf3d43888f62d
  out <- res %>%
    rlist::list.flatten() %>%
    purrr::imap_dfc(~purrr::set_names(tibble::tibble(.x), .y))

  return(out)
}

