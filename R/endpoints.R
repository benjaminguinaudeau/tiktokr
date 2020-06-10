#' get_trending
#' @description Function to get trending tiktoks
#' @param n A numeric scalar indicating the number of tiktoks to scrape
#' @export
get_trending <- function(n = 10000, ua = default_ua) get_n("trending", n = n, ua = ua)

#' get_user_post
#' @description Function to get tiktoks from a given username
#' @param username Character indicating the username to scrape
#' @param n Numeric indicating the number of tiktoks to scrape
#' @export
<<<<<<< HEAD
<<<<<<< HEAD
get_user_post <- function(username, n = 10000, save_dir = NULL, port = NULL, ua = default_ua, ...){
  user <- get_username(username, port = port, ua = ua)

  if(length(user) == 0){message(glue::glue("{username} was not found")) ; return(tibble::tibble())}

  get_n(type = "user_post", n = n, user_id = user$id, sec_uid = user$secUid, query = username, save_dir = save_dir, port = port, ua = ua, ...)
=======
get_user_post <- function(count, username, ...){
  user <- get_username(username)
  get_count(type = "user_post", count = count, user_id = user$id, sec_uid = user$secUid, query = username, ...)
>>>>>>> 16eb9ca88d97851895eba630e32e62f679ed8cf8
=======
get_user_post <- function(count, username, save = F, path = NULL, port = NULL, ...){
  user <- get_username(username, port = port)

  if(length(user) == 0){message(glue::glue("{username} was not found")) ; return(tibble::tibble())}

  get_count(type = "user_post", count = count, user_id = user$id, sec_uid = user$secUid, query = username, save = save, path = path, port = port, ...)
>>>>>>> 5aea75f85e35f9e94fcecf3b8ebdf3d43888f62d
}

#' get_music_post
#' @description Function to get tiktoks from a given music piece
#' @param music_id Character indicating the \code{music_id} to scrape
#' @param n Numeric indicating the number of tiktoks to scrape
#' @export
<<<<<<< HEAD
<<<<<<< HEAD
get_music_post <- function(music_id, n = 10000, port = NULL, ua = default_ua,  ...){
  # user <- get_username(username)
  get_n("music_post", n = n, music_id = music_id, query = music_id, port = port, ua = ua, ...)
=======
get_music_post <- function(count, music_id, ...){
  # user <- get_username(username)
  get_count("music_post", count = count, music_id = music_id, query = music_id, ...)
>>>>>>> 16eb9ca88d97851895eba630e32e62f679ed8cf8
=======
get_music_post <- function(count, music_id, port = NULL, ...){
  # user <- get_username(username)
  get_count("music_post", count = count, music_id = music_id, query = music_id, port = port,...)
>>>>>>> 5aea75f85e35f9e94fcecf3b8ebdf3d43888f62d
}

#' get_hashtag_post
#' @description Function to get tiktoks from a given hashtag
#' @param hashtag Hashtag to scrape
#' @param n Number of tiktoks to retrieve
#' @export
<<<<<<< HEAD
<<<<<<< HEAD
get_hashtag_post <- function(hashtag, n = 10000, cursor = 0, save_dir = NULL,  port = NULL, ua = default_ua){
  hash <- get_hashtag(hashtag, port = port, ua = ua)

  get_n(type = "hashtag_post", n = n, cursor = cursor, hash_id = hash$challengeInfo.challenge.id, query = hashtag,
        save_dir = save_dir, port = port, ua = ua )
=======
get_hashtag_post <- function(count, hashtag, ...){
  hash <- get_hashtag(hashtag)
  get_count("hashtag_post", count = count, hash_id = hash$challengeInfo.challenge.id, query = hashtag, ...)
>>>>>>> 16eb9ca88d97851895eba630e32e62f679ed8cf8
=======
get_hashtag_post <- function(count, hashtag, cursor = 0, save = F, path = NULL,  port = NULL, ...){
  hash <- get_hashtag(hashtag, port = port)
  get_count(type = "hashtag_post", count = count, cursor = cursor, hash_id = hash$challengeInfo.challenge.id, query = hashtag,
            save = save, path = path, port = port, ...)
>>>>>>> 5aea75f85e35f9e94fcecf3b8ebdf3d43888f62d
}

#' discover_hashtags
#' @description Function to get 20 suggested hashtags
#' @export
discover_hashtags <- function(ua = default_ua) get_n("discover_hash", ua = ua)

#' discover_music
#' @description Function to get 20 suggested musics
#' @export
discover_music <- function(ua = default_ua) get_n("discover_music", ua = ua)



