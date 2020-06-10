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
get_user_post <- function(username, n = 10000, save_dir = NULL, port = NULL, ua = default_ua, ...){
  user <- get_username(username, port = port, ua = ua)

  if(length(user) == 0){message(glue::glue("{username} was not found")) ; return(tibble::tibble())}


}

#' get_music_post
#' @description Function to get tiktoks from a given music piece
#' @param music_id Character indicating the \code{music_id} to scrape
#' @param n Numeric indicating the number of tiktoks to scrape
#' @export
get_music_post <- function(music_id, n = 10000, port = NULL, ua = default_ua,  ...){
  # user <- get_username(username)
  get_n("music_post", n = n, music_id = music_id, query = music_id, port = port, ua = ua, ...)
}

#' get_hashtag_post
#' @description Function to get tiktoks from a given hashtag
#' @param hashtag Hashtag to scrape
#' @param n Number of tiktoks to retrieve
#' @export
get_hashtag_post <- function(hashtag, n = 10000, cursor = 0, save_dir = NULL,  port = NULL, ua = default_ua){
  hash <- get_hashtag(hashtag, port = port, ua = default_ua)

  get_n(type = "hashtag_post", n = n, cursor = cursor, hash_id = hash$challengeInfo.challenge.id, query = hashtag,
        save_dir = save_dir, port = port, ua = ua)
}

#' discover_hashtags
#' @description Function to get 20 suggested hashtags
#' @export
discover_hashtags <- function(ua = default_ua) get_n("discover_hash", ua = ua)

#' discover_music
#' @description Function to get 20 suggested musics
#' @export
discover_music <- function(ua = default_ua) get_n("discover_music", ua = ua)



