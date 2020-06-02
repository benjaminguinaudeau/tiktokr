#' get_trending
#' @description Function to get trending tiktoks
#' @param count A numeric scalar indicating the number of tiktoks to scrape
#' @export
get_trending <- function(count) get_count("trending", count = count)

#' get_user_post
#' @description Function to get tiktoks from a given username
#' @param username Character indicating the username to scrape
#' @param count Numeric indicating the number of tiktoks to scrape
#' @export
get_user_post <- function(count, username, save = F, path = NULL, ...){
  user <- get_username(username)

  if(length(user) == 0){message(glue::glue("{username} was not found")) ; return(tibble::tibble())}

  get_count(type = "user_post", count = count, user_id = user$id, sec_uid = user$secUid, query = username, save = save, path = path, ...)
}

#' get_music_post
#' @description Function to get tiktoks from a given music piece
#' @param music_id Character indicating the \code{music_id} to scrape
#' @param count Numeric indicating the number of tiktoks to scrape
#' @export
get_music_post <- function(count, music_id, ...){
  # user <- get_username(username)
  get_count("music_post", count = count, music_id = music_id, query = music_id, ...)
}

#' get_hashtag_post
#' @description Function to get tiktoks from a given hashtag
#' @param hashtag Hashtag to scrape
#' @param count Number of tiktoks to retrieve
#' @export
get_hashtag_post <- function(count, hashtag, cursor = 0, save = F, path = NULL,  ...){
  hash <- get_hashtag(hashtag)
  get_count(type = "hashtag_post", count = count, cursor = cursor, hash_id = hash$challengeInfo.challenge.id, query = hashtag,
            save = save, path = path, ...)
}

#' discover_hashtags
#' @description Function to get 20 suggested hashtags
#' @export
discover_hashtags <- function() get_count("discover_hash")

#' discover_music
#' @description Function to get 20 suggested musics
#' @export
discover_music <- function() get_count("discover_music")



