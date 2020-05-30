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
get_user_post <- function(count, username){
  user <- get_username(username)
  get_count("user_post", count = count, user_id = user$id, sec_uid = user$secUid)
}

#' get_music_post
#' @description Function to get tiktoks from a given music
#' @param music_id Character indicating the username to scrape
#' @param count Numeric indicating the number of tiktoks to scrape
#' @export
get_music_post <- function(count, music_id){
  # user <- get_username(username)
  get_count("music_post", count = count, music_id = music_id)
}

#' get_hashtag_post
#' @description Function to get tiktoks from a given hashtag
#' @param hashtag Hashtag to scrape
#' @param count Number of tiktoks to retrieve
#' @export
get_hashtag_post <- function(count, hashtag){
  hash <- get_hashtag(hashtag)
  get_count("hashtag_post", count = count, hash_id = hash$challengeInfo.challenge.id)
}

#' discover_hashtags
#' @description Function to get 20 suggested hashtags
#' @export
discover_hashtags <- function() get_count("discover_hash")

#' discover_music
#' @description Function to get 20 suggested musics
#' @export
discover_music <- function() get_count("discover_music")



