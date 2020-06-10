
#' tk_posts
#' @description Get TikToks from a given username/hashtag or music_id
#' @param scope Character indicating the endpoint to scrape (must be "user", "hashtag", "music" or "trends")
#' @param query Character indicating the username/hashtag/music_id to scrape
#' @param n Numeric indicating the number of tiktoks to scrape
#' @export
tk_posts <- function(scope, query, n, ...){

  if(!(scope %in% c("user", "hashtag", "music", "trends"))){
    stop("scope must be one of the following: user, hashtag, music or trends")
  }

  if(scope == "user"){
    user <- tk_info(scope = scope, query)
    result <- get_count(type = "user_post", count = n, user_id = user$id, sec_uid = user$secUid, query = query, ...)
  } else if(scope == "hashtag"){
    hash <- tk_info(scope = scope, query)
    ## TODO: hash$challengeInfo.challenge.id this probably needs to be a better variable name!
    result <- get_count("hashtag_post", count = n, hash_id = hash$challengeInfo.challenge.id, query = query, ...)
  } else  if(scope == "music"){
    result  <- get_count("music_post", count = n, music_id = query, query = query, ...)
  } else  if(scope == "trends"){
    result  <- get_count("trending", count = n, ...)
  }

  return(result)
}

#' tk_discover
#' @param scope Character indicating the endpoint to scrape (must be "hashtag" or "music")
#' @description Function to get 20 suggested hashtags or music_ids
#' @export
tk_discover <- function(scope){

  if(!(scope %in% c("hashtag", "music"))){
    stop("scope must be one of the following: hashtag or music")
  }

  if(scope == "hashtag"){
    result <- get_count("discover_hash")
  } else if(scope == "music"){
    result <- get_count("discover_music")
  }

  return(result)
}

