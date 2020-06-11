
#' tk_posts
#' @description Get TikToks from a given username/hashtag or music_id
#' @param scope Character indicating the endpoint to scrape (must be "user", "hashtag", "music" or "trends")
#' @param query Character indicating the username/hashtag/music_id to scrape
#' @param n Numeric indicating the number of tiktoks to scrape
#' @export
tk_posts <- function(scope, query, n = 10000, cursor = 0, save_dir = NULL, port = NULL, ua = default_ua){

  if(!(scope %in% c("user", "hashtag", "music", "trends"))){
    stop("scope must be one of the following: user, hashtag, music or trends")
  }

  switch(
    scope,
    "user" = {
      user <- tk_info(scope = scope, query, port = port, ua = ua)
      if(length(user) == 0){message(glue::glue("{query} was not found")) ; return(tibble::tibble())}
      get_n(type = "user_post", n = n, query_1 = user$id, query_2 = user$secUid, query = query,
                      save_dir = save_dir, port = port, ua = ua)
    },
    "hashtag" = {
      hash <- tk_info(scope = scope, query, port = port, ua = ua)
      ## TODO: hash$challengeInfo.challenge.id this probably needs to be a better variable name!
      get_n(type = "hashtag_post", n = n, cursor = cursor, query_1 = hash$challengeInfo.challenge.id, query = query,
                      save_dir = save_dir, port = port, ua = ua )
    },
    "music" = {
      get_n("music_post", n = n, query_1 = query, query = query,
                    save_dir = save_dir, port = port, ua = ua)
    },
    "trends" = {
      get_n("trending", n = n,
                     save_dir = save_dir, port = port, ua = ua)
    }
  )

}

#' tk_discover
#' @param scope Character indicating the endpoint to scrape (must be "hashtag" or "music")
#' @description Function to get 20 suggested hashtags or music_ids
#' @export
tk_discover <- function(scope, save_dir = NULL, port = NULL, ua = default_ua){

  if(!(scope %in% c("hashtag", "music"))){
    stop("scope must be one of the following: hashtag or music")
  }

  switch(
    scope,
    "hashtag" = get_n("discover_hash", save_dir = NULL, port = NULL, ua = ua),
    "music" = get_n("discover_music", save_dir = NULL, port = NULL, ua = ua)
  )

}

