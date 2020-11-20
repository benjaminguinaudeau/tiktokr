
#' tk_posts
#' @description Get TikToks from a given username/hashtag or music_id
#' @param scope Character indicating the endpoint to scrape (must be "user", "hashtag", "music" or "trends")
#' @param query Character indicating the username/hashtag/music_id to scrape
#' @param n Numeric indicating the number of tiktoks to scrape
#' @export
tk_posts <- function(scope, query = "", n = 10000, start_date = lubridate::dmy("01-01-1900"), cursor = 0, save_dir = NULL, port = NULL, ua = default_ua, vpn = F, verbose = T){

  if(!(scope %in% c("user", "hashtag", "music", "trends"))){
    stop("scope must be one of the following: user, hashtag, music or trends")
  }

  out <- try({
    switch(
      scope,
      "user" = {
        user <- tk_info(scope = scope, query, port = port, ua = ua, vpn = vpn)
        if(length(user) == 0){
          tibble::tibble(author_uniqueId = query, id = NA_character_)
        } else if(user$stats.videoCount == 0 | user$user.privateAccount){
          user
        } else {
          tmp <- get_n("user_post", n = n, start_date = start_date, query_1 = user$user.id, query_2 = user$user.secUid, query = query,
                       save_dir = save_dir, port = port, ua = ua, vpn = vpn) %>%
            dplyr::bind_cols(user)
          if(nrow(tmp) == 0){
            user$stats.videoCount <- 0
            user
          } else {
            tmp
          }
        }
      },
      "hashtag" = {
        hash <- tk_info(scope = scope, query, port = port, ua = ua, vpn = vpn)
        ## TODO: hash$challengeInfo.challenge.id this probably needs to be a better variable name!
        get_n("hashtag_post", n = n, cursor = cursor, query_1 = hash$challengeInfo.challenge.id, query = query,
              save_dir = save_dir, port = port, ua = ua, vpn = vpn)
      },
      "music" = {
        get_n("music_post", n = n, query_1 = query, query = query,
              save_dir = save_dir, port = port, ua = ua, vpn = vpn)
      },
      "trends" = {
        get_n("trending", n = n,
              save_dir = save_dir, port = port, ua = ua, vpn = vpn)
      }
    )
  })

  if(inherits(out, "try-error")){
    return(tibble::tibble())
  } else {
    if(verbose){
      if(stringr::str_detect(scope, "music")){
        if(nrow(out) != 0){
          cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} ({nrow(out)})")
        } else {
          cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (no new video)")
        }
      } else {

        if(!any(stringr::str_detect(names(out), "stats"))){
          cli::cli_alert_warning("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (not found)")
        } else if ("stats.videoCount" %in% names(out)){
          if(unique(out[["stats.videoCount"]]) == 0){
            cli::cli_alert_info("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (no videos)")
          } else if(unique(out$user.privateAccount)){
            cli::cli_alert_info("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (private)")
          } else {
            out <- out %>%
              dplyr::filter(from_unix(createTime) > start_date)

            if(nrow(out) != 0){
              cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} ({nrow(out)})")
            } else {
              cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (no new video)")
            }
          }

        } else {
          out <- out %>%
            dplyr::filter(from_unix(createTime) > start_date)
        }
      }
    }
    return(out)
  }
}



#' tk_discover
#' @param scope Character indicating the endpoint to scrape (must be "hashtag" or "music")
#' @description Function to get 20 suggested hashtags or music_ids
#' @export
tk_discover <- function(scope, save_dir = NULL, port = NULL, ua = default_ua, vpn = F){

  if(!(scope %in% c("hashtag", "music"))){
    stop("scope must be one of the following: hashtag or music")
  }

  switch(
    scope,
    "hashtag" = get_n("discover_hash", save_dir = NULL, port = NULL, ua = ua, vpn = vpn),
    "music" = get_n("discover_music", save_dir = NULL, port = NULL, ua = ua, vpn = vpn)
  )

}

