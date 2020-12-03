
#' tk_posts
#' @description Get TikToks from a given username/hashtag or music_id.
#'
#' 1. user (no known API-limit)
#'
#' 2. hashtag (API-limit of 2 000 tiktoks at a time)
#'
#' 3. music (API-limit of 2 000 tiktoks at a time)
#'
#' 4. trends (API-limit of 2 000 tiktoks at a time)
#'
#' @param scope Character indicating the endpoint to scrape (must be "user", "hashtag", "music" or "trends")
#' @param query Character indicating the username/hashtag/music_id to scrape
#' @param n Numeric indicating the number of tiktoks to scrape
#' @export
#' @examples
#' \dontrun{
#'
#' # Get posts with hashtags
#' tk_posts(scope = "hashtag", query = "maincharacter", n = 100)
#' # Get posts from user willsmith
#' tk_posts(scope = "user", query = "willsmith", n = 50)
#'
#' }
tk_posts <- function(scope, query = "", n = 10000, start_date = lubridate::dmy("01-01-1900"), save_dir = NULL, verbose = T, ...){

  if(lubridate::is.Date(start_date)){ start_date <- as.numeric(start_date)}

  if(!(scope %in% c("user", "hashtag", "music", "trends"))){
    stop("scope must be one of the following: user, hashtag, music or trends")
  }

  out <- switch(
    scope,

    "user" = {
      user <- tk_info(scope = scope, query, ...)
      if("found" %in% names(user)){

        if(verbose) cli::cli_alert_warning("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (not found)")
        return(tibble::tibble(query = query, found = F))

      } else if(user$stats.videoCount == 0 | user$user.privateAccount){

        if(user[["stats.videoCount"]] == 0){
          if(verbose) cli::cli_alert_info("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (no videos)")
        } else {
          if(verbose) cli::cli_alert_info("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (account is private)")
        }
        return(user)

      } else {

        tmp <- get_n(scope = "user_post", n = n, start_date = start_date, query_1 = user$user.id, query_2 = user$user.secUid, query = query,
                     save_dir = save_dir, ...) %>%
          dplyr::bind_cols(user)

        if(nrow(tmp) == 0){

          user$stats.videoCount <- 0
          return(user)
        } else {

          tmp

        }
      }
    },

    "hashtag" = {
      hash <- tk_info(scope = scope, query, ...)
      if("found" %in% names(hash)){
        if(verbose) cli::cli_alert_warning("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (not found)")
        return(hash)
      }
      get_n("hashtag_post", n = n, query_1 = hash$challengeInfo.challenge.id, query = query, save_dir = save_dir, ...)
    },

    "music" = {
      tmp <- get_n("music_post", n = n, query_1 = query, query = query, save_dir = save_dir, ...)
      if("found" %in% names(tmp)){
        return(tibble::tibble(query = query, found = F))
      }
      if(verbose & n > 1 & nrow(tmp) < 2) cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (no new video)")
      tmp
    },
    "trends" = {
      tmp <- get_n(scope = "trending", n = n, save_dir = save_dir, ...)
      if("found" %in% names(tmp)){
        return(tibble::tibble(query = "trend", found = F))
      }
      tmp
    }
  )

  # if(verbose){
  # if ("stats.videoCount" %in% names(out)){
  # if(unique(out[["stats.videoCount"]]) == 0){
  # cli::cli_alert_info("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (no videos)")
  # } else if(unique(out$user.privateAccount)){
  # cli::cli_alert_info("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (private)")
  # } else {
  out <- out %>%
    dplyr::filter(createTime > start_date)

  # if(nrow(out) != 0){
  #   cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} ({nrow(out)})")
  # } else {
  #   cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} (no new video)")
  # }
  # }
  # } else {
  # out <- out %>%
  #   dplyr::filter(createTime > start_date)
  # }
  # }
  return(out)
}


# TODO

#' tk_discover
#' @param scope Character indicating the endpoint to scrape (must be "hashtag" or "music")
#' @description Function to get 20 suggested hashtags or music_ids
# tk_discover <- function(scope, save_dir = NULL, port = NULL, ua = default_ua, vpn = F){
#
#   if(!(scope %in% c("hashtag", "music"))){
#     stop("scope must be one of the following: hashtag or music")
#   }
#
#   switch(
#     scope,
#     "hashtag" = get_n("discover_hash", save_dir = NULL, port = NULL, ua = ua, vpn = vpn),
#     "music" = get_n("discover_music", save_dir = NULL, port = NULL, ua = ua, vpn = vpn)
#   )
#
# }

