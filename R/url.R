#' shape_url
#' @description add query parameter, user agent and current verify to the url
#' @export
shape_url <- function(base_url, ...){

  dots <- list(...)

  if(length(dots) > 0){
    out <- dots %>%
      purrr::imap_chr(~paste0("&", .y, "=", .x)) %>%
      paste(collapse = "")
  } else {
    out <- ""
  }

  paste0(base_url, out, "&verifyFp=", get_current_verify(), "&user_agent=", encode_string(Sys.getenv("TIKTOK_UA")))

}

#' get_url
#' @description Encode the user agent
#' @export
shape_ua <- function(ua){
  stringr::str_replace_all(ua, c("\\/" = "%252F", " " = "%2B", "\\(" = "%28", "\\)" = "%29", "\\," = "%2C", "\\;" = "%253B"))
}


#' get_url
#' @description handle the api url based on a given type
#' @export

get_url <- function(type, n = NULL, cursor = NULL,
                    query_1 = NULL, query_2 = "",
                    max = 0, min =0){

  switch(
    type,
    "trending" = {

      "https://m.tiktok.com/api/item_list/?aid=1988" %>%
        shape_url(id = "1", secUid = "", count = n,
                  maxCursor = max, minCursor = min, sourceType = "12")

    },
    "user_post" = {

      glue::glue("https://m.tiktok.com/api/item_list/?aid=1988") %>%
        shape_url(id = query_1, secUid = query_2, count = n,
                  maxCursor = max, minCursor = min, sourceType = "8")

    },
    "username" = {
      glue::glue("https://www.tiktok.com/node/share/user/@{query_1}?aid=1988") %>%
        shape_url()
    },
    "hashtag" = {
      glue::glue("https://m.tiktok.com/api/challenge/detail/?challengeName={query_1}&language=en") %>%
        shape_url()
    },
    "hashtag_post" = {
      "https://m.tiktok.com/api/challenge/item_list/?aid=1988" %>%
        shape_url(challengeID = query_1, count = n, shareUid = "", cursor = min)
    },
    # "discover_hash" = {
    #   glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount={n}&scene=0")
    # },
    "music" = {
      glue::glue("https://m.tiktok.com/api/music/detail/?musicId={query_1}&language=en") %>%
        shape_url()
    },
    "music_post" = {

      "https://m.tiktok.com/api/music/item_list/?aid=1988" %>%
        shape_url(musicID = query_1, count = n,cursor = min)

    },
    # "discover_music" = {
    #   glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount=30&scene=0&verifyFp=")
    # },
    "comment" = {
      glue::glue("https://www.tiktok.com/api/comment/list/?aid=1988&aweme_id={query_1}&cursor={cursor}&count={n}&aid=1988") %>%
        shape_url()
    },
    # "reply" = {
    #   glue::glue("https://www.tiktok.com/api/comment/list/reply/?comment_id={query_1}&item_id={query_2}&cursor={cursor}&count={n}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    # },
    "post" = {
      glue::glue("https://m.tiktok.com/api/item/detail/?aid=1988&itemId={query_1}&language=en") %>%
        shape_url()
    }
  )
}
