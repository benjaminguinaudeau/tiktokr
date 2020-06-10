#' get_url
#' @description handle the api url based on a given type
#' @export

get_url <- function(type, n = NULL, cursor = NULL,
                    verify = NULL,
                    username = NULL, user_id = NULL, sec_uid = NULL,
                    comment_id = NULL, post_id = NULL,
                    hashtag = NULL, hash_id = NULL,
                    music_id = NULL,
                    max = 0, min =0){

  switch(
    type,
    "trending" = {
      glue::glue("https://m.tiktok.com/api/item_list/?count={n}&id=1&type=5&secUid=&maxCursor={max}&minCursor={min}&sourceType=12&appId=1233&verifyFp=")
    },
    "user_post" = {
      glue::glue("https://m.tiktok.com/api/item_list/?count={n}&id={user_id}&type=1&secUid={sec_uid}&maxCursor={max}&minCursor={min}&sourceType=8&appId=1233&region=US&language=en&verifyFp=")
    },
    "username" = {
      glue::glue("https://m.tiktok.com/api/user/detail/?uniqueId={username}&language=en&verifyFp=")
    },
    "hashtag" = {
      glue::glue("https://m.tiktok.com/api/challenge/detail/?verifyFP=&challengeName={hashtag}&language=en")
    },
    "hashtag_post" = {
      glue::glue("https://m.tiktok.com/share/item/list?secUid=&id={hash_id}&type=3&count={n}&minCursor={min}&maxCursor={max}&shareUid=&lang=en&verifyFp=")
    },
    "discover_hash" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount={n}&scene=0&verifyFp=")
    },
    "music" = {
      glue::glue("https://m.tiktok.com/api/music/detail/?musicId={music_id}&language=en&verifyFp=")
    },
    "music_post" = {
      glue::glue("https://m.tiktok.com/share/item/list?secUid=&id={music_id}&type=4&count={n}&minCursor={min}&maxCursor={max}&shareUid=&lang=en&verifyFp=")
    },
    "discover_music" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount=30&scene=0&verifyFp=")
    },
    "comment" = {
      glue::glue("https://www.tiktok.com/api/comment/list/?aweme_id={post_id}&cursor={cursor}&count={n}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    },
    "reply" = {
      glue::glue("https://www.tiktok.com/api/comment/list/reply/?comment_id={comment_id}&item_id={post_id}&cursor={cursor}&count={n}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    }
  )
}

#' parse_json_structure
#' @description parse nested data frame in json responses
#' @export
#' @param x json to be parsed
parse_json_structure <- function(x){
  if(is.null(x)){return(tibble::tibble())}
  suppressMessages({ x %>%
      dplyr::select_if(is.data.frame) %>%
      purrr::map_dfc(~{
        if(!any(purrr::map_lgl(.x, is.data.frame))){return(.x)}
        parse_json_structure(.x)
      }) %>%
      dplyr::bind_cols(dplyr::select_if(x, ~!is.data.frame(.x)))
  })
}

#' init_tiktokr
#' @description Intitalize puppeeter browser in the reticulate session
#' @export
init_tiktokr <- function(){
  reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/browser.py")
}

#' install_tiktokr
#' @description Install needed python libraries
#' @export
install_tiktokr <- function(){
  reticulate::py_install(c("pyppeteer", "pyppeteer_stealth", "asyncio"), pip = T)
}



#' download_video
#' @description Function that enable to download tiktoks
#' @export
#' @param url url of tiktok to scrape
#' @param path path to download tiktok video to
download_video <- function(url, path){
  raw_video <- get_data(url, parse = F)
  writeBin(raw_video, path)
}

#' quiet
#' @export
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' from_unix
#'
#' @description Converts UNIX timestamp to datetime format
#' @param x UNIX timestamp to be converted to datetime
#' @export
from_unix <- function(x) {
  as.POSIXct(as.numeric(x), origin = '1970-01-01', tz = 'UTC')
}

#'@export
default_ua <- "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A372 Safari/604.1"


#' @export
get_signature <- function(urls, ua, port = NULL){
  if(!is.null(port)){
      out <- urls %>%
        purrr::map_chr(get_docker_signature, port = port)
  } else {
      out <- get_puppeteer_signature(urls, ua)
  }

  paste0(urls, "&_signature=", out)
}

#' @export
get_puppeteer_signature <- function(urls, ua){

  url <- paste(urls, collapse = '", "')

  tries <- 0
  br <- tryCatch(py$browser(url, ua), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
  while(tries < 3 & inherits(br, "try-error")){
    br <- tryCatch(py$browser(url, ua), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
    tries <- tries + 1
  }

  return(br$signature)
}

#' @export
get_docker_signature <- function(url, port = 8080){
  res <- httr::POST(url  = glue::glue("localhost:{port}/signature"), body = url)
  jsonlite::fromJSON(rawToChar(res$content))$signature
}
