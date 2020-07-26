#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' get_url
#' @description handle the api url based on a given type
#' @export

get_url <- function(type, n = NULL, cursor = NULL,
                    verify = "",
                    query_1 = NULL, query_2 = NULL,
                    # username = NULL, user_id = NULL, sec_uid = NULL,
                    # comment_id = NULL, post_id = NULL,
                    # hashtag = NULL, hash_id = NULL,
                    # music_id = NULL,
                    max = 0, min =0){

  switch(
    type,
    "trending" = {
      glue::glue("https://m.tiktok.com/api/item_list/?count={n}&id=1&type=5&secUid=&maxCursor={max}&minCursor={min}&sourceType=12&appId=1233")
    },
    "user_post" = {
      glue::glue("https://m.tiktok.com/api/item_list/?count={n}&id={query_1}&type=1&secUid={query_2}&maxCursor={max}&minCursor={min}&sourceType=8&appId=1233")
    },
    "username" = {
      glue::glue("https://m.tiktok.com/api/user/detail/?uniqueId={query_1}")
    },
    "hashtag" = {
      glue::glue("https://m.tiktok.com/api/challenge/detail/?verifyFP=&challengeName={query_1}&language=en")
    },
    "hashtag_post" = {
      glue::glue("https://m.tiktok.com/share/item/list?secUid=&id={query_1}&type=3&count={n}&minCursor={min}&maxCursor={max}&shareUid=&lang=en")
    },
    "discover_hash" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount={n}&scene=0")
    },
    "music" = {
      glue::glue("https://m.tiktok.com/api/music/detail/?musicId={query_1}&language=en")
    },
    "music_post" = {
      glue::glue("https://m.tiktok.com/share/item/list?secUid=&id={query_1}&type=4&count={n}&minCursor={min}&maxCursor={max}&shareUid=&lang=en")
    },
    "discover_music" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount=30&scene=0&verifyFp=")
    },
    "comment" = {
      glue::glue("https://www.tiktok.com/api/comment/list/?aweme_id={query_1}&cursor={cursor}&count={n}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    },
    "reply" = {
      glue::glue("https://www.tiktok.com/api/comment/list/reply/?comment_id={query_1}&item_id={query_2}&cursor={cursor}&count={n}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    },
    "post" = {
      glue::glue("https://m.tiktok.com/api/item/detail/?itemId={query_1}&language=en&verifyFp={verify}")
    }
  )
}

#' parse_json_structure
#' @description parse nested data frame in json responses
#' @export
#' @param x json to be parsed
parse_json_structure <- function(x){
  if(is.null(x)){return(tibble::tibble())}
  suppressMessages({
    x %>%
      dplyr::select_if(is.data.frame) %>%
      purrr::imap_dfc(~{
        if(!any(purrr::map_lgl(.x, is.data.frame))){
          return(purrr::set_names(.x, paste(.y, names(.x), sep = "_")))
        }
        parse_json_structure(.x)
      }) %>%
      dplyr::bind_cols(dplyr::select_if(x, ~!is.data.frame(.x))) %>%
      dplyr::mutate_if(is.list, as.character) %>%
      dplyr::mutate_if(is.logical, as.character)
  })
}

#' tk_init
#' @description Intitalize puppeeter browser in the reticulate session
#' @export
tk_init <- function(){
  reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/stealth.py")
  reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/browser.py")
}

#' tk_install
#' @description Install needed python libraries
#' @export
tk_install <- function(){
  reticulate::py_install(c("pyppeteer", "pyppeteer_stealth", "asyncio"), pip = T)
}



#' tk_dl_video
#' @description Function that enable to download tiktoks
#' @export
#' @param url url of tiktok to scrape
#' @param path path to download tiktok video to
tk_dl_video <- function(url, path, ua = default_ua, port = NULL, vpn = F, vpn_host = ""){
  raw_video <- get_data(url, parse = F, ua = default_ua, port = NULL, vpn = F, vpn_host = "")
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
  verify <- get_verify()
  urls <- paste0(urls, "&verifyFp=", verify)

  if(!is.null(port)){
      out <- urls %>%
        purrr::map_chr(get_docker_signature, port = port)
  } else {
      out <- get_puppeteer_signature(urls, ua)
  }

  paste0(urls, "&_signature=", out)
}

#' @export
get_verify <- function(){
  paste(sample(c(letters, LETTERS, as.character(1:9)), size = 16, replace = T), collapse = "")
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
