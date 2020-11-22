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
      glue::glue("https://m.tiktok.com/api/item/detail/?itemId={query_1}&language=en") %>%
        shape_url()
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

#' tk_dl_video
#' @description Function that enable to download tiktoks
#' @export
#' @param url url of tiktok to scrape
#' @param path path to download tiktok video to
download_video <- function(url, path, time_out = 10){
  req <- httr::GET(url,
                   httr::add_headers(.headers = c(
                     'Connection' = 'keep-alive' ,
                     'User-Agent' = Sys.getenv("TIKTOK_UA"),
                     'Accept' = '*/*' ,
                     'Sec-Fetch-Site' = 'cross-site' ,
                     'Sec-Fetch-Mode' = 'no-cors' ,
                     'Sec-Fetch-Dest' = 'video' ,
                     'Referer' = 'https://www.tiktok.com/foryou?lang=fr' ,
                     'Accept-Language' = 'en-GB,en;q=0.9' ,
                     'Range' = 'bytes=0-'
                   )),
                   timeout = httr::timeout(time_out))

  writeBin(req$content, con = path)
}

#' tk_dl_video
#' @description Function that enable to download tiktoks
#' @export
#' @param url url of tiktok to scrape
#' @param path path to download tiktok video to
tk_dl_video <- function(post_id = NULL, url = NULL, path, verbose = T, ...){
  if(!is.null(post_id)){
    post <- tk_info("post", post_id, ...)
    index <- 0
    while(!any(stringr::str_detect(names(post), "itemInfo")) & index < 10){
      post <-tk_info("post", post_id, ...) #%>% glimpse
      index <- index + 1
      try(if(post$statusCode == "10201"){ index <- 10 }, silent = T)
    }
    if(index == 10){
      write_rds(tibble::tibble(), str_replace(path, "mp4$", "rds$"))
      if(verbose) cli::cli_alert('[{Sys.time()}] {str_replace(path, "mp4$", "rds")}')
      return(list())
    } else {
      url <- post$itemInfo.itemStruct.video.downloadAddr
    }
  }

  out <- try({
    download_video(url, path)
  }, silent = T)

  if(inherits(out, "try-error")){
    write_rds(tibble::tibble(), str_replace(path, "mp4$", "rds"))
    if(verbose) cli::cli_alert(glue::glue("[{Sys.time()}] {path}"))
  } else {
    if(verbose) cli::cli_alert_success(glue::glue("[{Sys.time()}] {path}"))
  }

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
encode_string <- function(string){
  stringr::str_replace_all(string, c("\\s+" = "+", "/" = "%2F", ";" = "%3B" ))
}

#' @export
get_signature <- function(urls, port = NULL){

  if(!is.null(port)){
    out <- urls %>%
      purrr::map_chr(get_docker_signature, port = port)
  } else {
    out <- get_puppeteer_signature(urls, ua = Sys.getenv("TIKTOK_UA"))
  }

  paste0(urls, "&_signature=", out)
}

#' @export
get_current_verify <- function(){
  if(stringr::str_detect(Sys.getenv("TIKTOK_COOKIE"), "verify_.")){
    Sys.getenv("TIKTOK_COOKIE") %>% stringr::str_extract("verify_.*?(?=\\s|;|$)")
  } else {
    return("")
  }
}

#' @export
get_puppeteer_signature <- function(urls, ua){

  if(!exists("py")) stop("Tiktokr was not initialized. Please run `tk_init()")
  if(!"browser" %in% names(py)) stop("Tiktokr was not initialized. Please run `tk_init()")


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
predict_date <- function(tt_id) {

  from_unix(predict(date_tbl, newdata = tibble(post_id = as.numeric(tt_id))))

}
