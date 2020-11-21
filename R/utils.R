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
                    query_1 = NULL, query_2 = "",
                    ua = default_ua,
                    # username = NULL, user_id = NULL, sec_uid = NULL,
                    # comment_id = NULL, post_id = NULL,
                    # hashtag = NULL, hash_id = NULL,
                    # music_id = NULL,
                    max = 0, min =0){

  switch(
    type,
    "trending" = {
      base_url <- "https://m.tiktok.com/api/item_list/?aid=1988"

      url_params <- list(id = "1", secUid = "", count = n,
                         maxCursor = max, minCursor = min, sourceType = "12")

      url_params %>%
        purrr::imap_chr(~paste0("&", .y, "=", .x)) %>%
        paste(collapse = "")  %>%
        paste0(base_url, .) %>%
        add_verify_ua(ua, verify)
    },
    "user_post" = {
      base_url <- glue::glue("https://m.tiktok.com/api/item_list/?aid=1988")

      url_params <- list(id = query_1, secUid = query_2, count = n,
                         maxCursor = max, minCursor = min, sourceType = "8")

      url_params %>%
        purrr::imap_chr(~paste0("&", .y, "=", .x)) %>%
        paste(collapse = "")  %>%
        paste0(base_url, .) %>%
        add_verify_ua(ua, verify)

    },
    "username" = {
      glue::glue("https://www.tiktok.com/node/share/user/@{query_1}?aid=1988") %>%
        add_verify_ua(ua, verify)
    },
    "hashtag" = {
      glue::glue("https://m.tiktok.com/api/challenge/detail/?challengeName={query_1}&language=en") %>%
        add_verify_ua(ua, verify)
    },
    "hashtag_post" = {
      base_url <- "https://m.tiktok.com/api/challenge/item_list/?aid=1988"

      url_params <- list(challengeID = query_1, count = n, shareUid = "",
                         cursor = min)

      url_params %>%
        purrr::imap_chr(~paste0("&", .y, "=", .x)) %>%
        paste(collapse = "")  %>%
        paste0(base_url, .) %>%
        add_verify_ua(ua, verify)
    },
    "discover_hash" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount={n}&scene=0")
    },
    "music" = {
      glue::glue("https://m.tiktok.com/api/music/detail/?musicId={query_1}&language=en") %>%
        add_verify_ua(ua, verify)
    },
    "music_post" = {
      base_url <- "https://m.tiktok.com/api/music/item_list/?aid=1988"

      url_params <- list(musicID = query_1, count = n,cursor = min)

      url_params %>%
        purrr::imap_chr(~paste0("&", .y, "=", .x)) %>%
        paste(collapse = "")  %>%
        paste0(base_url, .) %>%
        add_verify_ua(ua, verify)
    },
    "discover_music" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount=30&scene=0&verifyFp=")
    },
    "comment" = {
      glue::glue("https://www.tiktok.com/api/comment/list/?aid=1988&aweme_id={query_1}&cursor={cursor}&count={n}&aid=1988") %>%
        add_verify_ua(ua, verify)
    },
    "reply" = {
      glue::glue("https://www.tiktok.com/api/comment/list/reply/?comment_id={query_1}&item_id={query_2}&cursor={cursor}&count={n}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    },
    "post" = {
      glue::glue("https://m.tiktok.com/api/item/detail/?itemId={query_1}&language=en") %>%
        add_verify_ua(ua, verify)
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
  require(reticulate)
  reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/stealth.py")
  reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/browser.py")
}


#' @title Set up a TikTok cookie
#' @description This function will add a TikTok cookie to your
#'   \code{.Renviron} file so it can be called securely without being stored in
#'   your code. After you have installed your key, it can be called any time by
#'   typing \code{Sys.getenv("TIKTOK_COOKIE")}. If you do not have an \code{.Renviron} file, the
#'   function will create one for you.
#' @param cookie The TikTok cookie as string. Find out here how to get a TikTok cookie: \url{https://www.youtube.com/watch?v=kYMV2ugxacs&feature=youtu.be}
#' @export tk_auth
#' @examples
#'
#' \dontrun{
#' # Put in your cookie:
#' tk_auth(cookie = "abcd012345678901234567890123456789")
#' # Restart R for changes to take effect or load your environment so you can use the cookie without
#' # restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("TIKTOK_COOKIE")
#' }
tk_auth <- function(cookie) {

  if (Sys.getenv("TIKTOK_COOKIE") == "" | missing(cookie)) {
    cookie <- rstudioapi::askForPassword(prompt = "Please enter the TikToK cookie")
    # message("Setting API key as AIRTABLE_API_KEY environment variable.")

    if(length(cookie)==0){
      stop("No Cookie specified. Please try again.")
    }
  }

  set_renv("TIKTOK_COOKIE" = cookie)

}

#' tk_install
#' @description Install needed python libraries
#' @export
tk_install <- function(){
  reticulate::py_install(c("pyppeteer", "pyppeteer_stealth", "asyncio", "requests"), pip = T)
}




#' tk_dl_video
#' @description Function that enable to download tiktoks
#' @export
#' @param url url of tiktok to scrape
#' @param path path to download tiktok video to
download_video <- function(url, path, ua = default_ua, time_out = 10){
  req <- httr::GET(url,
                   httr::add_headers(.headers = c(
                     'Connection' = 'keep-alive' ,
                     'User-Agent' = ua,
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
tk_dl_video <- function(post_id = NULL, url = NULL, path, ua = default_ua, port = NULL, vpn = F, verbose = T){
  if(!is.null(post_id)){
    post <- tk_info("post", post_id, ua = ua, port = port, vpn = vpn)
    index <- 0
    while(!any(stringr::str_detect(names(post), "itemInfo")) & index < 10){
      post <-tk_info("post", post_id, ua = ua, port = port, vpn = vpn) #%>% glimpse
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
    download_video(url, path, ua = ua)
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
default_ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36"

#'@export
encode_string <- function(string){
  stringr::str_replace_all(string, c("\\s+" = "+", "/" = "%2F", ";" = "%3B" ))
}

#'@export
add_verify_ua <- function(url, ua, verify){
  paste0(url, "&verifyFp=", verify, "&user_agent=", encode_string(ua))
}

#' @export
get_signature <- function(urls, ua = default_ua, port = NULL){
  # verify <- get_verify()
  # urls <- paste0(urls, "&verifyFp=", verify)

  if(!is.null(port)){
    out <- urls %>%
      purrr::map_chr(get_docker_signature, port = port)
  } else {
    out <- get_puppeteer_signature(urls, ua)
  }

  paste0(urls, "&_signature=", out)
}

#' @export
base36encode <- function(number){
  alphabet <- stringr::str_extract_all('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', ".")[[1]]

  output <- c()
  number <- round(as.numeric(number), 0)*1000

  if(number > 0 & number < length(alphabet)){
    output <- alphabet[i]
  }

  while(number != 0){
    i <- round(number, 0) %% 36
    number <- number %/% 36
    output <- c(alphabet[i], output)
  }

  return(paste0(output, collapse = ""))
}

#' @export
get_verify <- function(){
  chars <- stringr::str_extract_all("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", ".")[[1]]
  title <- base36encode(lubridate::now())

  ending <- 1:36 %>%
    imap_chr(~{
      if(.y == 1) return("0")
      if(.y %in% c(8, 13, 18, 23)) return("_")
      if(.y %in% c(14)) return("4")
      return(sample(chars, 1))
    }) %>%
    paste(collapse = "")

  paste0("verify_", title, ending)
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
get_docker_signature <- function(url, port = 8080, time_out = 10){
  res <- httr::POST(url  = glue::glue("http://localhost:{port}/signature"), body = url, timeout = httr::timeout(time_out))
  jsonlite::fromJSON(rawToChar(res$content))$signature
}

#' @export
predict_date <- function(tt_id) {

  from_unix(predict(date_tbl, newdata = tibble(post_id = as.numeric(tt_id))))

}
