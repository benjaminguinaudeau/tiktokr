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

#' tk_dwnl
#' @description Function that enable to download tiktoks
#' @export
#' @param url url of tiktok to scrape
#' @param path path to download tiktok video to
tk_dwnl <- function(url, path, time_out = 10){
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

#' tk_dwnl_from_id
#' @description Function that enable to download tiktoks given a post_id
#' @export
#' @param url url of tiktok to scrape
#' @param path path to download tiktok video to
tk_dwnl_from_id <- function(post_id = NULL, url = NULL, path, verbose = T, ...){
  if(!is.null(post_id)){
    post <- tk_info("post", post_id, ...)
    index <- 0
    while(!any(stringr::str_detect(names(post), "itemInfo")) & index < 10){
      post <-tk_info("post", post_id, ...) #%>% glimpse
      index <- index + 1
      try(if(post$statusCode == "10201"){ index <- 10 }, silent = T)
    }
    if(index == 10){
      readr::write_rds(tibble::tibble(), stringr::str_replace(path, "mp4$", "rds$"))
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
    readr::write_rds(tibble::tibble(), stringr::str_replace(path, "mp4$", "rds"))
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
#' @examples
#'
#' \dontrun{
#' # Convert unix timestamp to datetime (POSIXct)
#' from_unix(1538430000)
#' }
from_unix <- function(x) {
  as.POSIXct(as.numeric(x), origin = '1970-01-01', tz = 'UTC')
}

#'@export
encode_string <- function(string){
  stringr::str_replace_all(string, c("\\s+" = "+", "/" = "%2F", ";" = "%3B" ))
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
predict_date <- function(tt_id) {

  from_unix(predict(date_tbl, newdata = tibble(post_id = as.numeric(tt_id))))

}
