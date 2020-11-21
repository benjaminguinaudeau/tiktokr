#' get_n
#' @description Main function to get data from tiktok
#' @export

get_n <- function(scope, n = 10000, start_date = lubridate::dmy("01-01-1900"), cursor = 0, query_1 = NULL, query_2 = "", save_dir = NULL, query = NULL, ...){
  response <- tibble::tibble()
  if(stringr::str_detect(scope, "music|trend|hash")){
    max_n <- 30
  } else {
    max_n <- 50
  }
  max_cursor <- cursor
  min_cursor <- cursor

  while(nrow(response) < n){

    url <- get_url(scope, n = max_n, min = min_cursor, max = max_cursor, query_1 = query_1, query_2 = query_2)
    out <- get_data(url, parse = T, ...)

    if(scope %in% c("music_post", "hashtag_post")){
      out$items <- out$itemList
    }

    if(stringr::str_detect(scope, "discover")){

      data <- out$body$exploreList %>%
        purrr::compact() %>%
        purrr::map_dfr(parse_json_structure)

      return(data)

    } else {

      data <- out$items %>% parse_json_structure

      if(length(data) == 0){

        # message("\nReached end of query or no more TikToks available.")

        if(!is.null(save_dir)){

          if(!dir.exists(save_dir)){dir.create(save_dir)}
          saveRDS(response, glue::glue("{save_dir}/{query}_{as.numeric(Sys.time())}.rds"))

        }

        return(response)
      }
    }

    response <- dplyr::bind_rows(response, data) %>%
      dplyr::distinct_at(dplyr::vars(dplyr::contains("id")), .keep_all = T)

    max_cursor = as.character(out$maxCursor)

    if(length(max_cursor) == 0){
      out <- out$body
      max_cursor <- out$maxCursor
    }
    if(scope != "user_post"){ min_cursor = as.character(out$minCursor)}

    # utils::flush.console()

    if("hasMore" %in% names(out)){
      if(!out$hasMore){
        # message("\nReached end of query or no more TikToks available.")
        if(!is.null(save_dir)){
          if(!dir.exists(save_dir)){dir.create(save_dir)}
          saveRDS(response, glue::glue("{save_dir}/{query}_{as.numeric(Sys.time())}.rds"))
        }
        return(response)
      }
    }

    if(scope == "user_post"){
      if(min(from_unix(response$createTime)) < start_date){
        response <- response %>%
          dplyr::filter(from_unix(createTime) < start_date)

        n <- 0
      }
    }
  }

  if(!is.null(save_dir)){
    if(!dir.exists(save_dir)){dir.create(save_dir)}
    saveRDS(response, glue::glue("{save_dir}/{query}_{as.numeric(Sys.time())}.rds"))
  }

  return(response)

}

#' get_data
#' @description Function for http queries
#' @param url url to visit and get data from
#' @param parse logical. whether to return parsed data or not. Defautls to \code{TRUE}.
#' @export
get_data <- function(url, parse = T, port = NULL, vpn = F, cookie = "", time_out = 10){

  if(!stringr::str_detect(url, "&_signature=")){
    url <- get_signature(url, port = port)
  }

  if(vpn){
    req <- try(get_vpn_data(url, cookie = cookie, time_out = time_out))
  } else {
    req <- try({
      httr::GET(url,
                httr::add_headers(.headers = c(
                  method = "GET",
                  referer = "https://www.tiktok.com/foryou",
                  `user-agent` = Sys.getenv("TIKTOK_UA"),
                  cookie = cookie
                ))
      )
    })
  }

  if(inherits(req, "try-error")){stop("Error happened while requesting")}

  if(!parse){return(req$content)}

  content <- rawToChar(req$content)

  if(content == "illegal request..."){
    warning("illegal request")
    content <- "{}"
  }

  if(content == ""){
    warning("empty response ; something was probably wrong with the request")
    content <- "{}"
  }

  out <- content %>%
    jsonlite::fromJSON()

  if(!is.null(out[["statusCode"]])){
    if(out$statusCode == "10000"){
      stop("Captcha required. Please update TIKTOK_COOKIE using `tk_auth()`.")
    }
  }
  if(!is.null(out[["status_code"]])){
    if(out$status_code == "8"){
      # stop("Captcha required. Please update the cookie file.")
    }
  }

  return(out)
}

