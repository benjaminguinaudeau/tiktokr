#' get_n
#' @description Main function to get data from tiktok
#' @export

get_n <- function(scope, n = 10000, start_date = lubridate::dmy("01-01-1900"), cursor = 0, query_1 = NULL, query_2 = "", save_dir = NULL, query = NULL, ...){

  if(lubridate::is.Date(start_date)){ start_date <- as.numeric(start_date)}

  response <- tibble::tibble()
  if(stringr::str_detect(scope, "music|trend|hash")){
    max_n <- 30
  } else {
    max_n <- 50
  }
  if(!exists("cursor")) cursor <- 0
  max_cursor <- cursor
  min_cursor <- cursor

  while(nrow(response) < n){

    url <- get_url(scope, n = max_n, min = min_cursor, max = max_cursor, query_1 = query_1, query_2 = query_2)
    out <- get_data(url, parse = T, ...)

    if("found" %in% names(out)){
      return(out)
    }

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

    if(scope != "user_post"){ min_cursor <- as.character(out$cursor) }
    if(scope == "trending"){ min_cursor <- as.character(as.numeric(out$minCursor) + 1) }
    if(scope == "trending"){ max_cursor <- as.character(as.numeric(out$max_cursor) + 1) }

    # if(length(max_cursor) == 0){
    #   out <- out$body
    #   max_cursor <- out$maxCursor
    # }

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
      if(min(response$createTime) < start_date){
        response <- response %>%
          dplyr::filter(createTime > start_date)

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
get_data <- function(url, parse = T, cookie = "", time_out = 10){

  if(!stringr::str_detect(url, "&_signature=")){
    final_url <- get_signature(url)
  } else {
    final_url <- url
  }

  if(Sys.getenv("TIKTOK_VPN") != ""){
    req <- try(get_vpn_data(final_url, cookie = cookie, time_out = time_out))
  } else {
    req <- httr::GET(final_url,
                     httr::add_headers(.headers = c(
                       method = "GET",
                       referer = "https://www.tiktok.com/foryou",
                       `user-agent` = Sys.getenv("TIKTOK_UA"),
                       cookie = cookie
                     ))
    )
  }

  if(inherits(req, "try-error")){stop("Error happened while requesting")}

  if(!parse){return(req$content)}

  content <- rawToChar(req$content)

  if(content == "illegal request..."){
    warning("illegal request")
    content <- "{}"
  }

  if(content == "{\n    statusCode: -1,\n    itemInfo: {}\n} "){
    stop("empty response : request was blocked by tiktok")
    content <- "{}"
  }

  if(content == ""){
    stop("empty response : request was blocked by tiktok")
    content <- "{}"
  }

  out <- try({
    content %>%
      jsonlite::fromJSON()
  })
  if(inherits(out, "try-error")){
    out <- content %>%
      str_replace_all("(?<=[^\\d]{1,60})\\b(?=[^\\d]{1,60})", "\"") %>%
      jsonlite::fromJSON()
  }

  if(!is.null(out[["statusCode"]])){
    out$code <- out$statusCode
  }
  if(!is.null(out[["status_code"]])){
    out$code <- out$status_code
  }
  if(!is.null(out[["statusMsg"]])){
    out$msg <- out$statusMsg
  }

  if(!is.null(out[["code"]])){
    # print(out$code)
    # if(out$code == "8"){
    #   if(out$type == "verify"){
    #   stop("Captcha required. Please update your tiktok cookie using `tk_auth(cookie = <your new tiktok cookie>)` or wait some time before querying again. ")
    #   }
    # }
    if(as.numeric(out$code) == 10000 ){ #c("10202", "10221",  "10204", "10225")
      stop("Captcha required. Please update your tiktok cookie using `tk_auth(cookie = <your new tiktok cookie>)` or wait some time before querying again. ")
    }
    if(out$code == "10222"){
      return(out)
    }
    if(as.numeric(out$code) > 10000 ){ #c("10202", "10221",  "10204", "10225")
      # 10202 User not found
      # 10205 Hashtag not found
      # 10101 Hashtag not found


      # warning(paste("Error", out$code, out$msg))
      return(tibble::tibble(found = F))
    }
  }


  return(out)
}

