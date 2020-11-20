#' get_n
#' @description Main function to get data from tiktok
#' @export

get_n <- function(scope, n = 10000, start_date = lubridate::dmy("01-01-1900"),cursor = 0, ua = default_ua, port = NULL, query_1 = NULL, query_2 = "", save_dir = NULL, query = NULL, vpn = F, verify = ""){
  response <- tibble::tibble()
  if(stringr::str_detect(scope, "music|trend|hash")){
    max_n <- 30
  } else {
    max_n <- 50
  }
  max_cursor <- cursor
  min_cursor <- cursor

  while(nrow(response) < n){
    # cat("\rCursor: ", max_cursor, "  TikToks: ", nrow(response))

    url <- get_url(scope, n = max_n, min = min_cursor, max = max_cursor, query_1 = query_1, query_2 = query_2, verify = verify)
    out <- get_data(url, ua = ua, port = port, parse = T, vpn = vpn)

    if(scope %in% c("music_post", "hashtag_post")){
      out$items <- out$itemList
    }
    # if(scope %in% c("hashtag_post", "sound_post")){
    #   out$items <- out$body$itemListData
    # }

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
get_data <- function(url, ua = default_ua, parse = T, port = NULL, vpn = F, id_cookie = "", signed = F, time_out = 10){

  if(!signed){
    final_url <- get_signature(url, ua = ua, port = port)
  } else {
    final_url <- url
  }

  # final_url <- url

  if(vpn){
    req <- try(get_vpn_data(final_url, ua, id_cookie = id_cookie, time_out = time_out))
  } else {
    req <- try({
      httr::GET(final_url,
                httr::add_headers(.headers = c(
                  method = "GET",
                  referer = "https://www.tiktok.com/trending?lang=en",
                  `user-agent` = ua,
                  cookie = id_cookie
                ))#,
                # timeout = httr::timeout(time_out)
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
  if(content == "") content <- "{}"

  out <- content %>%
    jsonlite::fromJSON()

  return(out)
}

#' @export
get_vpn_data <- function(final_url, ua = default_ua, vpn_host = "", vpn_port = "", id_cookie = "", time_out = 10){

  if(vpn_host == ""){
    vpn_host <- Sys.getenv("tiktok_vpn_host")
  }

  if(vpn_port == ""){
    vpn_port <- Sys.getenv("tiktok_vpn_port")
  }

  head <- list(
    method = "GET",
    referer = "https://www.tiktok.com/trending?lang=en",
    `user-agent` = ua,
    cookie = id_cookie
  )

  data <- list(url = final_url, head = head)

  req <- try({
    httr::POST(glue::glue("http://{vpn_host}:{vpn_port}/get"), body = data,  encode = "json", timeout = httr::timeout(time_out))
  })

  return(req)
}
