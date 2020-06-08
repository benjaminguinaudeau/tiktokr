#' get_count
#' @description Main function to get data from tiktok
#' @export

get_count <- function(type, cursor = 0, ..., count = 1, save = F, path = NULL, query = NULL){
  response <- tibble::tibble()
  max_count <- 99
  max_cursor <- cursor
  min_cursor <- cursor

  while(nrow(response) < count){
    cat("\rCursor: ", max_cursor, "  TikToks: ", nrow(response))

    url <- get_url(type, count = 99, min = min_cursor, max = max_cursor, ...)
    # url <- get_url(type, count = 99, user_id = user_id, sec_uid = sec_uid, min = min_cursor, max = max_cursor)
    # url <- get_url(type, count = real_count, hash_id = hash_id, min = min_cursor, max = max_cursor)

    out <- get_data(url)

    if(type %in% c("hashtag_post", "sound_post")){
      out$items <- out$body$itemListData
    }

    if(stringr::str_detect(type, "discover")){
      data <- out$body$exploreList %>%
        purrr::compact() %>%
        purrr::map_dfr(parse_json_structure)

      return(data)
    } else {
      data <- out$items %>% parse_json_structure
      if(length(data) == 0){
        message("\nReached end of query or no more TikToks available.")
        if(save){
          if(!dir.exists(path)){dir.create(path)}
          saveRDS(data, glue::glue("{path}/{query}.rds"))
          return(T)
        }
        return(response)
      }
    }

    response <- dplyr::bind_rows(response, data) %>%
      dplyr::distinct(id, .keep_all = T)

    max_cursor = as.character(out$maxCursor)
    if(length(max_cursor) == 0){
      out <- out$body
      max_cursor <- out$maxCursor
    }
    if(type != "user_post"){ min_cursor = as.character(out$minCursor)}

    utils::flush.console()

    if("hasMore" %in% names(out)){
      if(!out$hasMore){
        message("\nReached end of query or no more TikToks available.")
        if(save){
          if(!dir.exists(path)){dir.create(path)}
          readr::write_rds(data, glue::glue("{path}/{query}.rds"))
          return(T)
        }
        return(response)
      }
    }
  }

  if(save){
    if(!dir.exists(path)){dir.create(path)}
    saveRDS(data, glue::glue("{path}/{query}.rds"))
    return(T)
  }
    return(response)

}

#' get_data
#' @description Function for http queries
#' @param url url to visit and get data from
#' @param parse logical. whether to return parsed data or not. Defautls to \code{TRUE}.
#' @export
get_data <- function(url, ua = default_ua, parse = T, port = NULL){

  final_url = get_signature(url, ua, port = port)

  .GlobalEnv[["test_req"]] <- req <- try({
    httr::GET(final_url, httr::add_headers(

      `method`= "GET",
      `accept-encoding` = "gzip, deflate, br",
      `referrer` = "https://www.tiktok.com/tag/jakefromstate?lang=en",
      `user-agent` = ua))
  })

  if(inherits(req, "try-error")){stop("Error happened while requesting")}

  if(!parse){return(req$content)}

  content <- rawToChar(req$content)

  if(content == "") content <- "{}"


  out <- content %>%
    jsonlite::fromJSON()

  return(out)
}
