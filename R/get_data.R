#' get_n
#' @description Main function to get data from tiktok
#' @export

get_n <- function(type, n = 10000, cursor = 0, ua = default_ua, port = NULL, query_1 = NULL, query_2 = NULL, save_dir = NULL, query = NULL){
  response <- tibble::tibble()
  max_n <- 99
  max_cursor <- cursor
  min_cursor <- cursor

  while(nrow(response) < n){
    cat("\rCursor: ", max_cursor, "  TikToks: ", nrow(response))

    url <- get_url(type, n = 99, min = min_cursor, max = max_cursor, query_1 = query_1, query_2 = query_2)
    out <- get_data(url, ua = ua, port = port, parse = T)

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
    if(type != "user_post"){ min_cursor = as.character(out$minCursor)}

    utils::flush.console()

    if("hasMore" %in% names(out)){
      if(!out$hasMore){
        message("\nReached end of query or no more TikToks available.")
        if(!is.null(save_dir)){
          if(!dir.exists(save_dir)){dir.create(save_dir)}
          saveRDS(response, glue::glue("{save_dir}/{query}_{as.numeric(Sys.time())}.rds"))
        }
        return(response)
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
get_data <- function(url, ua = default_ua, parse = T, port = NULL){

  final_url = get_signature(url, ua = ua, port = port)

  .GlobalEnv[["test_req"]] <- req <- try({
    httr::GET(final_url, httr::add_headers(
      `method`= "GET",
      `accept-encoding` = "gzip, deflate, br",
      `referrer` = "https://www.tiktok.com/trending?lang=en",
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
