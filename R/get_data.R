#' get_count
#' @description Main function to get data from tiktok
#' @export

get_count <- function(type, ..., count = 1, save = F, path = NULL, query = NULL){
  response <- tibble::tibble()
  max_count <- 99
  max_cursor <- 0
  min_cursor <- 0
  set <- 1

  # while(length(response) < count){
  while(nrow(response) < count){
    cat("\rPage: ", set, "  TikToks: ", nrow(response))
    if(count < max_count){
      real_count <- count
    } else {
      real_count <- max_count
    }

    url <- get_url(type, count = real_count, min = min_cursor, max = max_cursor, ...)
    # url <- get_url(type, count = real_count, hash_id = hash_id, min = min_cursor, max = max_cursor)

    out <- quiet(get_data(url))

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
    }

    if(save){
      if(!dir.exists(path)){dir.create(path)}
      saveRDS(data, glue::glue("{path}/{query}_{set}.RDS"))

    } else {
      response <- dplyr::bind_rows(response, data) %>%
        unique
    }

    set <- set + 1

    real_count = count-nrow(response)
    max_cursor = out$body$maxCursor
    min_cursor = out$body$minCursor

    utils::flush.console()

    if("hasMore" %in% names(out$body)){
      if(!out$body$hasMore){
        message("\nReached end of query or no more TikToks available.")
        return(response)
      }
    }
  }

  return(response)
}

#' get_data
#' @description Function for http queries
#' @param url url to visit and get data from
#' @param parse logical. whether to return parsed data or not. Defautls to \code{TRUE}.
#' @export
get_data <- function(url, parse = T){
  br <- quiet(py$browser(url))

  final_url = paste0(url, "&_signature=", br$signature)

  req <- httr::GET(final_url, httr::add_headers(
    `method`= "GET",
    `accept-encoding` = "gzip, deflate, br",
    `referrer` = "https://www.tiktok.com/tag/jakefromstate?lang=en",
    `user-agent` = br$userAgent))

  if(!parse){return(req$content)}

  content <- rawToChar(req$content)

  if(content == "") content <- "{}"


  out <- content %>%
    jsonlite::fromJSON()

  return(out)
}
