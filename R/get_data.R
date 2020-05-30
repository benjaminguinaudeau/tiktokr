#' get_count
#' @description Main function to get data from tiktok
#' @export

get_count <- function(type, ..., count = 1){
  response <- tibble::tibble()
  max_count <- 99
  max_cursor <- 0
  min_cursor <- 0

  # while(length(response) < count){
  while(nrow(response) < count){

    if(count < max_count){
      real_count <- count
    } else {
      real_count <- max_count
    }

    url <- get_url(type, count = real_count, ...)
    # url <- get_url(type, count = real_count, hash_id = hash_id)

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
    }

    response <- dplyr::bind_rows(response, data) %>%
      unique

    real_count = count-nrow(response)
    max_cursor = out$maxCursor
    min_cursor = out$minCursor

    if("hasMore" %in% names(out)){if(!out$hasMore){return(response)}}
  }

  return(response)
}

#' get_data
#' @description Function for http queries
#' @export
get_data <- function(url, parse = T){
  br <- py$browser(url)

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
