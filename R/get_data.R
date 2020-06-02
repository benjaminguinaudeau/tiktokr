#' get_count
#' @description Main function to get data from tiktok
#' @export

get_count <- function(type, cursor = 0, ..., count = 1, save = F, path = NULL, query = NULL){
  response <- tibble::tibble()
  max_count <- 99
  max_cursor <- cursor
  # max_cursor <- 100000000
  min_cursor <- cursor
  # min_cursor <- 100000000

  # while(length(response) < count){
  while(nrow(response) < count){
    cat("\rCursor: ", max_cursor, "  TikToks: ", nrow(response))
    if(count < max_count){
      real_count <- count
    } else {
      real_count <- max_count
    }

    url <- get_url(type, count = real_count, min = min_cursor, max = max_cursor, ...)
    # url <- get_url(type, count = real_count, user_id = user_id, sec_uid = sec_uid, min = min_cursor, max = max_cursor)
    # url <- get_url(type, count = real_count, hash_id = hash_id, min = min_cursor, max = max_cursor)


    # cli::cli_alert("Url: {url}")
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
        } else {
          return(response)
        }
      }
    }

    response <- dplyr::bind_rows(response, data) %>%
      dplyr::distinct(id, .keep_all = T)



    real_count = count-nrow(response)
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
          saveRDS(data, glue::glue("{path}/{query}.rds"))
        } else {
          return(response)
        }
      }
    }
  }

  if(save){
    if(!dir.exists(path)){dir.create(path)}
    saveRDS(data, glue::glue("{path}/{query}.rds"))
  } else {
    return(response)
  }
}

#' get_data
#' @description Function for http queries
#' @param url url to visit and get data from
#' @param parse logical. whether to return parsed data or not. Defautls to \code{TRUE}.
#' @export
get_data <- function(url, parse = T){

  tries <- 0
  # print(1)
  br <- tryCatch(py$browser(url), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
  # print(2)
  while(tries < 3 & inherits(br, "try-error")){
    br <- tryCatch(py$browser(url), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
    tries <- tries + 1
  }

  final_url = paste0(url, "&_signature=", br$signature)

  # print(3)
  .GlobalEnv[["test_req"]] <- req <- try({
    httr::GET(final_url, httr::add_headers(

      `method`= "GET",
      `accept-encoding` = "gzip, deflate, br",
      `referrer` = "https://www.tiktok.com/tag/jakefromstate?lang=en",
      `user-agent` = br$userAgent))
  })
  # print(4)


  if(inherits(req, "try-error")){stop("Error happened while requesting")}

  if(!parse){return(req$content)}

  content <- rawToChar(req$content)

  if(content == "") content <- "{}"


  out <- content %>%
    jsonlite::fromJSON()

  return(out)
}
