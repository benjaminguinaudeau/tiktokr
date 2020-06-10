#' @export
get_comment <- function(post_id, ua, verify = "", id_cookie = "", port = NULL){

  response <- tibble::tibble()
  count <- sample(50:100, 1)
  max_cursor <- 1000
  has_more <- T

  while(has_more){
    cursor <- seq(max_cursor - 1000, max_cursor - 1, 50)
    cat("\rCursor: ", max_cursor, "  Comments: ", nrow(response))
    urls <- glue::glue("https://www.tiktok.com/api/comment/list/?aweme_id={post_id}&cursor={cursor}&count={count}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    fins <- get_signature(urls, ua, port = port)
    # fin <- get_signature(url, ua)


    index <- 1
    while(has_more & index <= 20){
      res <- httr::GET(fins[index],
                       httr::add_headers(.headers = c(
                         referer = "https://www.tiktok.com/trending/?lang=fr",
                         `user-agent` = ua,
                         cookie = id_cookie)
                       )) %>%
        .[["content"]] %>%
        rawToChar %>%
        jsonlite::fromJSON()



      data <- try({
        res %>%
        .[["comments"]] %>%
        parse_json_structure
      })

      if(inherits(data, "try-error")){
        return(response)
      }

      if(length(data) == 0){
        message("\nReached end of comments or no more comments available.")
        return(response)
      }

      response <- dplyr::bind_rows(response, data) %>%
        dplyr::distinct(cid, .keep_all = T)

      has_more <- res$has_more
      index <- index + 1
    }
    max_cursor <- max_cursor + 1000
  }

  return(response)

}

#' @export
get_reply <- function(comment_id, post_id, ua, verify, id_cookie, port = NULL){

  response <- tibble::tibble()
  count <- sample(50:100, 1)
  max_cursor <- 1000
  has_more <- T

  while(has_more){
    cursor <- seq(max_cursor - 1000, max_cursor - 1, 50)
    cat("\rCursor: ", max_cursor, "  Comments: ", nrow(response))
    urls <- glue::glue("https://www.tiktok.com/api/comment/list/reply/?comment_id={comment_id}&item_id={post_id}&cursor={cursor}&count={count}&aid=1988&app_language=fr&device_platform=web_pc&current_region=CA&fromWeb=1&channel_id=5&verifyFp={verify}")
    fins <- get_signature(urls, ua, port = port)
    # fin <- get_signature(urls[1], ua)

    index <- 1

    while(has_more & index <= 20){
      res <- httr::GET(fins[index],
                       httr::add_headers(.headers = c(
                         referer = "https://www.tiktok.com/trending/?lang=fr",
                         `user-agent` = ua,
                         cookie = id_cookie)
                       )) %>%
        .[["content"]] %>%
        rawToChar %>%
        jsonlite::fromJSON()

      data <- res %>%
        .[["comments"]] %>%
        parse_json_structure

      if(length(data) == 0){
        message("\nReached end of comments or no more comments available.")
        return(response)
      }

      response <- dplyr::bind_rows(response, data) %>%
        dplyr::distinct(cid, .keep_all = T)

      has_more <- res$has_more
      index <- index + 1
    }

    max_cursor <- max_cursor + 1000

  }

  return(response)

}


