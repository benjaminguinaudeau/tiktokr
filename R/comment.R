#' @export
tk_comment <- function(post_id, ua, verify = "", id_cookie = "", port = NULL){

  response <- tibble::tibble()
  count <- sample(50:100, 1)
  max_cursor <- 1000
  has_more <- T

  while(has_more){
    cursor <- seq(max_cursor - 1000, max_cursor - 1, 50)
    cat("\rCursor: ", max_cursor, "  Comments: ", nrow(response))

    urls <- get_url("comment", post_id = post_id, n = count, cursor = cursor, verify = verify)
    fins <- get_signature(urls, ua, port = port)

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
tk_reply <- function(comment_id, post_id, ua, verify, id_cookie, port = NULL){

  response <- tibble::tibble()
  count <- sample(50:100, 1)
  max_cursor <- 1000
  has_more <- T

  while(has_more){
    cursor <- seq(max_cursor - 1000, max_cursor - 1, 50)
    cat("\rCursor: ", max_cursor, "  Comments: ", nrow(response))
    urls <- get_url("reply", comment_id = comment_id, post_id = post_id, n = count, cursor = cursor, verify = verify)
    fins <- get_signature(urls, ua, port = port)

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


