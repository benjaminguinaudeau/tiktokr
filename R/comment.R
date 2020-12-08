#' @export
tk_comment <- function(post_id, n = 10000, verbose = T, vpn = F){

  response <- tibble::tibble()
  count <- sample(30:50, 1)
  max_cursor <- 1000
  has_more <- T

  while(has_more){
    cursor <- seq(max_cursor - 1000, max_cursor - 1, 50)

    urls <- get_url("comment", query_1 = post_id, n = count, cursor = cursor)
    fins <- get_signature(urls)

    index <- 1
    while(has_more & index <= 20){
      res <- get_data(url = fins[index], vpn = vpn, cookie = Sys.getenv("TIKTOK_ID_COOKIE"))

      # if(res$code == "8"){
      #     stop("Please update your logged in cookie.")
      # }

      data <- try({
        res %>%
          .[["comments"]] %>%
          parse_json_structure
      }, silent = T)

      if(inherits(data, "try-error")){ has_more <- F ; break }
      if(length(data) == 0){ has_more <- F ; break }

      response <- dplyr::bind_rows(response, data) %>%
        dplyr::distinct(cid, .keep_all = T)

      if(nrow(response) > n){
        has_more <- F
      } else {
        has_more <- res$has_more
      }
      index <- index + 1
    }
    max_cursor <- max_cursor + 1000
  }

  if(verbose){
    cli::cli_alert_success("[{Sys.time()}] c-{post_id} ({nrow(response)})")
  }

  if(nrow(response) == 0){
    return(tibble::tibble(query = post_id, found = F))
  }

  return(response)

}

# Need to be maintained
#' @export
tk_reply <- function(comment_id, post_id, id_cookie, verbose = T, time_out = 10){

  response <- tibble::tibble()
  count <- sample(50:100, 1)
  max_cursor <- 1000
  has_more <- T

  while(has_more){
    cursor <- seq(max_cursor - 1000, max_cursor - 1, 50)
    cat("\rCursor: ", max_cursor, "  Comments: ", nrow(response))
    urls <- get_url("reply", query_1 = comment_id, query_2 = post_id, n = count, cursor = cursor)
    fins <- get_signature(urls)

    index <- 1
    while(has_more & index <= 20){
      res <- httr::GET(fins[index],
                       httr::add_headers(.headers = c(
                         referer = "https://www.tiktok.com/trending/?lang=fr",
                         `user-agent` = ua,
                         cookie = id_cookie)
                       ),
                       timeout = httr::timeout(time_out)) %>%
        .[["content"]] %>%
        rawToChar %>%
        jsonlite::fromJSON()

      data <- res %>%
        .[["comments"]] %>%
        parse_json_structure

      if(length(data) == 0){
        # message("\nReached end of comments or no more comments available.")
        if(verbose){
          cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} ({nrow(response)})")
        }

        return(response)
      }

      response <- dplyr::bind_rows(response, data) %>%
        dplyr::distinct(cid, .keep_all = T)

      has_more <- res$has_more
      index <- index + 1
    }

    max_cursor <- max_cursor + 1000

  }

  if(verbose){
    if(inherits(response, "try-error")){
      cli::cli_alert_danger("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query}")
      return(tibble::tibble())
    } else {
      cli::cli_alert_success("[{Sys.time()}] {stringr::str_extract(scope, '.')}-{query} ({nrow(response)})")
    }
  }

  return(response)

}


