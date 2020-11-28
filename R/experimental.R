
#' @export
get_vpn_data <- function(final_url, vpn_host = "", vpn_port = "", cookie = "", time_out = 10){

  if(vpn_host == ""){
    vpn_host <- Sys.getenv("tiktok_vpn_host")
  }

  if(vpn_port == ""){
    vpn_port <- Sys.getenv("tiktok_vpn_port")
  }

  head <- list(
    method = "GET",
    referer = "https://www.tiktok.com/trending?lang=en",
    `user-agent` = Sys.getenv("TIKTOK_UA"),
    cookie = cookie
  )

  data <- list(url = final_url, head = head)

  req <- try({
    httr::POST(glue::glue("http://{vpn_host}:{vpn_port}/get"), body = data,  encode = "json", timeout = httr::timeout(time_out))
  })

  return(req)
}

#' @export
get_new_verify <- function(){
  chars <- stringr::str_extract_all("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", ".")[[1]]
  title <- base36encode(lubridate::now())

  ending <- 1:36 %>%
    purrr::imap_chr(~{
      if(.y == 1) return("0")
      if(.y %in% c(8, 13, 18, 23)) return("_")
      if(.y %in% c(14)) return("4")
      return(sample(chars, 1))
    }) %>%
    paste(collapse = "")

  paste0("verify_", title, ending)
}

#' @export
base36encode <- function(number){
  alphabet <- stringr::str_extract_all('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', ".")[[1]]

  output <- c()
  number <- round(as.numeric(number), 0)*1000

  if(number > 0 & number < length(alphabet)){
    output <- alphabet[i]
  }

  while(number != 0){
    i <- round(number, 0) %% 36
    number <- number %/% 36
    output <- c(alphabet[i], output)
  }

  return(paste0(output, collapse = ""))
}
