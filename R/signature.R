#' @export
get_signature <- function(urls){

  if(Sys.getenv("TIKTOK_DOCKER") == "TRUE"){
    out <- urls %>% purrr::map_chr(get_docker_signature)
  } else {
    out <- get_puppeteer_signature(urls)
  }

  paste0(urls, "&_signature=", out)
}

#' @export
get_puppeteer_signature <- function(urls){

  if(!exists("py")) stop("Tiktokr was not initialized. Please run tk_init()")
  if(!"browser" %in% names(py)) stop("Tiktokr was not initialized. Please run tk_init()")
  ua <- Sys.getenv("TIKTOK_UA")
  if(ua == "") stop("No user agent was detected. Please register a user agent using tk_auth()")


  url <- paste(urls, collapse = '", "')

  tries <- 0
  br <- tryCatch(py$browser(url, ua), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
  while(tries < 3 & inherits(br, "try-error")){
    br <- tryCatch(py$browser(url, ua), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
    tries <- tries + 1
  }

  return(br$signature)
}

#' @export
get_docker_signature <- function(url, port = 32768){

  ua <- Sys.getenv("TIKTOK_UA")
  if(ua == "") stop("No user agent was detected. Please register a user agent using tk_auth()")

  if(!any(stringr::str_detect(system("docker ps", intern = T), "tiktoksignature$"))){
    message("Container was stopped. Starting container")
    system("docker start tiktoksignature", intern = T)
    Sys.sleep(6)
  }

  cmd <- list(url = url, ua = Sys.getenv("TIKTOK_UA"))
  req <- try(httr::POST(url = glue::glue("http://localhost:{port}/"),  body  = cmd, encode = "json"))
  if(inherits(req, "try-error")){

    system("docker stop tiktoksignature", intern = T)
    system("docker start tiktoksignature", intern = T)
    req <- try(httr::POST(url = glue::glue("http://localshost:{port}/"),  body  = cmd, encode = "json"))
  }
  jsonlite::fromJSON(rawToChar(req$content))
}

# get_docker_signature <- function(url){
#
#   if(length(system("docker ps -a -f 'name=tiktoksignature'", intern = T)) == 1){
#     message("Creating container `tiktoksignature`... This might take a couple of minutes.")
#     system("docker run -dt --rm --name tiktoksignature tiktoksignature:latest", intern = T)
#   }
#
#   res <- system(glue::glue("docker exec -t tiktoksignature /bin/python3 /usr/app/sign.py '{url}' '{get_current_verify()}'"), intern = T)
#   paste0(url, "&_signature=", jsonlite::fromJSON(res)[[3]])
#
# }

#' @export
get_api_signature <- function(url, port = 8080, time_out = 10){
  res <- httr::POST(url  = glue::glue("http://localhost:{port}/signature"), body = url, timeout = httr::timeout(time_out))
  jsonlite::fromJSON(rawToChar(res$content))$signature
}

