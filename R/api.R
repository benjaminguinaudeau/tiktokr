#' @get /echo
function(req){
  data <- as.list(req)
  print(data$HEADERS)

  now <- suppressWarnings(as.numeric(nanotime::nanotime(lubridate::now())))
  save(data, file = here::here(paste0("req/", now, ".Rdata")))
}

