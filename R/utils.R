#' get_url
#' @description handle the api url based on a given type
#' @export

get_url <- function(type, count = NULL,
                    username = NULL, user_id = NULL, sec_uid = NULL,
                    hashtag = NULL, hash_id = NULL,
                    music_id = NULL,
                    max = 0, min =0){

  switch(
    type,
    "trending" = {
      glue::glue("https://m.tiktok.com/api/item_list/?count={count}&id=1&type=5&secUid=&maxCursor={max}&minCursor={min}&sourceType=12&appId=1233&verifyFp=")
    },
    "user_post" = {
      glue::glue("https://m.tiktok.com/api/item_list/?count={count}&id={user_id}&type=1&secUid={sec_uid}&maxCursor={max}&minCursor={min}&sourceType=8&appId=1233&region=US&language=en&verifyFp=")
    },
    "username" = {
      glue::glue("https://m.tiktok.com/api/user/detail/?uniqueId={username}&language=en&verifyFp=")
    },
    "hashtag" = {
      glue::glue("https://m.tiktok.com/api/challenge/detail/?verifyFP=&challengeName={hashtag}&language=en")
    },
    "hashtag_post" = {
      glue::glue("https://m.tiktok.com/share/item/list?secUid=&id={hash_id}&type=3&count={count}&minCursor={min}&maxCursor={max}&shareUid=&lang=en&verifyFp=")
    },
    "discover_hash" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount={count}&scene=0&verifyFp=")
    },
    "music" = {
      glue::glue("https://m.tiktok.com/api/music/detail/?musicId={music_id}&language=en&verifyFp=")
    },
    "music_post" = {
      glue::glue("https://m.tiktok.com/share/item/list?secUid=&id={music_id}&type=4&count={count}&minCursor={min}&maxCursor={max}&shareUid=&lang=en&verifyFp=")
    },
    "discover_music" = {
      glue::glue("https://m.tiktok.com/node/share/discover?noUser=1&userCount=30&scene=0&verifyFp=")
    }
  )
}

#' parse_json_structure
#' @description parse nested data frame in json responses
#' @export

parse_json_structure <- function(x){
  x %>%
    dplyr::select_if(is.data.frame) %>%
    purrr::map_dfc(~{
      if(!any(purrr::map_lgl(.x, is.data.frame))){return(.x)}
      parse_json_structure(.x)
    }) %>%
    dplyr::bind_cols(dplyr::select_if(x, ~!is.data.frame(.x)))
}

#' init_tiktokr
#' @description Intitalize puppeeter browser in the reticulate session
#' @export
init_tiktokr <- function(){
  readr::write_lines(script, "tiktokr.py")
  reticulate::source_python("tiktokr.py")
}

#' download_video
#' @description Function that enable to download tiktoks
#' @export
download_video <- function(url, path){
  raw_video <- get_data(url, parse = F)
  writeBin(raw_video, path)
}

# script <- readLines("browser.py")
# usethis::use_data(script, overwrite = T)
