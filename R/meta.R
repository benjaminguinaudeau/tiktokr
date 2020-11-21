#' tk_init
#' @description Intitalize puppeeter browser in the reticulate session
#' @export
tk_init <- function(){
  require(reticulate)
  reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/stealth.py")
  reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/browser.py")
}


#' @title Set up a TikTok cookie
#' @description This function will add a TikTok cookie to your
#'   \code{.Renviron} file so it can be called securely without being stored in
#'   your code. After you have installed your key, it can be called any time by
#'   typing \code{Sys.getenv("TIKTOK_COOKIE")}. If you do not have an \code{.Renviron} file, the
#'   function will create one for you.
#' @param cookie The TikTok cookie as string. Find out here how to get a TikTok cookie: \url{https://www.youtube.com/watch?v=kYMV2ugxacs&feature=youtu.be}
#' @export tk_auth
#' @examples
#'
#' \dontrun{
#' # Put in your cookie:
#' tk_auth(cookie = "abcd012345678901234567890123456789")
#' # Restart R for changes to take effect or load your environment so you can use the cookie without
#' # restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("TIKTOK_COOKIE")
#' }
tk_auth <- function(cookie, ua, id_cookie) {

  if(missing(cookie) & Sys.getenv("TIKTOK_COOKIE") != ""){
    message("A tiktok cookie is stored in .Renviron")
  }
  if (missing(cookie) & Sys.getenv("TIKTOK_COOKIE") == "") {
    cookie <- readline(prompt = "Please enter your TikToK cookie")

    if(length(cookie)==0){
      stop("No Cookie specified. Please try again.")
    }
  }
  if(Sys.getenv("TIKTOK_COOKIE") != cookie){
    message("Tiktok cookie was updated")
    set_renv("TIKTOK_COOKIE" = cookie)
  }

  if(missing(ua)) ua <- Sys.getenv("TIKTOK_UA")
  if(Sys.getenv("TIKTOK_UA") != ""){
    message("A user agent is stored in .Renviron")
  }
  if (missing(ua) & Sys.getenv("TIKTOK_UA") == "") {
    ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36"
  }
  if(missing(ua)) ua <- Sys.getenv("TIKTOK_UA")
  if(Sys.getenv("TIKTOK_UA") != ua){
    message("User agent was updated")
    set_renv("TIKTOK_UA" = ua)
  }

  if(missing(id_cookie)) id_cookie <- Sys.getenv("TIKTOK_COOKIE")
  set_renv("TIKTOK_ID_COOKIE" = id_cookie)

}

#' tk_install
#' @description Install needed python libraries
#' @export
tk_install <- function(){
  reticulate::py_install(c("pyppeteer", "pyppeteer_stealth", "asyncio", "requests"), pip = T)
}
