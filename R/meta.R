#' tk_init
#' @description Intitalize puppeeter browser in the reticulate session
#' @export
tk_init <- function(){
  if(Sys.getenv("TIKTOK_DOCKER") != ""){
    tk_init_docker()
  } else {
    require(reticulate)
    reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/stealth.py")
    reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/browser.py")
  }
}


#' @title Set up TikTok cookie and user agent
#' @description This function will add a TikTok cookie and a user agent to your
#'   \code{.Renviron} file so it can be called securely without being stored in
#'   your code. After you have installed your key, it can be called any time by
#'   typing \code{Sys.getenv("TIKTOK_COOKIE")} or \code{Sys.getenv("TIKTOK_UA")}. If you do not have an \code{.Renviron} file, the
#'   function will create one for you.
#' @param cookie The TikTok cookie as string. Find out here how to get a TikTok cookie: \url{https://www.youtube.com/watch?v=kYMV2ugxacs&feature=youtu.be}
#' @param ua The User Agent, that should be used when making HTTP requests. Default is 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36'
#' @param id_cookie Logged-in cookie used by the comment endpoint (experimental).
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
tk_auth <- function(cookie, ua, id_cookie, docker = F) {

  if(docker){
    if(Sys.getenv("TIKTOK_DOCKER") == ""){
      message("Setting `TIKTOK_DOCKER` as TRUE")
      set_renv("TIKTOK_DOCKER" = T)
    } else {
      message("TIKTOK_DOCKER found. Tiktokr will use a docker container to sign the urls")
    }
  } else {
    if(Sys.getenv("TIKTOK_DOCKER") == "") message("TIKTOK_DOCKER found. Tiktokr will use a docker container to sign the urls")
  }

  ## Tiktok Cookie

  if (missing(cookie)) {
    if(Sys.getenv("TIKTOK_COOKIE") == ""){
      cookie <- readline(prompt = "Please enter your TikToK cookie")
    } else {
      message("Tiktok cookie already specified in `.Renviron`")
      cookie <- Sys.getenv("TIKTOK_COOKIE")
    }
  }

  if(Sys.getenv("TIKTOK_COOKIE") != cookie){
    message("Tiktok cookie was updated")
    set_renv("TIKTOK_COOKIE" = cookie)
  }

  ## User agent

  if (missing(ua)){
    if(Sys.getenv("TIKTOK_UA") == ""){
      message("Using default User Agent")
      ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36"
    } else {
      message("User agent already specified in `.Renviron`")
      ua <- Sys.getenv("TIKTOK_UA")
    }
  }

  if(Sys.getenv("TIKTOK_UA") != ua){
    message("User agent was updated")
    set_renv("TIKTOK_UA" = shape_ua(ua))
  }

  ## ID Cookie

  if(missing(id_cookie)) id_cookie <- Sys.getenv("TIKTOK_ID_COOKIE")
  set_renv("TIKTOK_ID_COOKIE" = id_cookie)

}

#' tk_install
#' @description Install needed python libraries
#' @export
tk_install <- function(){
  reticulate::py_install(c("pyppeteer", "pyppeteer_stealth", "asyncio", "requests"), pip = T)
}
