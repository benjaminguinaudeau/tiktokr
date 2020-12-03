
# devtools::load_all()
# system("docker stop tiktoksignature", intern = T) ; system("docker build ../tiktok_signature -t tiktoksignature")
# system("docker stop tiktoksignature", intern = T) ; system("docker build ../tiktok_signature -t tiktoksignature")
#tk_init_docker()


if (Sys.getenv("USER") != "travis") {


  ## Authentification tests ----
  context("Authentification tests")

  test_that("tk_init and signature", {

    expect_error(tk_info(scope = "user", query = "willsmith"), "Tiktokr was not initialized. Please run tk_init()")
    Sys.setenv("TIKTOK_DOCKER" = "")
    tk_init()
    expect_gt(stringr::str_length(get_signature("")), 16)
    Sys.setenv("TIKTOK_DOCKER" = "TRUE")
    tk_init()
    Sys.setenv("TIKTOK_DOCKER" = "")

  })



  tk_auth(cookie = Sys.getenv("TIKTOK_COOKIE_TEST"), id_cookie = Sys.getenv("TIKTOK_ID_COOKIE_TEST"),
          ua = 'Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.0 Mobile/15E148 Safari/604.1')


  # context("Parsing")

  ## Signature ----
  context("Signature")

  test_that("signature Puppetteer process", {

    Sys.setenv("TIKTOK_DOCKER" = "TRUE")
    sig <- get_signature("http://tiktok.com/trends")
    expect_true(stringr::str_detect(sig, "&_signature="))
    expect_true(stringr::str_detect(sig, "(?<=&_signature=).{10}"))

  })

  test_that("signature docker process", {

    Sys.setenv("TIKTOK_DOCKER" = "TRUE")
    sig <- get_signature("http://tiktok.com/trends")
    expect_true(stringr::str_detect(sig, "&_signature="))
    expect_true(stringr::str_detect(sig, "(?<=&_signature=).{10}"))


    # What happens if container is off

    if(length(system("docker ps -a -f 'name=tiktoksignature'", intern = T)) == 2){
      system("docker stop tiktoksignature", intern = T)
    }

    expect_message(get_signature("http://tiktok.com/trends"), "Container was stopped. Starting container")
    expect_silent(get_signature("http://tiktok.com/trends"))
    Sys.setenv("TIKTOK_DOCKER" = "")
  })

  ## Captcha ----

  context("Captcha")

  test_that("captcha trigger", {
    # prev_cookie <- Sys.getenv("TIKTOK_COOKIE")
    # Sys.setenv("TIKTOK_COOKIE" = "wrong_verify")
    # testthat::skip("Skiping captcha trigger")
    # expect_error(tk_posts(scope = "user", query = "willsmith", n = 50), "Captcha required. Please update the cookie file.")
    # Sys.setenv("TIKTOK_COOKIE" = prev_cookie)
  })

  ## Download ----
  context("Download")

  ## User ----
  context("User")

  test_that("user info", {

    # Good User Info

    user <- tk_info(scope = "user", query = "willsmith")
    expect_equal(nrow(user), 1)
    expect_true("user.uniqueId" %in% names(user))
    expect_true("user.privateAccount" %in% names(user))
    expect_true("stats.heart" %in% names(user))
    expect_gt(ncol(user), 24)

    # Wrong user info

    user <- "idontexist_at_all"
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_info(scope = "user", query = user), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_info(scope = "user", query = user)$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    expect_false(tk_info(scope = "user", query = user)$found)
  })

  test_that("user post", {

    # Good User ID

    user_post <- tk_posts(scope = "user", query = "willsmith", n = 40)
    expect_gt(nrow(user_post), 2)
    expect_true("desc" %in% names(user_post))
    expect_gt(ncol(user_post), 90)
    user_post_ <- tk_posts(scope = "user", query = "willsmith", n = 100)
    expect_gt(nrow(dplyr::anti_join(user_post_, user_post, by = "id")), 0)

    # Docker User

    Sys.setenv("TIKTOK_DOCKER" = "TRUE")
    user_post <- tk_posts(scope = "user", query = "willsmith", n = 40)
    expect_gt(nrow(user_post), 2)
    expect_true("desc" %in% names(user_post))
    expect_gt(ncol(user_post), 90)
    user_post_ <- tk_posts(scope = "user", query = "willsmith", n = 100)
    expect_gt(nrow(dplyr::anti_join(user_post_, user_post, by = "id")), 0)
    Sys.setenv("TIKTOK_DOCKER" = "")

    # Wrong User ID

    user <- "idontexist_at_all"
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_posts(scope = "user", query = user, n = 40), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_posts(scope = "user", query = user, n = 40)$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    expect_false(tk_info(scope = "user", query = user)$found)

    # Zero video

    expect_equal(nrow(tk_posts(scope = "user", query = "lepetitrober")), 1)

  })

  ## Hashtag ----
  context("Hashtag")

  test_that("hashtag info", {

    # Good Hashtag Info

    hashtag <- tk_info(scope = "hashtag", query = "maincharacter")
    expect_equal(nrow(hashtag), 1)
    expect_true("challengeInfo.challenge.id" %in% names(hashtag))
    expect_equal(ncol(hashtag), 17)

    # Wrong Hashtag Info

    hashtag <- "iamnohashtag"
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_info(scope = "hashtag", query = hashtag), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_info(scope = "hashtag", query = hashtag)$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    expect_false(tk_info(scope = "hashtag", query = hashtag)$found)
  })

  test_that("hashtag post", {

    # Good Hashtag

    hashtag <- tk_posts(scope = "hashtag", query = "maincharacter", n = 25)
    expect_gt(nrow(hashtag), 20)
    expect_true("desc" %in% names(hashtag))
    expect_gt(ncol(hashtag), 60)
    hashtag_ <- tk_posts(scope = "hashtag", query = "maincharacter", n = 60)
    expect_gt(nrow(dplyr::anti_join(hashtag_, hashtag, by = "id")), 0)

    # Hashtag Docker

    Sys.setenv("TIKTOK_DOCKER" = "TRUE")
    hashtag <- tk_posts(scope = "hashtag", query = "maincharacter", n = 25)
    expect_gt(nrow(hashtag), 20)
    expect_true("desc" %in% names(hashtag))
    expect_gt(ncol(hashtag), 60)
    hashtag_ <- tk_posts(scope = "hashtag", query = "maincharacter", n = 60)
    expect_gt(nrow(dplyr::anti_join(hashtag_, hashtag, by = "id")), 0)
    Sys.setenv("TIKTOK_DOCKER" = "")

    # Wrong Hashtag

    hashtag <- "iamnohashtag"
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_posts(scope = "hashtag", query = hashtag, n = 40), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_posts(scope = "hashtag", query = hashtag, n = 40)$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    expect_false(tk_posts(scope = "hashtag", query = hashtag)$found)

  })


  ## Music ----
  context("Music")

  test_that("music info", {

    # Good music ID

    music <- tk_info(scope = "music", query = "6782187241935505410")
    expect_gt(nrow(music), 0)
    expect_true("desc" %in% names(music))
    expect_gt(ncol(music), 60)

    # Wrong Music ID

    music <- "67821871935505411" # No existing music
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_info(scope = "music", query = music), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_info(scope = "music", query = music)$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    expect_false(tk_info(scope = "music", query = music)$found)
  })

  test_that("music post", {

    # Good Music ID

    music <- tk_posts(scope = "music", query = "6782187241935505410", n = 25)
    expect_gt(nrow(music), 20)
    expect_true("desc" %in% names(music))
    expect_gt(ncol(music), 60)
    music_ <- tk_posts(scope = "music", query = "6782187241935505410", n = 60)
    expect_gt(nrow(dplyr::anti_join(music_, music, by = "id")), 0)

    # Docker Signature

    Sys.setenv("TIKTOK_DOCKER" = "TRUE")
    music <- tk_posts(scope = "music", query = "6782187241935505410", n = 25)
    expect_gt(nrow(music), 20)
    expect_true("desc" %in% names(music))
    expect_gt(ncol(music), 60)
    music_ <- tk_posts(scope = "music", query = "6782187241935505410", n = 60)
    expect_gt(nrow(dplyr::anti_join(music_, music, by = "id")), 0)
    Sys.setenv("TIKTOK_DOCKER" = "")


    # Wrong Music ID

    music <- "67821871935505411" # No existing music
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_posts(scope = "music", query = music), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_info(scope = "music", query = music)$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    expect_false(tk_posts(scope = "music", query = music)$found)

  })


  ## Post ----
  context("Post")

  test_that("info post", {

    # Work
    post <- tk_info(scope = "post", query = "6826115812009495814")
    expect_equal(nrow(post), 1)
    expect_true("itemInfo.shareMeta.desc" %in% names(post))
    expect_gt(ncol(post), 85)

    # Wrong Post ID

    post <- 6826115812009495 # don't 'exist
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_info(scope = "post", query = post), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_info(scope = "post", query = post)$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    expect_false(tk_info(scope = "post", query = post)$found)
  })

  ## Trend ----
  context("Trend")

  test_that("post trend", {
    trends <- tk_posts(scope = "trends", n = 25)
    expect_gt(nrow(trends), 1)
    expect_lt(nrow(trends), 50)
    expect_true("desc" %in% names(trends))
    expect_gt(ncol(trends), 60)
    # trends_ <- tk_posts(scope = "trends", n = 50)
    # expect_gt(nrow(dplyr::anti_join(trends_, trends, by = "id")), 0)

    post <- 6826115812009495 # don't 'exist
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_posts(scope = "trends", n = 25), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_gt(nrow(tk_posts(scope = "trends", n = 25)), 25)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
  })

  ## Comment ----
  context("Comment")

  test_that("comment", {
    comment <- tk_comment(post_id = "6742968498936433925", verbose = T)
    expect_gt(nrow(comment), 2)
    expect_true("cid" %in% names(comment))
    expect_gt(ncol(comment), 35)

    comment <- 67429688936433925 #Wrong comment id
    Sys.setenv("TIKTOK_UA" = "")
    expect_error(tk_comment(post_id = "comment"), "No user agent was detected. Please register a user agent using tk_auth()")
    Sys.setenv("TIKTOK_UA" = "a")
    expect_false(tk_comment(post_id = "comment")$found)
    Sys.setenv("TIKTOK_UA" = Sys.getenv("TIKTOK_UA_TEST"))
    Sys.setenv("TIKTOK_ID_COOKIE" = "")
    expect_error(tk_comment(post_id = "comment"), "Please update your logged in cookie.")
    Sys.setenv("TIKTOK_ID_COOKIE" = "a")
    expect_error(tk_comment(post_id = "comment"), "Please update your logged in cookie.")
    Sys.setenv("TIKTOK_ID_COOKIE" = Sys.getenv("TIKTOK_ID_COOKIE_TEST"))
  })




}



