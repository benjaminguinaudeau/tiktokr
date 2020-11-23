
# devtools::load_all()

context("Authentification tests")

test_that("error without init", {
  expect_error(tk_info(scope = "user", query = "willsmith"), "Tiktokr was not initialized. Please run `tk_init()")
})

tk_init()

Sys.unsetenv("TIKTOK_COOKIE")

test_that("info user without cookie", {
  user <- tk_info(scope = "user", query = "willsmith")
  expect_equal(nrow(user), 1)
  expect_true("user.uniqueId" %in% names(user))
  expect_true("user.privateAccount" %in% names(user))
  expect_true("stats.heart" %in% names(user))
  expect_gt(ncol(user), 24)
})

tk_auth(cookie = Sys.getenv("TIKTOK_COOKIE_TEST"), id_cookie = Sys.getenv("TIKTOK_ID_COOKIE_TEST"))

context("Info Endpoints")

test_that("info user", {
  user <- tk_info(scope = "user", query = "willsmith")
  expect_equal(nrow(user), 1)
  expect_true("user.uniqueId" %in% names(user))
  expect_true("user.privateAccount" %in% names(user))
  expect_true("stats.heart" %in% names(user))
  expect_gt(ncol(user), 24)
})

test_that("info hashtag", {
  hashtag <- tk_info(scope = "hashtag", query = "maincharacter")
  expect_equal(nrow(hashtag), 1)
  expect_true("challengeInfo.challenge.id" %in% names(hashtag))
  expect_equal(ncol(hashtag), 15)
})

test_that("info music", {
  music <- tk_info(scope = "music", query = "6782187241935505410")
  expect_gt(nrow(music), 0)
  expect_true("desc" %in% names(music))
  expect_gt(ncol(music), 60)
})

test_that("info post", {
  post <- tk_info(scope = "post", query = "6826115812009495814")
  expect_equal(nrow(post), 1)
  expect_true("itemInfo.shareMeta.desc" %in% names(post))
  expect_gt(ncol(post), 85)
})

context("Post Endpoint")

test_that("post user", {
  user_post <- tk_posts(scope = "user", query = "willsmith", n = 40)
  expect_gt(nrow(user_post), 2)
  expect_true("desc" %in% names(user_post))
  expect_gt(ncol(user_post), 90)
  user_post_ <- tk_posts(scope = "user", query = "willsmith", n = 100)
  expect_gt(nrow(dplyr::anti_join(user_post_, user_post, by = "id")), 0)
})


test_that("post trend", {
  trends <- tk_posts(scope = "trends", n = 25)
  expect_gt(nrow(trends), 1)
  expect_true("desc" %in% names(trends))
  expect_gt(ncol(trends), 60)
  trends_ <- tk_posts(scope = "trends", n = 50)
  expect_gt(nrow(dplyr::anti_join(trends_, trends, by = "id")), 0)
})

test_that("post music", {
  music <- tk_posts(scope = "music", query = "6782187241935505410", n = 25)
  expect_gt(nrow(music), 20)
  expect_true("desc" %in% names(music))
  expect_gt(ncol(music), 60)
  music_ <- tk_posts(scope = "music", query = "6782187241935505410", n = 60)
  expect_gt(nrow(dplyr::anti_join(music_, music, by = "id")), 0)
})

test_that("post hashtag", {
  hashtag <- tk_posts(scope = "hashtag", query = "maincharacter", n = 25)
  expect_gt(nrow(hashtag), 20)
  expect_true("desc" %in% names(hashtag))
  expect_gt(ncol(hashtag), 60)
  hashtag_ <- tk_posts(scope = "hashtag", query = "maincharacter", n = 60)
  expect_gt(nrow(dplyr::anti_join(hashtag_, hashtag, by = "id")), 0)
})

# id_cookie <- Sys.getenv("TIKTOK_ID_COOKIE")

test_that("comment", {
  comment <- tk_comment(post_id = "6829090092984929541", verbose = T)
  expect_gt(nrow(comment), 2)
  expect_true("cid" %in% names(comment))
  expect_gt(ncol(comment), 35)
})

context("Parsing")

context("Signature")

test_that("signature process", {
  sig <- get_signature("http://tiktok.com/trends", port = NULL)

  expect_true(stringr::str_detect(sig, "&_signature="))
  expect_true(stringr::str_detect(sig, "(?<=&_signature=).{10}"))

})

context("Captcha")

test_that("captcha trigger", {
  prev_cookie <- Sys.getenv("TIKTOK_COOKIE")
  Sys.setenv("TIKTOK_COOKIE" = "wrong_verify")
  testthat::skip("Skiping captcha trigger")
  expect_error(tk_posts(scope = "user", query = "willsmith", n = 50), "Captcha required. Please update the cookie file.")
  Sys.setenv("TIKTOK_COOKIE" = prev_cookie)
})

context("Download")
