
devtools::load_all()
tk_init()


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

test_that("post user simple", {
  user_post <- tk_posts(scope = "user", query = "willsmith", n = 40)
  expect_gt(nrow(user_post), 1)
  expect_true("desc" %in% names(user_post))
  expect_gt(ncol(user_post), 90)
})

test_that("post user double", {
  user_post <- tk_posts(scope = "user", query = "willsmith", n = 100)
  expect_gt(nrow(user_post), 50)
  expect_true("desc" %in% names(user_post))
  expect_gt(ncol(user_post), 90)
})

test_that("post trend simple", {
  trends <- tk_posts(scope = "trends", n = 25)
  expect_gt(nrow(trends), 1)
  expect_true("desc" %in% names(trends))
  expect_gt(ncol(trends), 60)
})

test_that("post trend double", {
  trends <- tk_posts(scope = "trends", n = 50)
  expect_gt(nrow(trends), 35)
  expect_true("desc" %in% names(trends))
  expect_gt(ncol(trends), 60)
})

test_that("post music simple", {
  music <- tk_posts(scope = "music", query = "6782187241935505410", n = 25)
  expect_gt(nrow(music), 20)
  expect_true("desc" %in% names(music))
  expect_gt(ncol(music), 60)
})

test_that("post hashtag simple", {
  hashtag <- tk_posts(scope = "hashtag", query = "maincharacter", n = 25)
  expect_gt(nrow(hashtag), 20)
  expect_true("desc" %in% names(hashtag))
  expect_gt(ncol(hashtag), 60)
})

context("Parsing")

context("Signature")

test_that("signature process", {
  sig <- get_signature("http://tiktok.com/trends", ua = default_ua, port = NULL)

  expect_true(stringr::str_detect(sig, "&_signature="))
  expect_true(stringr::str_detect(sig, "(?<=&_signature=).{10}"))

})

