
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tiktokr

<!-- badges: start -->

<!-- badges: end -->

The goal of tiktokr is to provide a scraper of tiktok. Mosty inspired by
[davidteather/TikTok-Api](https://github.com/davidteather/TikTok-Api)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("benjaminguinaudeau/tiktokr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tiktokr)
## basic example code
```

``` r
reticulate::use_python("/usr/local/bin/python3")
library(reticulate)
devtools::load_all()
#> Loading tiktokr

# reticulate::source_python("browser.py")
init_tiktokr()
```

``` r

# Trend
trends <- get_trending(200)

# Username
user <- get_username("willsmith")
user_posts <- get_user_post(200, "willsmith")


hash <- get_hashtag("maincharacter")
hash_post <- get_hashtag_post(100, "maincharacter")

discover_hashtags(100)
```

``` r
trends <- get_trending(10)

trends %>%
  split(1:nrow(.)) %>% 
  purrr::walk(~{download_video(.x$downloadAddr, paste0("video/", .x$id, ".mp4"))})
```

``` r

hash <- get_hashtag("maincharacter")

trends %>%
  split(1:nrow(.)) %>% 
  purrr::walk(~{download_video(.x$downloadAddr, paste0("video/", .x$id, ".mp4"))})
```
