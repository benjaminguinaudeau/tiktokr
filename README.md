
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tiktokr

<!-- badges: start -->

<!-- badges: end -->

The goal of `tiktokr` is to provide a scraper for the video-sharing
social networking service [TikTok](http://tiktok.com/). Mostly inspired
by this Python module:
[davidteather/TikTok-Api](https://github.com/davidteather/TikTok-Api).
You will need Python 3.6 or higher to use `tiktokr`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("benjaminguinaudeau/tiktokr")
```

Load library

``` r
library(tiktokr)
## basic example code
```

Install necessary Python libraries

``` r
install_tiktokr()
```

## Example

This is a basic example which shows you how to solve a common problem:

Make sure to use your preferred Python installation

``` r
library(reticulate)

use_python(py_config()$python)
```

Initialize `tiktokr`

``` r
init_tiktokr()
```

### Get TikTok trends

Returns a tibble with trends.

``` r
# Trend
trends <- get_trending(200)
```

### Get TikToks from user name

``` r
# Username
user <- get_username("willsmith")
user_posts <- get_user_post(200, "willsmith")
```

### Get TikToks from hashtag

``` r
hash <- get_hashtag("maincharacter")
hash_post <- get_hashtag_post(100, "maincharacter")

discover_hashtags()
```

### Download TikTok Videos

From Trends:

``` r
trends <- get_trending(200)

trends %>%
  split(1:nrow(.)) %>% 
  purrr::walk(~{download_video(.x$downloadAddr, paste0("video/", .x$id, ".mp4"))})
```

From hashtag:

``` r

hash <- get_hashtag_post(20, "trump2020")


trends %>%
  head(20) 

hash %>%
  split(1:nrow(.)) %>% 
  purrr::walk(~{download_video(.x$downloadAddr, paste0("video/hashtag/", .x$id, ".mp4"))})
```
