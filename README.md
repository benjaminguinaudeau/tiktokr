
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tiktokr <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/favstats/tiktokr/branch/master/graph/badge.svg)](https://codecov.io/gh/benjaminguinaudeau/tiktokr?branch=master)
[![Travis build
status](https://travis-ci.com/favstats/tiktokr.svg?branch=master)](https://travis-ci.com/benjaminguinaudeau/tiktokr)
<!-- badges: end -->

The goal of `tiktokr` is to provide a scraper for the video-sharing
social networking service [TikTok](http://tiktok.com/). Mostly inspired
by this Python module:
[davidteather/TikTok-Api](https://github.com/davidteather/TikTok-Api).
You will need Python 3.6 or higher to use `tiktokr`.

*Many thanks go to [Vivien Fabry](https://twitter.com/ViviFabrien) for
creating the hexagon logo.*

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
```

Make sure to use your preferred Python installation

``` r
library(reticulate)

use_python(py_config()$python)
```

Install necessary Python libraries

``` r
tk_install()
```

## Authentification

In November 2020, Tiktok increased its security protocol. They now
frequently show a captcha, which is easily triggered after a few
requests. This can be solved by specifying the cookie parameter. To get
a cookie session:

1.  Open a browser and go to “<http://tiktok.com>”
2.  Scroll down a bit, to ensure, that you don’t get any captcha
3.  Open the javascript console (in Chrome: View \> Developer \>
    Javascript Console)
4.  Run `document.cookie` in the console. Copy the entire output (your
    cookie).
5.  Run `tk_auth()` in R and paste the cookie.

Click on image below for screen recording of how to get your TikTok
cookie:

[<img src="https://github.com/benjaminguinaudeau/tiktokr/raw/master/data/preview.png" width="50%">](https://youtu.be/kYMV2ugxacs)

The `tk_auth` function will save cookies (and user agent) as environment
variable to your `.Renviron` file. You need to only run this once to use
the `tiktokr` or whenever you want to update your cookie/user agent.

``` r
tk_auth(cookie = "<paste here the output from document.cookie>")
```

## Examples

Every time before you run functions you need to initialize `tiktokr`

``` r
tk_init()
```

### Get TikTok trends

Returns a tibble with trends.

``` r
# Trend
trends <- tk_posts(scope = "trends", n = 200)
```

### Get TikToks from User

Note: User query often only provides 2k hits but limit is unclear.
Sample seems to be from most recent to oldest.

``` r
user_posts <- tk_posts(scope = "user", query = "willsmith", n = 50)
```

### Get TikToks from hashtag

Note: Hashtags query only provides 2k hits, which are not drawn randomly
or based on the most recent post date but rather **some mix of recent
and popular** TikToks.

``` r
hash_post <- tk_posts(scope = "hashtag", query = "maincharacter", n = 100)
```

### Download TikTok Videos

With `tk_dl_video` you can download videos from TikTok.

From Trends:

``` r
trends <- tk_posts(scope = "trends", n = 5)

trends %>%
  split(1:nrow(.)) %>% 
  purrr::walk(~{tk_dl_video(.x$downloadAddr, paste0("video/", .x$id, ".mp4"))})
```

From hashtag:

``` r

hash_post <- tk_posts(scope = "hashtag", query = "maincharacter", n = 5)

hash_post %>%
  split(1:nrow(.)) %>% 
  purrr::walk(~{tk_dl_video(.x$downloadAddr, paste0("video/hashtag/", .x$id, ".mp4"))})
```
