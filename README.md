
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tiktokr <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/benjaminguinaudeau/tiktokr/branch/master/graph/badge.svg)](https://codecov.io/gh/benjaminguinaudeau/tiktokr?branch=master)
[![Travis build
status](https://travis-ci.com/benjaminguinaudeau/tiktokr.svg?branch=master)](https://travis-ci.com/benjaminguinaudeau/tiktokr)
<!-- badges: end -->

The goal of `tiktokr` is to provide a scraper for the video-sharing
social networking service [TikTok](http://tiktok.com/).

While writing this library, we were broadly inspired by the Python
module
[davidteather/TikTok-Api](https://github.com/davidteather/TikTok-Api).
You will need Python 3.6 or Docker to use `tiktokr`. If you want to use
Docker check out the guide for that
[here](https://github.com/benjaminguinaudeau/tiktokr#authentification).

*Many thanks go to [Vivien Fabry](https://twitter.com/ViviFabrien) for
creating the hexagon logo.*

**Overview**

  - [Installation](https://github.com/benjaminguinaudeau/tiktokr#installation)
  - [Authentification](https://github.com/benjaminguinaudeau/tiktokr#authentification)
  - [Using tiktokr with
    Docker](https://github.com/benjaminguinaudeau/tiktokr#authentification)
  - [Examples](https://github.com/benjaminguinaudeau/tiktokr#examples)

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

[<img src="https://github.com/benjaminguinaudeau/tiktokr/raw/master/data/preview.png" width="100%">](https://youtu.be/kYMV2ugxacs)

The `tk_auth` function will save cookies (and user agent) as environment
variable to your `.Renviron` file. You need to only run this once to use
`tiktokr` or whenever you want to update your cookie/user agent.

``` r

tk_auth(cookie = "<paste here the output from document.cookie>")
```

## Using `tiktokr` with Docker

TikTok requires API queries to be identified with a unique hash. To get
this hash, `tiktokr` runs a `puppeteer-chrome` session in the
background. Apparently `puppeteer` sometimes causes issues on some
operating systems, so we also created a Docker image, that can be run on
any computer with Docker installed.

To find out, if you are experiencing puppeteer problems run:

``` r

library(tiktokr)
Sys.setenv("TIKTOK_DOCKER" = "")
tk_auth(cookie = "<your_cookie_here>")
tk_init()
out <- get_signature("test")

if(stringr::str_length(get_docker_signature("")) > 16){
  message("Puppeteer works well on you computer")
} else {
  message("Puppeteer does not work, please consider using Docker")
}
```

If you experience a problem try to install Docker as outlined in the
steps below.

### Installing Docker

If you have either a
[Mac](https://docs.docker.com/docker-for-mac/install/), Linux (for
example [Ubuntu](https://docs.docker.com/engine/install/ubuntu/)) or
[Windows 10 Professional / Education /
Enterprise](https://docs.docker.com/docker-for-windows/install/)
operating system, simply install Docker (click on respective
hyperlinks).

If you only have Windows 10 Home the installation of Docker requires
more steps.

1.  Follow the steps to [install Windows Subsystem for
    Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10)

2.  Follow the steps to [install Docker on Windows
    Home](https://docs.docker.com/docker-for-windows/install-windows-home/)

### Initialize Docker

To run `tiktokr` with Docker, simply run `tk_auth()` with `docker =
TRUE` which sets the necessary environment variable.

``` r
tk_auth(docker = T)
```

Now run `tk_init()` to set up the Docker container.

``` r
tk_init()
```

You can check whether your Docker container is working correctly by
running the following code:

``` r

if(stringr::str_length(get_docker_signature("")) > 16){
  message("Signature successfull, your docker container is working.")
} else {
  message("Unable to get the signature")
}
```

Now try running the examples below.

## Examples

For every session involving `tiktokr`, you will need to initialize the
package with `tk_init()`. Once it is initialized you can run as many
queries as you want.

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

### Get TikToks from music id

Note: Hashtags query only provides 2k hits, which are not drawn randomly
or based on the most recent post date but rather **some mix of recent
and popular** TikToks.

``` r

user_posts <- tk_posts(scope = "user", query = "willsmith", n = 50)
music_post <- tk_posts(scope = "music", query = user_posts$music_id[1], n = 100)
```

### Download TikTok Videos

With `tk_dwnl` you can download videos from TikTok.

From Trends:

``` r
# fs::dir_create("video")
trends <- tk_posts(scope = "trends", n = 5)

trends %>%
  split(1:nrow(.)) %>%
  purrr::walk(~{tk_dwnl(.x$video_downloadAddr, paste0("video/", .x$id, ".mp4"))})
# fs::dir_delete("video")
```
