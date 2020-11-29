#  ua = "Mozilla%252F5.0%2B%28iPhone%253B%2BCPU%2BiPhone%2BOS%2B12_2%2Blike%2BMac%2BOS%2BX%29%2BAppleWebKit%252F605.1.15%2B%28KHTML%2C%2Blike%2BGecko%29%2BVersion%252F13.0%2BMobile%252F15E148%2BSafari%252F604.1"
#  browser(url = "test", ua = ua)

import asyncio
import pyppeteer
import random
import time
import json
import string
import atexit
import requests
import logging




class browser:
  def __init__(self, url, ua, debug=False):
    self.url = url
    self.debug = debug
    self.referrer = "https://www.tiktok.com/"
    self.userAgent = ua #"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.0 Safari/537.36)"
    self.args = [
      "--no-sandbox",
      "--disable-setuid-sandbox",
      "--disable-infobars",
      "--window-position=0,0",
      "--ignore-certifcate-errors",
      "--ignore-certifcate-errors-spki-list",
      "--user-agent=" + self.userAgent
    ]
    self.options = {
      'args': self.args,
      'headless': True,
      'ignoreHTTPSErrors': True,
      'userDataDir': "./tmp",
      'handleSIGINT': False,
      'handleSIGTERM': False,
      'handleSIGHUP': False
    }
    loop = asyncio.new_event_loop()
    loop.run_until_complete(self.start())
  async def start(self):
    self.browser = await pyppeteer.launch(self.options)
    self.page = await self.browser.newPage()
    await self.page.evaluateOnNewDocument("""() => {delete navigator.__proto__.webdriver;}""")
    await stealth(self.page)
    await self.page.goto("about:blank", {
      'waitUntil': "load"
    })
    await self.page.setUserAgent(self.userAgent)
    await self.page.evaluate("() => { " + self.__get_js() + " }")
    self.signature = await self.page.evaluate('''() => {
              var urls = ["''' + self.url + '''"]
              var token = urls.map(x => window.byted_acrawler.sign({ url: x}))

              // var t = {}
              // webpackJsonp.filter(x => typeof x[1]['duD4'] === "function")[0][1].duD4(null, t)
              // var token = urls.map(x => t.sign({ url: x}))

              return token;
              }''')
    self.data = await self.page.content()
    await self.browser.close()
    await self.browser.close()
    self.browser.process.communicate()
  def __get_js(self):
    return requests.get("https://sf16-muse-va.ibytedtos.com/obj/rc-web-sdk-gcs/acrawler.js").text
