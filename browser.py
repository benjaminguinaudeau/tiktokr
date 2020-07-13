import asyncio
import pyppeteer
import random
# from pyppeteer_stealth import stealth

import time
import json
import string
import atexit
import requests


class browser:
    def __init__(self, url, api_url = None, language='en', ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A372 Safari/604.1"):
        self.url = url
        self.userAgent = ua
        self.api_url = api_url
        self.referrer = "https://www.tiktok.com/"
        self.language = language
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
        try:
            self.browser = await pyppeteer.launch(self.options)
            self.page = await self.browser.newPage()
    
            await self.page.evaluateOnNewDocument("""() => {
            delete navigator.__proto__.webdriver;
                }""")
    
            await stealth(self.page)
    
            # await self.page.emulate({'viewport': {
            #     'width': random.randint(320, 1920),
            #     'height': random.randint(320, 1920),
            #     'deviceScaleFactor': random.randint(1, 3),
            #     'isMobile': random.random() > 0.5,
            #     'hasTouch': random.random() > 0.5
            # }})
    
            await self.page.setUserAgent(self.userAgent)
            await self.page.goto("https://www.bing.com/")
            
            self.verifyFp = None
            
            if self.verifyFp == None:

                  key = ''
                  for i in range(16):
                      key += random.choice(string.ascii_lowercase + string.ascii_uppercase + string.digits)
                  self.verifyFp = key

            self.proxy = None
            
            await self.page.evaluate("() => { " + self.__get_js(proxy=self.proxy) + " }")
            # await self.page.goto("https://www.tiktok.com/trending?lang=en", {
            #     'waitUntil': "load"
            # })
    
    
            self.signature = await self.page.evaluate('''() => {
              var urls = ["''' + self.url + '''"]
              var token = urls.map(x => window.byted_acrawler.sign({ url: x}))

              // var t = {}
              // webpackJsonp.filter(x => typeof x[1]['duD4'] === "function")[0][1].duD4(null, t)
              // var token = urls.map(x => t.sign({ url: x}))

              return token;
              }''')
    
            await self.browser.close()
        except:
            await self.browser.close()

    def __get_js(self, proxy=None):
        return requests.get("https://sf16-muse-va.ibytedtos.com/obj/rc-web-sdk-gcs/acrawler.js").text
