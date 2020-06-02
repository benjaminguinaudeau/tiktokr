import asyncio
import pyppeteer
import random
from pyppeteer_stealth import stealth

class browser:
    def __init__(self, url, ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A372 Safari/604.1"):
        self.url = url
        self.userAgent = ua
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
            'userDataDir': "./tmp"
        }

        asyncio.get_event_loop().run_until_complete(self.start())

    async def start(self):
        self.browser = await pyppeteer.launch(self.options)
        self.page = await self.browser.newPage()

        await stealth(self.page)

        await self.page.emulate({'viewport': {
            'width': random.randint(320, 1920),
            'height': random.randint(320, 1920),
            'deviceScaleFactor': random.randint(1, 3),
            'isMobile': random.random() > 0.5,
            'hasTouch': random.random() > 0.5
        }})

        await self.page.setUserAgent(self.userAgent)
        
        # await self.page.evaluate('''() => {
        #     document.setCookie = 'sid_guard=de3a7d545d4bc49a1764a0f003df17fa%7C1590933930%7C5184000%7CThu%2C+30-Jul-2020+14%3A05%3A30+GMT'
        #     }''')
        # await self.page.evaluate('''() => {
        #     return document.cookie
        # }''')
        # await self.page.setCookie({'sid_guard'='de3a7d545d4bc49a1764a0f003df17fa%7C1590933930%7C5184000%7CThu%2C+30-Jul-2020+14%3A05%3A30+GMT'})

        await self.page.goto("https://www.tiktok.com/trending?lang=en", {
            'waitUntil': "load"
        })

        self.signature = await self.page.evaluate('''() => {
          var t = {}
          webpackJsonp.filter(x => typeof x[1]['duD4'] === "function")[0][1].duD4(null, t)
          var urls = ["''' + self.url + '''"]
          var token = urls.map(x => t.sign({url : x}))
          return token;
          }''')

        # self.signature = await self.page.evaluate('''() => {
        #   var t = {}
        #   webpackJsonp.filter(x => typeof x[1]['duD4'] === "function")[0][1].duD4(null, t)
        #   var url = "''' + self.url + '''"
        #   var token = t.sign({url: url})
        #   return token;
        #   }''')
        await self.browser.close()


