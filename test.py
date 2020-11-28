import json
import sys
from TikTokApi import TikTokApi

api = TikTokApi.get_instance()
res = api.browser.sign_url(url = sys.argv[1])
print(json.dumps(res))
