import os
import re
import urllib

from scrapy.spider import BaseSpider
from scrapy.selector import HtmlXPathSelector
from scrapy.http.request import Request

class MathJeanSpider(BaseSpider):
    """
    """
    base_url = "http://genealogy.math.ndsu.nodak.edu"
    start_urls = [
        # pierce
        "http://genealogy.math.ndsu.nodak.edu/id.php?id=50223"
    ]
    download_delay = 1
    name = "mathjean"

    OUTPUT = "./crawl.log"

    def urlOfId(self, href):
        return "%s/%s" % (self.base_url, href)

    def parse(self, response):
        """
        2014-07-11:
            Find the name
            Find the advisor(s)
        """
        hxs = HtmlXPathSelector(response)
        sName = hxs.select("//h2/text()").extract()[0].strip()
        advisorHxs = hxs.select("//p[contains(text(), 'Advisor')]/a")
        for ahxs in advisorHxs:
            aName = ahxs.select("./text()").extract()[0].strip()
            href = ahxs.select("./@href").extract()[0].strip()
            with open(self.OUTPUT, "a") as f:
                print>>f, "\t".join([sName, aName])
            yield Request(self.urlOfId(href), callback=self.parse)
                
