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
    download_delay = 1
    name = "mathjean"

    OUTPUT = "./crawl.log"

    def normalize(self, s):
        return "".join((c for c in s if ord(c) < 128))

    def parseNode(self, response):
        """
        2014-07-11:
            Find the name
            Find the advisor(s)
        """
        hxs = HtmlXPathSelector(response)
        sName = self.normalize(hxs.select("//h2/text()").extract()[0].strip())
        advisorHxs = hxs.select("//p[contains(text(), 'Advisor')]/a")
        for ahxs in advisorHxs:
            aName = self.normalize(ahxs.select("./text()").extract()[0].strip())
            href = ahxs.select("./@href").extract()[0].strip()
            with open(self.OUTPUT, "a") as f:
                print>>f, "\t".join([sName, aName])
            yield Request(self.urlOfId(href), callback=self.parseNode)

    def start_requests(self):
        self.reset_output()
        print("################################################################################")
        print("##########                     --- WELCOME ---                        ##########")
        print("################################################################################")
        print("Input the URL of the mathematician you want to start from:")
        url = raw_input()
        print("Thanks. Parsing the tree... Results will be saved incrementally to '%s'." % self.OUTPUT)
        yield Request(url, callback=self.parseNode)

    def reset_output(self):
        with open(self.OUTPUT, "w") as f:
            f.write("")
        return

    def urlOfId(self, href):
        return "%s/%s" % (self.base_url, href)
