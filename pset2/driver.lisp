;;; Driver code for sample scraping.
(load "ebay-scraper")

(ebay-scraper:set-db-name "ece464_pset2")
(ebay-scraper:scrape-items '("rtx 8000"))


