;;; Driver code for sample scraping.
(load "ebay-scraper")

(ebay-scraper:set-db-name "ece464_pset2")

;;; gpu shenanigans and crocs (why?)
(ebay-scraper:scrape-items
 '("rtx 8000"
   "rtx 6000"
   "rtx 3090"
   "gtx 1080"
   "gpu"
   "cpu"
   "computer"
   "thinkstation"
   "sandals"
   "crocs"))
