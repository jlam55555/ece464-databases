;;; TODO: change some of these names later
(load "deps")
(load "scraper")
(load "item")
(load "item-list")

;;; keep scraping until no new ids are found
(defvar *item-infos*
  (scrape-all-pages "rtx 8000"))
