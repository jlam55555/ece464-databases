;;; TODO: change some of these names later
(load "deps")
(load "scraper2")
(load "item")
(load "item_list")

;;; keep scraping until no new ids are found
(defvar *item-infos*
  (scrape-all-pages "rtx 8000"))
