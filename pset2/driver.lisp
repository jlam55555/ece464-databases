;;; TODO: change some of these names later
(load "deps")
(load "scraper2")
(load "item")
(load "item_list")

;; (defvar *item-list-dom*
;;   (scrape (search-query-to-url "rtx 8000")))

;; (defvar *item-ids*
;;   (map 'vector
;;        #'item-url-to-id
;;        (filter-ebay-item-urls
;;         (lquery:$ *item-list-dom* "a" (attr :href)))))

;; (defvar *item-infos*
;;   (lparallel:pmap 'vector #'item-info *item-ids*))

;;; keep scraping until no new ids are found
(defvar *item-infos*
  (scrape-all-pages "rtx 8000"))
