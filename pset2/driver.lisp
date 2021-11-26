(load "deps")
(load "item")

;;; TODO: change these two names later
(load "item_list")
(load "scraper2")

(defvar *item-list-dom*
  (scrape (search-query-to-url "rtx 8000")))

(defvar *item-ids*
  (map 'vector
       #'item-url-to-id
       (filter-ebay-item-urls
        (lquery:$ *item-list-dom* "a" (attr :href)))))

(defvar *item-infos*
  (lparallel:pmap 'vector #'item-info *item-ids*))
