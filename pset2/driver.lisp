;;; Driver code: entrypoint for sample code.
(load "deps")
(load "mongo")

(defvar *db-name* "ece464_pset2")
(defvar *collection-name* "ebay")
(defvar *mut* (sb-thread:make-mutex))
(cl-mongo:db.use *db-name*)

(load "scraper")
(load "item")
(load "item-list")

;;; Scrape an arbitrary search
(scrape-all-pages "gpu")
