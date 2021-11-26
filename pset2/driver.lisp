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
(defvar *item-ids*
  (scrape-all-pages "gpu"))

(lparallel:pmap
 'vector
 ;; directly insert into db to avoid storing intermediately in memory
 (lambda (id)
   (let ((doc (alist-to-document (item-info id))))
     (sb-thread:with-mutex (*mut*)
       (cl-mongo:db.insert *collection-name* doc)))
   'ok)
*item-ids*)
