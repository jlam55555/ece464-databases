;;; Driver code: entrypoint for sample code.
(load "deps")
(load "mongo")
(load "scraper")
(load "item")
(load "item-list")

;;; Set up database parameters
(defvar *db-name* "ece464_pset2")
(cl-mongo:db.use *db-name*)

;;; Concurrent database inserts may cause some to miss
(defvar *mut* (sb-thread:make-mutex))

;;; Scrape an arbitrary search
(defun insert-to-mongo (collection-name ids)
  (lparallel:pmap
   'vector
   (lambda (id)
     (let ((doc (alist-to-document (item-info id))))
       (sb-thread:with-mutex (*mut*)
         (cl-mongo:db.insert collection-name doc)))
     'ok)
   ids))

(defun scrape-item (query)
  (insert-to-mongo
   (concatenate 'string "item_" (str:replace-all " " "_" query))
   (scrape-all-pages query)))

(lparallel:pmap
 'vector
 (lambda (query)
   (scrape-item query)
   'ok)
 '("gpu" "cpu" "computer"))
