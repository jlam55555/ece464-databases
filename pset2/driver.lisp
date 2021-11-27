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

(defun insert-seller-to-mongo (name)
  (let ((seller-info-doc (alist-to-document (seller-info name))))
    (sb-thread:with-mutex (*mut*)
      (cl-mongo:db.update
       "sellers"
       (cl-mongo:kv "name"
                    (cl-mongo:get-element "name" seller-info-doc))
       seller-info-doc
       :upsert t))))

(defun insert-items-to-mongo (collection-name ids)
  (lparallel:pmap
   'vector
   (lambda (id)
     (let ((item-doc (alist-to-document (item-info id))))
       (insert-seller-to-mongo (cl-mongo:get-element "seller-name" item-doc))
       (sb-thread:with-mutex (*mut*)
         (cl-mongo:db.insert collection-name item-doc)))
     'ok)
   ids))

(defun scrape-item (query)
  (insert-items-to-mongo
   (concatenate 'string "item_" (str:replace-all " " "_" query))
   (scrape-all-pages query)))

(lparallel:pmap
 'vector
 (lambda (query)
   (scrape-item query)
   'ok)
 '("rtx 8000"))
