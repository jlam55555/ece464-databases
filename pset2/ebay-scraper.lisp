;;; Package definition for ebay-scraper package
(defpackage :ebay-scraper
  (:documentation "Tools for scraping eBay items and sellers into a MongoDB 
database. 

@a[https://github.com/jlam55555/ece464-databases/tree/master/pset2]{Source.}")
  (:use :cl)
  (:export :set-db-name
   :scrape-item
           :scrape-items
   :item-info
           :seller-info))

(in-package :ebay-scraper)

(load "deps")
(load "mongo")
(load "scraper")
(load "item")
(load "item-list")
(load "seller")

(defun set-db-name (db-name)
  "Sets the MongoDB database name."
  (cl-mongo:db.use db-name))

(defvar *mut*
  (sb-thread:make-mutex)
  "Synchronizes calls to cl-mongo CRUD operations.")

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
  "Scrapes a single query/item."
  (insert-items-to-mongo
   (concatenate 'string "item_" (str:replace-all " " "_" query))
   (scrape-all-pages query)))

(defun scrape-items (queries)
  "Scrapes a list of queries/items."
  (lparallel:pmap
   'vector
   (lambda (query)
     (scrape-item query)
     'ok)
   queries))
