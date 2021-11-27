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

(defun get-item-collection-name (query)
  "Gets the name of the collection for an item of this query."
  (concatenate 'string "item_" (str:replace-all " " "_" query)))

(defun get-seller-collection-name (query)
  "Gets the name of the collection for a seller of this query."
  (concatenate 'string "seller_" (str:replace-all " " "_" query)))

(defun insert-seller-to-mongo (name query)
  "Scrapes a seller, inserts into the designated collection."
  (let ((seller-info-doc (alist-to-document (seller-info name))))
    (sb-thread:with-mutex (*mut*)
      (cl-mongo:db.update
       (get-seller-collection-name query)
       (cl-mongo:kv "name"
                    (cl-mongo:get-element "name" seller-info-doc))
       seller-info-doc
       :upsert t))))

(defun insert-items-to-mongo (ids query)
  "Inserts a list of items (given a list of ID's) and their sellers to mongodb."
  (lparallel:pmap
   'vector
   (lambda (id)
     (let ((item-doc (alist-to-document (item-info id))))
       (insert-seller-to-mongo
        (cl-mongo:get-element "seller-name" item-doc)
        query)
       (sb-thread:with-mutex (*mut*)
         (cl-mongo:db.insert
          (get-item-collection-name query)
          item-doc)))
     'ok)
   ids))

(defun scrape-item (query)
  "Scrapes a single query/item."
  (insert-items-to-mongo
   (scrape-all-pages query)
   query))

(defun scrape-items (queries)
  "Scrapes a list of queries/items."
  (lparallel:pmap
   'vector
   (lambda (query)
     (scrape-item query)
     'ok)
   queries))
