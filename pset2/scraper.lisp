;;; Web scraping based on the Common Lisp Cookbook:
;;; https://lispcookbook.github.io/cl-cookbook/web-scraping.html

;;; load dependencies
(ql:quickload '("dexador" "plump" "lquery" "lparallel" "str"))

;;; https://github.com/fukamachi/dexador/issues/88#issuecomment-840114866
(setf dexador.connection-cache::*threads-connection-pool*
      (make-hash-table :test 'equal :synchronized t))

(defun scrape (url)
  "Get the parsed DOM for the given URL."
  (lquery:$ (initialize (dex:get url))))

(defun query-text (dom q)
  "Query text on DOM. A function wrapper around the lquery:$ macro."
  (lquery:$ dom q (text)))

(defun query-attr (dom q a)
  "Query an attribute on DOM. A function wrapper around the lquery:$ macro."
  (lquery:$ dom q (attr a)))

(defvar *test* (query-attr (scrape "https://lambdalambda.ninja") "a" :href))

(defvar *paths* (map 'vector (lambda (path) (if (str:starts-with? "/" path) (concatenate 'string "https://lambdalambda.ninja" path) path)) *test*))
(setf (aref *paths* 0) "https://anesriansei.osrg")

(defvar *lst1* (map 'vector (lambda (it) (ignore-errors (nth-value 1 (dex:get it)))) *paths*))

(setf lparallel:*kernel* (lparallel:make-kernel 10))

;;; remove invalid (nil) values
(defvar *lst2*
  (remove-if-not
   (lambda (x) x)
   (lparallel:pmap
    'vector
    (lambda (it)
      (ignore-errors
       (if (= 200 (nth-value 1 (dex:get it)))
           it)))
    *paths*)))
