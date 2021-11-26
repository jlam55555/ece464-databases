;;; Helper functions for scraping an eBay item page
;;; (the URL would look like "https://www.ebay.com/itm/...")

(defvar *item-url-base*
  "https://www.ebay.com/itm/"
  "Base URL of an eBay item page.")

(defun item-url-to-id (url)
  "Converts an eBay URL to its item ID."
  (parse-integer
   ;; get last path component
   (cadr (str:rsplit
          "/"
          ;; remove query parameters
          (quri:render-uri (quri:make-uri :defaults url :query '()))
          :limit 2))))

(defun item-id-to-url (id)
  "Converts an eBay item ID to its URL."
  (concatenate 'string *item-url-base* (write-to-string id)))

(defun filter-ebay-item-urls (urls)
  "Filters out non-eBay-item pages from a list of URL's."
  (remove-if-not
   (lambda (url) (str:starts-with? *item-url-base* url))
   urls))

(defun item-dom (id)
  "Gets the DOM for an item page for the item with the given id."
  (scrape-remove-non-text (item-id-to-url id)))

(defun item-get-name (dom)
  "Gets item title."
  (str:trim
   (str:replace-first
    "Details about"
    ""
    (afirst (lquery:$ dom "#itemTitle" (text))))))

(defun item-info (id)
  "Gets all of the item information for an item ID."
  (let ((dom (item-dom id)))
    (and dom
         (lparallel:pmap
          'vector
          (lambda (getter-symbol)
            (cons
             (string-downcase
              (str:replace-first "ITEM-GET-" "" (symbol-name getter-symbol)))
             (funcall (symbol-function getter-symbol) dom)))
          '(item-get-name)))))
