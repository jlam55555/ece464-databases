;;; Helper functions for scraping an eBay item page
;;; (the URL would look like "https://www.ebay.com/itm/...")

(defvar *item-url-base*
  "https://www.ebay.com/itm/"
  "Base URL of an eBay item page.")

(defun item-url-to-id (url)
  "Converts an eBay URL to its item ID."
  (parse-integer
   ;; get last path component
   (cadr
    (str:rsplit "/"
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
  (scrape (item-id-to-url id)))

(defun item-name (dom)
  "Gets item title."
  (lquery:$ dom "#itemTitle" (text)))

