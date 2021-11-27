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

(defun item-get-id (dom)
  "Gets item id."
  (parse-integer
   (scrape-first-text dom "#descItemNumber")))

(defun item-get-name (dom)
  "Gets item title."
  (str:trim
   (str:replace-first
    "Details about"
    ""
    (scrape-first-text dom "#itemTitle"))))

(defun item-get-itemprops (dom)
  "Gets item properties (e.g., price, currency)"
  (map
   'list
   (lambda (itemprop-list)
     (let ((key (car itemprop-list))
           (value (cadr itemprop-list)))
       (cons key
             (if (string= key "price")
                 (parse-float:parse-float value)
                 value))))
   (remove-if-not
    (lambda (itemprop-list)
      (and
       (not (string= (car itemprop-list) "position"))
       (cadr itemprop-list)))
    (lquery:$ dom "[itemprop]" (combine (attr :itemprop) (attr :content))))))

(defun item-get-about-this-item (dom)
  "Gets specifics about this item ('Item specifics')"
  (let ((about-this-item-dom
          (lquery:$ dom "[data-testid='x-about-this-item']")))
    (map 'list
         (lambda (label value) (cons label value))
         (lquery:$ about-this-item-dom ".ux-labels-values__labels" (text))
         (lquery:$ about-this-item-dom ".ux-labels-values__values" (text)))))

(defun item-get-desc-text (dom)
  "Gets description text (not in iframe)."
  (string-trim-collapse
   (scrape-first-text dom "#desc_wrapper_ctr")))

(defun item-get-desc-iframe-text (dom)
  "Gets description text (from first iframe in description, if any)."
  (let ((iframe-dom
          (scrape-remove-non-text
           (afirst (lquery:$ dom "#desc_wrapper_ctr iframe" (attr :src))))))
    (when iframe-dom
      (string-trim-collapse (lquery-funcs:text iframe-dom)))))

(defun item-get-condition-message (dom)
  "Gets message about item condition."
  (scrape-first-text dom ".topItmCndDscMsg"))

(defun item-get-seller-name (dom)
  "Gets seller name."
  (str:trim (scrape-first-text dom ".si-content a")))

(defun item-get-item-location (dom)
  "Gets item location."
  (str:trim
   (str:replace-first
    "Item location:"
    ""
    (scrape-first-text dom ".sh-loc"))))

(defun item-get-ships-to-locations (dom)
  "Gets item ships to/excludes shipping to locations."
  (let ((ships-to
          (map 'list
               (lambda (s)
                 (map 'vector
                      #'identity
                      (str:split
                       ", "
                       (cadr
                        (str:split
                         ": "
                         (str:trim (str:collapse-whitespaces s))
                         :limit 2)))))
               (lquery:$ dom ".sh-sLoc" (text)))))
    (when (and (consp ships-to) (= (length ships-to) 2))
      (list
       (cons "includes" (car ships-to))
       (cons "excludes" (cadr ships-to))))))

(defun item-get-policies (dom)
  "Gets shipping, return, and payment policy details."
  (let ((shipping-policies
          (map 'vector
               #'string-trim-collapse
               (lquery:$ dom ".sh-tbl tbody" (text))))
        (return-policies
          (map 'vector
               #'string-trim-collapse
               (lquery:$ dom ".rpTbl" (text))))
        (payment-policies
          (map 'vector
               #'string-trim-collapse
               (lquery:$ dom ".pd-data-dbl tbody" (text)))))
    (list
     (cons "shipping" shipping-policies)
     (cons "return" return-policies)
     (cons "payment" payment-policies))))

(defun item-get-also-viewed (dom)
  "Gets ids of other products commonly viewed alongside this item."
  (let ((also-viewed-dom
          (let ((merch-modules
                  (lquery:$ dom ".merch-module section")))
            (afirst (remove-if-not
                     (lambda (section)
                       (string=
                        (scrape-first-text section ".merch-title")
                        "People who viewed this item also viewed"))
                     merch-modules)))))
    (map 'vector
         #'item-url-to-id
         (lquery:$ also-viewed-dom "a" (attr :href)))))

(defun item-get-images (dom)
  "Gets the images of the item. (at the largest resolution, l2000)."
  (delete-duplicates
   (map
    'vector
    ;; replace with larger image
    (lambda (url)
      (cl-ppcre:regex-replace
       "-l([0-9]+)(.[a-z]+)$"
       url
       (lambda (_ &rest groups)
         (declare (ignore _))
         (concatenate 'string "-l2000" (cadr groups)))
       :simple-calls T))
    (remove-if
     ;; don't include static images
     (lambda (url) (str:contains? "ebaystatic.com" url))
     (lquery:$ dom "#PicturePanel img" (attr :src))))
   :test #'string=))

(defun item-info (id)
  "Gets all of the item information for an item ID."
  (log:info "Scraping item id: " id)
  (let ((dom (item-dom id)))
    (and dom
         (lparallel:pmap
          'list
          (lambda (getter-symbol)
            (cons
             (string-downcase
              (str:replace-first "ITEM-GET-" "" (symbol-name getter-symbol)))
             (funcall (symbol-function getter-symbol) dom)))
          '(item-get-id
            item-get-name
            item-get-itemprops
            item-get-about-this-item
            item-get-desc-text
            item-get-desc-iframe-text
            item-get-condition-message
            item-get-seller-name
            item-get-item-location
            item-get-ships-to-locations
            item-get-policies
            ;; item-get-also-viewed
            item-get-images)))))
