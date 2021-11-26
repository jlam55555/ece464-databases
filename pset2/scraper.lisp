;;; Web scraping based on the Common Lisp Cookbook:
;;; https://lispcookbook.github.io/cl-cookbook/web-scraping.html

;;; load dependencies
;; (load "./deps.lisp")

;; (defun query-text (dom q)
;;   "Query text on DOM. A function wrapper around the lquery:$ macro."
;;   (lquery:$ dom q (text)))

;; (defun query-attr (dom q a)
;;   "Query an attribute on DOM. A function wrapper around the lquery:$ macro."
;;   (lquery:$ dom q (attr a)))

;; (defvar *test* (query-attr (scrape "https://lambdalambda.ninja") "a" :href))

;; (defvar *paths* (map 'vector (lambda (path) (if (str:starts-with? "/" path) (concatenate 'string "https://lambdalambda.ninja" path) path)) *test*))
;; (setf (aref *paths* 0) "https://anesriansei.osrg")

;; (defvar *lst1* (map 'vector (lambda (it) (ignore-errors (nth-value 1 (dex:get it)))) *paths*))


;;; remove invalid (nil) values

;;; TODO: this might still be useful
;; (defvar *lst2*
;;   (remove-if-not
;;    (lambda (x) x)
;;    (lparallel:pmap
;;     'vector
;;     (lambda (it)
;;       (ignore-errors
;;        (if (= 200 (nth-value 1 (dex:get it)))
;;            it)))
;;     *paths*)))

;;; scrape ebay links with query
;; (defvar *test2* (query-attr (scrape "https://www.ebay.com/sch/i.html?_nkw=nvidia+rtx8000") "a" :href))

;;; get only item links
;; (defvar *item-url-base* "https://www.ebay.com/itm/")
;; (defvar *test3* (remove-if-not (lambda (url) (str:starts-with? *item-url-base* url)) *test2*))

;;; map links to item ID's
;; (defun item-url-to-id (url)
;;   (parse-integer
;;    (cadr (str:rsplit "/"
;;                      (quri:render-uri (quri:make-uri :defaults url :query '()))
;;                      :limit 2))))

;; (defvar *test4*
;;   (map 'vector #'item-url-to-id *test3*))

;; ;;; for a single item URI
;; (defun remove-comments (dom)
;;   (lquery:parse-html
;;    (cl-ppcre:regex-replace-all "<!--(.*?)-->" (lquery-funcs:serialize (lquery-funcs:node dom)) "")))

;; (defvar *item-id* (aref *test4* 1))
;; (defvar *item-url* (concatenate 'string *item-url-base* (write-to-string *item-id*)))
;; (defvar *item-dom*
;;   (let ((dom-no-comments (lquery-funcs:root (remove-comments (scrape *item-url*)))))
;;     (lquery:$ dom-no-comments "script,style" (remove))
;;     (lquery:initialize dom-no-comments)
;;     dom-no-comments))
;; (defvar *item-name* (query-text *item-dom* "#itemTitle"))
;; (defvar *item-price*
;;   (parse-float:parse-float (aref (query-attr *item-dom* ".mainPrice [itemprop='price']" :content) 0)))
;; (defvar *item-currency*
;;   (aref (query-attr *item-dom* ".mainPrice [itemprop='priceCurrency']" :content) 0))

;; ;;; get all itemprops (including price and currency)
;; (defvar *item-itemprops*
;;   (remove-if-not
;;    (lambda (itemprop-list)
;;      (and
;;       (not (string=  (car itemprop-list) "position"))
;;       (cadr itemprop-list)))
;;    (lquery:$ *item-dom* "[itemprop]" (combine (attr :itemprop) (attr :content)))))

;;; get item info
;; (defvar *item-about-this-item-dom*
;;   (lquery:$ *item-dom* "[data-testid='x-about-this-item']"))
;; (defvar *item-about-this-item-labels*
;;   (lquery:$ *item-about-this-item-dom* ".ux-labels-values__labels" (text)))
;; (defvar *item-about-this-item-values*
;;   (lquery:$ *item-about-this-item-dom* ".ux-labels-values__values" (text)))
;; (defvar *item-about-this-item*
;;   (map 'vector
;;        (lambda (label value) (cons label value))
;;        *item-about-this-item-labels*
;;        *item-about-this-item-values*))

;;; get text from description (not in iframe)
;; (defvar *desc-text*
;;   (str:trim (str::collapse-whitespaces (aref (lquery:$ *item-dom* "#desc_wrapper_ctr" (text)) 0))))

;;; get text from item iframe, if any
;; (defvar *desc-iframe-dom*
;;   (let ((dom (aref (scrape (aref (lquery:$ *item-dom* "#desc_wrapper_ctr" "iframe" (attr 'src)) 0)) 0)))
;;     (lquery:$ dom "script,style" (remove))
;;     dom))
;; (defvar *desc-iframe-text*
;;   (str:trim
;;    (str:collapse-whitespaces
;;     (lquery-funcs:text *desc-iframe-dom*))))

;;; condition information
;; (defvar *condition-message*
;;   (aref (lquery:$ *item-dom* ".topItmCndDscMsg" (text)) 0))

;;; seller name
;; (defvar *seller-name*
;;   (str:trim (aref (lquery:$ *item-dom* ".si-content a" (text)) 0)))

;;; shipping information
;; (defvar *item-location*
;;   (str:trim (aref (lquery:$ *item-dom* ".sh-loc" (text)) 0)))

;;; shipping to (includes and excludes list)
;; (defvar *ships-to*
;;   (let ((includes-excludes
;;           (map 'vector
;;                (lambda (s)
;;                  (str:split
;;                   ", "
;;                   (cadr (str:split ": " (str:trim (str:collapse-whitespaces s)) :limit 2))))
;;                (lquery:$ *item-dom* ".sh-sLoc" (text)))))
;;     includes-excludes))

;;; shipping policies
;; (defvar *shipping-policies*
;;   (map 'vector
;;        (lambda (s) (str:trim (str:collapse-whitespaces s)))
;;        (lquery:$ *item-dom* ".sh-tbl tbody" (text))))

;; ;;; return policies
;; (defvar *return-policies*
;;   (remove-if
;;    (lambda (s) (string= s ""))
;;    (map 'vector
;;         (lambda (s) (str:trim (str:collapse-whitespaces s)))
;;         (lquery:$ *item-dom* ".rpTbl" (text)))))

;; ;;; payment policies
;; (defvar *payment-policies*
;;   (map 'vector
;;        (lambda (s) (str:trim (str:collapse-whitespaces s)))
;;        (lquery:$ *item-dom* ".pd-data-tbl tbody" (text))))

;;; people who viewed this item also viewed...
;; (defvar *phvtiav-dom*
;;   (let ((merch-modules
;;           (lquery:$ *item-dom* ".merch-module section")))
;;     (aref (remove-if-not
;;            (lambda (section)
;;              (string=
;;               (aref (lquery:$ section ".merch-title" (text)) 0)
;;               "People who viewed this item also viewed"))
;;            merch-modules) 0)))
;; (defvar *phvtiav-ids*
;;   (map 'vector
;;        #'item-url-to-id
;;        (lquery:$ *phvtiav-dom* "a" (attr :href))))

;;; get images
;; (defvar *image-urls*
;;   (delete-duplicates
;;    (map
;;     'vector
;;     ;; replace with larger image
;;     (lambda (url)
;;       (cl-ppcre:regex-replace
;;        "-l([0-9]+)(.[a-z]+)$"
;;        url
;;        (lambda (_ &rest groups)
;;          (declare (ignore _))
;;          (concatenate 'string "-l2000" (cadr groups)))
;;        :simple-calls T))
;;     (remove-if
;;      (lambda (url) (str:contains? "ebaystatic.com" url))
;;      (lquery:$ *item-dom* "#PicturePanel img" (attr :src))))
;;    :test #'string=))

;; ;;; putting all the data together
;; (defvar *item-info*
;;   (list
;;    (cons 'id *item-id*)
;;    (cons 'name *item-name*)
;;    (cons 'itemprops *item-itemprops*)
;;    (cons 'about *item-about-this-item*)
;;    (cons 'desc-text *desc-text*)
;;    (cons 'desc-iframe-text *desc-iframe-text*)
;;    (cons 'condition-message *condition-message*)
;;    (cons 'seller-name *seller-name*)
;;    (cons 'item-location *item-location*)
;;    (cons 'ships-to *ships-to*)
;;    (cons 'shipping-policies *shipping-policies*)
;;    (cons 'return-policies *return-policies*)
;;    (cons 'payment-policies *payment-policies*)
;;    (cons 'phvtiav-ids *phvtiav-ids*)
;;    (cons 'image-urls *image-urls*)))
