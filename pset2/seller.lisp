;;; Helper functions for scraping an eBay seller.

(defvar *seller-url-base*
  "https://www.ebay.com/usr/"
  "Base URL for sellers on eBay.")

(defun seller-name-to-url (name)
  "Gets the URL for a seller."
  (concatenate 'string *seller-url-base* name))

(defun seller-dom (name)
  "Gets the DOM for a seller."
  (scrape-remove-non-text (seller-name-to-url name)))

(defun seller-get-name (dom)
  "Gets the seller's name."
  (str:replace-first
   "User ID "
   ""
   (scrape-first-text dom ".usrinfo a.mbg-id")))

(defun seller-get-bio (dom)
  "Gets the seller's bio."
  (string-trim-collapse
   (scrape-first-text dom ".bio.inline_value")))

(defun seller-get-feedback-score (dom)
  "Gets the seller's feedback score."
  (parse-integer
   (cl-ppcre:regex-replace
    "^.*?([0-9]+)$"
    (afirst (lquery:$ dom ".gspr.yellowShoot.star" (attr :aria-label)))
    (lambda (match &rest groups)
      (declare (ignore match))
      (car groups))
    :simple-calls t)))

(defun seller-get-feedback-scores (dom)
  "Gets the seller's distribution of feedback scores."
  (map
   'list
   #'cons
   (lquery:$ dom ".usrfedbk .score .txt" (text))
   (map
    'vector
    (lambda (num) (parse-integer (str:replace-all "," "" num)))
    (lquery:$ dom ".usrfedbk .score .num" (text)))))

(defun seller-get-feedback-ratings (dom)
  "Gets seller's feedback ratings (stars for item as described, communication,
shipping time, shipping charges)."
  (map
   'list
   #'cons
   (lquery:$ dom ".dsr_type" (text))
   (map
    'vector
    (lambda (dsr-count) (parse-integer (str:replace-all "," "" dsr-count)))
    (lquery:$ dom ".dsr_count" (text)))))

(defun seller-get-creation-date (dom)
  "Gets seller's creation date."
  (chronicity:parse
   (car (cl-ppcre:all-matches-as-strings
         "(?<=Member since: )([^|]+)"
         (scrape-first-text dom "#member_info")))))

(defun seller-get-location (dom)
  "Gets seller location."
  (scrape-first-text dom ".mem_loc"))

(defun seller-info (name)
  "Gets all of the information for a seller."
  (let ((dom (seller-dom name)))
    (and dom
         (lparallel:pmap
          'list
          (lambda (getter-symbol)
            (cons
             (string-downcase
              (str:replace-first "SELLER-GET-" "" (symbol-name getter-symbol)))
             (funcall (symbol-function getter-symbol) dom)))
          '(seller-get-name
            seller-get-bio
            seller-get-feedback-score
            seller-get-feedback-scores
            seller-get-feedback-ratings
            seller-get-creation-date
            seller-get-location)))))
