(defvar *seller*
  (scrape-remove-non-text "https://www.ebay.com/usr/cloud_storage_corp"))

(cl-ppcre:regex-replace
 "^.*?([0-9]+)$"
 (afirst (lquery:$ *seller* ".gspr.yellowShoot.star" (attr :aria-label)))
 (lambda (match &rest groups)
   (declare (ignore match))
   (car groups))
 :simple-calls t)
