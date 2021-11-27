;;; Helper functions that extend the dexador/lquery interface.
;;; 
;;; In particular, lquery has some strange behavior, such as including comment
;;; script, and style node contents as text. These get rid of those text
;;; elements, which are not useful for this scraping use case.

(defun afirst (arr)
  "Helper function to get first element of an array."
  (aref arr 0))

(defun string-trim-collapse (s)
  "Helper function to trim extra spaces in text."
  (if (stringp s)
      (str:trim (str:collapse-whitespaces s))
      ""))

(defun scrape-first-text (dom selector)
  "Helper function to get (single) string if exists, or empty string
otherwise."
  (let ((selections (lquery:$ dom selector (text))))
    (if (or (zerop (array-total-size selections))
            (not (stringp (afirst selections))))
        ""
        (afirst selections))))

(defun scrape-remove-non-text (url)
  "Scrape and remove comments, script, style tags."
  (log:info "Scraping " url)
  (let ((req-text (handler-case
                      (dex:get url)
                    (error () nil))))
    (if (and req-text (not (zerop (array-total-size req-text))))
        (let ((dom
                (afirst
                 (lquery:$
                   (initialize
                    ;; remove comments
                    (cl-ppcre:regex-replace-all
                     "<!--(.*?)-->"
                     ;; remove ascii:
                     ;; https://programming-idioms.org/idiom/147/remove-all-non-ascii-characters/2791/lisp
                     (remove-if-not (lambda (i) (<= 0 i 127))
                                    req-text
                                    :key #'char-code)
                     ""))))))
          (lquery:$ dom "script,style" (remove))
          dom)
        nil)))
