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

(defun scrape (url)
  "Get the parsed DOM for the given URL, or an error value if the page
doesn't exist."
  (log:info "Scraping " url)
  (handler-case (afirst (lquery:$ (initialize (dex:get url))))
    (error () nil)))

(defun remove-non-text (dom)
  "Remove comments, script, style tags from DOM."
  (let ((dom-no-comments
          (afirst
           (lquery-funcs:root
            (lquery:parse-html
             (cl-ppcre:regex-replace-all
              "<!--(.*?)-->"
              (lquery-funcs:serialize (lquery-funcs:node dom))
              ""))))))
    (lquery:$ dom-no-comments "script,style" (remove))
    dom-no-comments))

(defun scrape-remove-non-text (url)
  "Scrape and remove comments, script, style tags."
  (let ((dom (scrape url)))
    (and dom (remove-non-text dom))))
