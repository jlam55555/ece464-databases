;;; Helper functions that extend the dexador/lquery interface.
;;; 
;;; In particular, lquery has some strange behavior, such as including comment
;;; script, and style node contents as text. These get rid of those text
;;; elements, which are not useful for this scraping use case.

(defun scrape (url)
  "Get the parsed DOM for the given URL."
  (lquery:$ (initialize (dex:get url))))
