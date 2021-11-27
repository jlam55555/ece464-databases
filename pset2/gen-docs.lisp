;;; Build docs for the `ebay-scraper` package.
(load "ebay-scraper")

(quicklisp:quickload :atdoc)

(atdoc:generate-html-documentation
 '(:ebay-scraper)
 "docs/"
 :index-title "eBay web scraper to mongo"
 :heading "ebay-scraper"
 :include-slot-definitions-p t)
