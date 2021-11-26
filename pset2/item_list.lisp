(defvar *item-list-base*
  "https://www.ebay.com/sch/i.html?_nkw="
  "Base URL of an eBay item list page.")

(defun search-query-to-url (query)
  "Gets the URL for a item list page for the given search query"
  (concatenate 'string *item-list-base* (quri.encode:url-encode query)))
