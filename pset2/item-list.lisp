(defvar *item-list-base*
  "https://www.ebay.com/sch/i.html?_nkw="
  "Base URL of an eBay item list page.")

(defun search-query-to-url (query &optional pagenum)
  "Gets the URL for a item list page for the given search query."
  (concatenate 'string
               *item-list-base*
               (quri.encode:url-encode query)
               "&_pgn="
               (write-to-string (or pagenum 1))))

(defun scrape-all-pages (query)
  "Scrapes all pages of a search until no new results are returned."
  (let ((item-ids (delete-duplicates
    (let ((iter
            (lambda (iter seen-ids page-num acc)
              (let* ((item-list-dom
                       (scrape (search-query-to-url query page-num)))
                     (item-ids
                       (map 'vector
                            #'item-url-to-id
                            (filter-ebay-item-urls
                             (lquery:$ item-list-dom "a" (attr :href)))))
                     (new-ids-hashset
                       (hash-set:list-to-hs
                        (map 'list #'identity item-ids))))
                (if (= (hash-set:hs-count
                        (hash-set:hs-intersection seen-ids new-ids-hashset))
                       (hash-set:hs-count new-ids-hashset))
                    acc
                    (funcall iter
                             iter
                             (hash-set:hs-union seen-ids new-ids-hashset)
                             (+ page-num 1)
                             (concatenate 'vector acc item-ids)))))))
      (funcall iter iter (hash-set:make-hash-set) 1 #())))))
    (log:info "Got item ids: " item-ids)
    (lparallel:pmap
     'vector
     ;; directly insert into db to avoid storing intermediately in memory
     (lambda (id)
       (let ((doc (alist-to-document (item-info id))))
         (sb-thread:with-mutex (*mut*)
           (cl-mongo:db.insert *collection-name* doc)))
       'ok)
     item-ids)))
