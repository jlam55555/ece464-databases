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

(defun range (max &key (min 0) (step 1))
  "Helper function to generate the range [lo,hi)
https://stackoverflow.com/a/13937652/2397327"
  (loop for n from min below max by step
        collect n))

(defun scrape-all-pages (query)
  "Scrapes all pages of a search until no new results are returned."
  (delete-duplicates
   (let* ((num-threads 20)
          (iter
            (lambda (iter seen-ids page-num acc id-doms-buf)
              ;; to parallelize this, grab multiple index pages at the same time
              (let* ((id-doms-buf
                       (if (= (mod page-num num-threads) 1)
                           (lparallel:pmap
                            'vector
                            (lambda (page-num)
                              (scrape (search-query-to-url query page-num)))
                            (range (+ page-num num-threads) :min page-num))
                           id-doms-buf))
                     (item-list-dom
                       (aref id-doms-buf
                             (mod (+ page-num num-threads -1) num-threads)))
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
                                  (concatenate 'vector acc item-ids)
                                  id-doms-buf))))))
          (funcall iter iter (hash-set:make-hash-set) 1 #() #()))))
