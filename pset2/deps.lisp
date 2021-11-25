;;; install dependencies
(ql:quickload
 '(:dexador                             ; request library
   :plump                               ; XML/HTML markup parser
   :lquery                              ; DOM query
   :lparallel                           ; parallel programming library (e.g., parallel requests)
   :str                                 ; string manipulation
   :cl-ppcre                            ; regex
   :quri                                ; URL parsing
   :parachute                           ; test framework
   :cl-mongo                            ; MongoDB driver
   ))
