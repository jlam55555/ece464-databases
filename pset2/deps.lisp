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
   :parse-float                         ; deserialize floats
   ))

;;; https://github.com/fukamachi/dexador/issues/88#issuecomment-840114866
(setf dexador.connection-cache::*threads-connection-pool*
      (make-hash-table :test 'equal :synchronized t))
