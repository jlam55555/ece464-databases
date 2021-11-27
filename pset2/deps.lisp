;;; Setup dependencies for the project
(ql:quickload
 '(:chronicity       ; date library
   :cl-mongo         ; MongoDB driver
   :cl-ppcre         ; regex
   :hash-set         ; hash sets
   :log4cl           ; logging
   :lparallel        ; parallel programming library (e.g., parallel requests)
   :lquery           ; DOM query
   :parachute        ; test framework
   :parse-float      ; deserialize floats
   :plump            ; XML/HTML markup parser
   :quri             ; URL parsing
   :str              ; string manipulation
   :dexador          ; request library
   ))

;;; Problem with parallelization and synchronization
;;; https://github.com/fukamachi/dexador/issues/88#issuecomment-840114866
(setf dexador.connection-cache::*threads-connection-pool*
      (make-hash-table :test 'equal :synchronized t))

;;; Parallelize web requests
(setf lparallel:*kernel* (lparallel:make-kernel 12))

