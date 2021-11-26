;;; MongoDB helpers
(defun alist-to-document (al)
  "Converts directly from association list to cl-mongo:document."
  (let ((doc (cl-mongo:make-document)))
    (loop for kv in al do
      (let ((key (car kv))
            (value
              (let ((value (cdr kv)))
                (cond ((consp value) (alist-to-document value))
                      ((and (vectorp value) (not (stringp value)))
                       (map 'list #'identity value))
                      (t value)))))
        (cl-mongo:add-element key value doc)))
    doc))

(defun print-doc (doc)
  "Formats a document like a server reply so it can be printed with
cl-mongo:pp."
  (let ((result (list () (list doc))))
    (cl-mongo:pp result)))
