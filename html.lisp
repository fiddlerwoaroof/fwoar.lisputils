(in-package :fw.lu)

(defun normalize-html (html)
  "Convert possibly bad HTML to sane HTML as best as possible."
  (let ((plump:*tag-dispatchers* plump:*html-tags*))
    (with-output-to-string (ss)
      (prog1 ss
        (map 'vector
             (lambda (x) (plump:serialize (plump:parse (plump:text x)) ss))
             html)))))
