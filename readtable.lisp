;; Sketches for my readtable

(defun pr-re (form v)
  (format t "~&~s: ~s~%" form v)
  v)

(set-macro-character #\!
                     (lambda (s c)
                       c
                       (let ((form (read s t nil t)))
                         `(pr-re ',form ,form))))
