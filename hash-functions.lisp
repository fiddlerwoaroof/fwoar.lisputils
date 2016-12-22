(in-package #:fwoar.lisputils)

(defun pick (keys h-t)
  (mapcar (plambda:plambda (gethash :1 h-t))
          keys))

