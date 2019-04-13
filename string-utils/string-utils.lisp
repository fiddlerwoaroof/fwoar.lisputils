(in-package :fwoar.string-utils)

(defun insert-at (position item string)
  (concatenate 'string
               (subseq string 0 position)
               (string item)
               (subseq string position)))

(defun insert-where (predicate item string)
  (insert-at (position-if predicate string)
             item
             string))
