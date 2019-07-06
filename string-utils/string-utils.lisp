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

(defmacro expand-branch (condition form)
  ;;; intentionally expands form twice: this should be ok, because
  ;;; only one side of the if will execute
  `(if ,condition
       ,form
       ,form))

(defun partition (at string &key from-end)
  (let ((from-end (not (not from-end))))
    (flet ((partition-char (char string from-end)
             (declare (type character char)
                      (type string string)
                      (type boolean from-end)
                      (optimize (speed 3)))
             (let ((pos (expand-branch from-end (position char string :from-end from-end))))
               (if pos
                   (list (subseq string 0 pos)
                         (subseq string (1+ pos)))
                   (list string
                         nil))))
           (partition-subseq (subseq string from-end)
             (declare (type sequence subseq)
                      (type string string)
                      (type boolean from-end)
                      (optimize (speed 3)))
             (let ((pos (expand-branch from-end (search subseq string :from-end from-end))))
               (if pos
                   (list (subseq string 0 pos)
                         (subseq string (+ (length subseq) pos)))
                   (list string
                         nil)))))
      (declare (inline partition-char partition-subseq)
               (optimize (speed 3)))
      (typecase at
        (character (partition-char at string from-end))
        (sequence (partition-subseq at string from-end))))))
