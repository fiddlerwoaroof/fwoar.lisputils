(defpackage :fwoar.lexical-compare
  (:use :cl )
  (:export
   #:lexi-compare
   #:apply-when
   #:natural-sort-strings))
(in-package :fwoar.lexical-compare)

(defun parse-mixed-string (str)
  (let ((first-int-pos (position-if #'digit-char-p str)))
    (if (> (length str) 0)
        (if first-int-pos
            (if (> first-int-pos 0)
                (cons (subseq str 0 first-int-pos)
                      (parse-mixed-string (subseq str first-int-pos)))
                (multiple-value-bind (int end) (parse-integer str :junk-allowed t)
                  (cons int
                        (parse-mixed-string
                         (subseq str end)))))
            (list str))
        nil)))

(defgeneric part< (a b)
  (:method (a b)
    nil)
  (:method ((a string) (b number))
    t)
  (:method ((a number) (b number))
    (< a b))
  (:method ((a string) (b string))
    (string< a b)))

(defgeneric part= (a b)
  (:method (a b)
    nil)
  (:method ((a number) (b number))
    (= a b))
  (:method ((a string) (b string))
    (string= a b)))

#+(or)
(st:deftest test-parse-mixed-string ()
  (st:should be equal
             (list)
             (parse-mixed-string ""))

  (st:should be equal
             (list "asdf")
             (parse-mixed-string "asdf"))

  (st:should be equal
             (list "asdf" 1234)
             (parse-mixed-string "asdf1234"))

  (st:should be equal
             (list 1234 "asdf")
             (parse-mixed-string "1234asdf"))

  (st:should be equal
             (list "asdf" 1234 "a")
             (parse-mixed-string "asdf1234a")))

(defun apply-when (fun &rest args)
  (when (car (last args))
    (apply 'apply fun args)))

(defun lexi-compare (a b &optional (elem-compare 'part<))
  (let* ((mismatch-pos (mismatch a b :test 'part=))
         (a-tail (when mismatch-pos (nthcdr mismatch-pos a)))
         (b-tail (when mismatch-pos (nthcdr mismatch-pos b))))
    (or (when (and a-tail b-tail)
          (funcall elem-compare
                   (car a-tail)
                   (car b-tail)))
        (null a-tail))))

(defun natural-sort-strings (a b)
  (lexi-compare (parse-mixed-string a)
                (parse-mixed-string b)))
