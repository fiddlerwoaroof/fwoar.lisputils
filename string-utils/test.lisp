(in-package :fwoar.string-utils)

(defun vos-equal (a b)
  (every 'equal a b))

(progn
  (st:deftest string-split-as-expected-with-test ()
    (st:should be vos-equal
               #("a" "a" "a" "a")
               (%split-on-string "b" "abababa" :test #'string-equal)))
  (st:deftest string-split-as-expected-with-count ()
    (st:should be vos-equal
               #("a" "b c d")
               (%split-on-string " " "a b c d" :count 2)))
  (st:deftest string-split-as-expected-with-consecutive-sep ()
    (st:should be vos-equal
               #("a" "b" "" "c" "d")
               (%split-on-string " " "a b  c d")))
  (st:deftest string-split-as-expected-with-leading-and-trailing-sep ()
    (st:should be vos-equal
               #("" "a" "b" "c" "d" "")
               (%split-on-string " " " a b c d ")))
  (st:deftest string-split-as-expected-with-leading-sep ()
    (st:should be vos-equal
               #("" "a" "b" "c" "d")
               (%split-on-string " " " a b c d")))
  (st:deftest string-split-as-expected-with-trailing-sep ()
    (st:should be vos-equal
               #("a" "b" "c" "d" "")
               (%split-on-string " " "a b c d ")))
  (st:deftest string-split-multichar-as-expected ()
    (st:should be vos-equal
               #("a" "b" "c" "d")
               (%split-on-string "  " "a  b  c  d")))
  (st:deftest string-split-as-expected ()
    (st:should be vos-equal
               #("a" "b" "c" "d")
               (%split-on-string " " "a b c d")))

  (st:deftest char-split-as-expected-with-test ()
    (st:should be vos-equal
               #("a" "a" "a" "a")
               (%split-on-char #\b "abababa" :test #'char-equal)))
  (st:deftest char-split-as-expected-with-count ()
    (st:should be vos-equal
               #("a" "b c d")
               (%split-on-char #\space "a b c d" :count 2)))
  (st:deftest char-split-as-expected-with-consecutive-sep ()
    (st:should be vos-equal
               #("a" "b" "" "c" "d")
               (%split-on-char #\space "a b  c d")))
  (st:deftest char-split-as-expected-with-leading-and-trailing-sep ()
    (st:should be vos-equal
               #("" "a" "b" "c" "d" "")
               (%split-on-char #\space " a b c d ")))
  (st:deftest char-split-as-expected-with-leading-sep ()
    (st:should be vos-equal
               #("" "a" "b" "c" "d")
               (%split-on-char #\space " a b c d")))
  (st:deftest char-split-as-expected-with-trailing-sep ()
    (st:should be vos-equal
               #("a" "b" "c" "d" "")
               (%split-on-char #\space "a b c d ")))
  (st:deftest char-split-as-expected ()
    (st:should be vos-equal
               #("a" "b" "c" "d")
               (%split-on-char #\space "a b c d")))
  )
