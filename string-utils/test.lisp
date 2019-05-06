(in-package :fwoar.string-utils)

(defun vos-equal (a b)
  (and (= (length a)
          (length b))
       (every 'equal a b)))

(progn
  (st:deftest string-split-as-expected-with-test ()
    (st:should be vos-equal
               #("a" "a" "a" "a")
               (%split-on-string "b" "abababa" :test #'string-equal)))
  (st:deftest string-split-as-expected-with-count-1 ()
    (st:should be vos-equal
               #("a" "b c d")
               (%split-on-string " " "a b c d" :count 1)))
  (st:deftest string-split-as-expected-with-count-2 ()
    (st:should be vos-equal
               #("a" "b" "c d")
               (%split-on-string " " "a b c d" :count 2)))
  (st:deftest string-split-as-expected-with-count-3 ()
    (st:should be vos-equal
               #("a" "b" "c" "d")
               (%split-on-string " " "a b c d" :count 3)))
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
  (st:deftest string-split-empty-string-as-expected ()
    (st:should be vos-equal
               #("")
               (%split-on-string "/" "")))

  (st:deftest char-split-as-expected-with-test ()
    (st:should be vos-equal
               #("a" "a" "a" "a")
               (%split-on-char #\b "abababa" :test #'char-equal)))
  (st:deftest char-split-as-expected-with-count-1 ()
    (st:should be vos-equal
               #("a" "b c d")
               (%split-on-char #\space "a b c d" :count 1)))
  (st:deftest char-split-as-expected-with-count-2 ()
    (st:should be vos-equal
               #("a" "b" "c d")
               (%split-on-char #\space "a b c d" :count 2)))
  (st:deftest char-split-as-expected-with-count-3 ()
    (st:should be vos-equal
               #("a" "b" "c" "d")
               (%split-on-char #\space "a b c d" :count 3)))
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

  (st:deftest char-split-empty-string-as-expected ()
    (st:should be vos-equal
               #("")
               (%split-on-char #\/ "")))

  (st:deftest char-split-no-sep-returns-vec-with-contents ()
    (st:should be vos-equal
               #("Bacon")
               (%split-on-char #\. "Bacon")))
  )

(progn
  (unintern 'char-split-as-expected)
  (unintern 'char-split-as-expected-with-consecutive-sep)
  (unintern 'char-split-as-expected-with-test)
  (unintern 'char-split-as-expected-with-count)
  (unintern 'char-split-as-expected-with-count-1)
  (unintern 'char-split-as-expected-with-count-2)
  (unintern 'char-split-as-expected-with-count-3)
  (unintern 'char-split-as-expected-with-leading-and-trailing-sep)
  (unintern 'char-split-as-expected-with-leading-sep)
  (unintern 'char-split-as-expected-with-trailing-sep)
  (unintern 'string-split-as-expected)
  (unintern 'string-split-as-expected-with-consecutive-sep)
  (unintern 'string-split-as-expected-with-test)
  (unintern 'string-split-as-expected-with-count)
  (unintern 'string-split-as-expected-with-count-1)
  (unintern 'string-split-as-expected-with-count-2)
  (unintern 'string-split-as-expected-with-count-3)
  (unintern 'string-split-as-expected-with-leading-and-trailing-sep)
  (unintern 'string-split-as-expected-with-leading-sep)
  (unintern 'string-split-as-expected-with-test)
  (unintern 'string-split-as-expected-with-trailing-sep)
  (unintern 'string-split-empty-string-as-expected)
  (unintern 'string-split-multichar-as-expected)
  (unintern 'CHAR-SPLIT-NO-SEP-RETURNS-VEC-WITH-CONTENTS)
  (unintern 'CHAR-SPLIT-EMPTY-STRING-AS-EXPECTED))
