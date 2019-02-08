(defpackage :fwoar.swank-utils
  #+fw.dev
  (:nicknames fw.su)
  (:use :cl )
  (:export
   #:log-json))
(in-package :fwoar.swank-utils)

(defvar *target-identifier* (alexandria:make-keyword (gensym "JSON")))
(defun log-json (obj &optional (indent t) (target-identifier *target-identifier*))
  (let* ((buffer-stream (swank-buffer-streams:make-buffer-output-stream target-identifier))
         (stream (yason:make-json-output-stream buffer-stream :indent indent)))
    (unwind-protect (progn (fresh-line buffer-stream)
                           (values (yason:encode obj stream)
                                   target-identifier))
      (terpri buffer-stream)
      (finish-output stream)
      (finish-output buffer-stream)
      (close stream))))
