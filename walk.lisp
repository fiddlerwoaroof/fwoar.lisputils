(in-package #:fwoar.lisputils)

(declaim (inline reuse-cons))
(defun reuse-cons (x y x-y)
  "If X and Y are the car and cdr of X-Y, return X-Y.

Otherwise, return a fresh cons of X and Y."
  (if (and (eq x (car x-y))
           (eq y (cdr x-y)))
      x-y
      (cons x y)))

(defun map-tree* (fun tree &optional (tag nil tagp))
  "Walk FUN over TREE and build a tree from the results.

The new tree may share structure with the old tree.

     (eq tree (map-tree #'identity tree)) => T

FUN can skip the current subtree with (throw TAG SUBTREE), in which
case SUBTREE will be used as the value of the subtree."
  (declare (optimize (speed 3) (safety 1)))
  (let ((fun (alexandria:ensure-function fun)))
    (labels ((map-tree (tree)
               (let ((tree2 (funcall fun tree)))
                 (if (atom tree2)
                     tree2
                     (reuse-cons (map-tree (car tree2))
                                 (map-tree (cdr tree2))
                                 tree2))))
             (map-tree/tag (tree tag)
               (catch tag
                 (let ((tree2 (funcall fun tree)))
                   (if (atom tree2)
                       tree2
                       (reuse-cons (map-tree/tag (car tree2) tag)
                                   (map-tree/tag (cdr tree2) tag)
                                   tree2))))))
      (if tagp
          (map-tree/tag tree tag)
          (map-tree tree)))))

(defun replace-subtree (predicate value tree)
  (let ((spliced-value nil))
    (flet ((mapper (x)
             (typecase x
               (cons
                (if (funcall predicate x)
                    (progn
                      (setf spliced-value x)
                      (throw 'bail value))
                    x))
               (t x))))
      (let ((result (map-tree* #'mapper tree 'bail)))
        (values result spliced-value)))))
