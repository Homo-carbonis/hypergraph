(defpackage :hypergraph/utils/misc
  (:nicknames :misc-utils)
  (:use :cl) 
  (:export :on :this :unordered-equal))

(in-package #:hypergraph/utils/misc)

(defmacro on (init-form &body body)
  "Bind `this` to `init-form`, evalutate `body` and then return `this`."
  `(let ((this ,init-form))
     ,@body
     this))

(defun unordered-equal (list-1 list-2)
  "Return t if list-1 and list-2 have the same elements arranged in any order."
  (labels ((hash-sort (list)
             (sort (mapcar #'sxhash list) #'<)))
    (equal (hash-sort list-1) (hash-sort list-2))))
