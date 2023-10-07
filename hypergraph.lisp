(defpackage #:hypergraph
  (:use #:cl #:misc #:alexandria)
  (:export #:make-graph #:add-vertex #:vertex-not-found-error #:graph-vertices #:graph-equal #:vertex-value #:vertex-edges #:vertex-edge-values
           #:add-edge #:edge-value #:edge-vertices #:edge-length
           #:vertex-nary-edges #:vertex-nary-edge-values))
(in-package #:hypergraph)

(defun make-graph (&optional size)
  (if size
      (make-hash-table :size size)
      (make-hash-table)))

(defun add-vertex (graph key &optional value)
  (ensure-gethash key graph (list value)))

(defun get-vertex (graph key)
  (or (gethash key graph)
      (error 'vertex-not-found-error :key key)))

(define-condition vertex-not-found-error (error) ((key :initarg :key :reader key)))

(defun graph-vertices (graph)
  (on nil
    (maphash (lambda (k v) (declare (ignore v)) (push k this)) graph)))

(defun vertex-count (graph)
  (hash-table-count graph))


(defun graph-equal (graph-1 graph-2)
  "Return `graph-1` if graphs have the same structure and equal vertex and edge values, else return nil"
  (let ((vertices (graph-vertices graph-1)))
    (and (unordered-equal vertices (graph-vertices graph-2))
         (every (lambda (v) (and (equal (vertex-value graph-1 v) (vertex-value graph-2 v))
                                 (unordered-equal (vertex-edges graph-1 v) (vertex-edges graph-2 v))))
                vertices)))) 
  
(defun vertex-value (graph vertex)
  (car (get-vertex graph vertex)))

(defsetf vertex-value (graph vertex) (value)
  `(setf (car (get-vertex ,graph ,vertex)) ,value))

(defun vertex-edges (graph vertex)
  (cdr (get-vertex graph vertex)))

(defun vertex-edge-values (graph vertex)
  (mapcar #'edge-value (vertex-edges graph vertex)))

(defun add-edge (graph vertices &optional value)
  (on (cons value vertices)
    (dolist (v vertices)
      (pushnew this (cdr (get-vertex graph v))))))

(defun edge-value (edge)
  (car edge))

(defun edge-vertices (edge)
  (cdr edge))

(defun edge-length (edge)
  (length (edge-vertices edge)))

(defun vertex-nary-edges (graph vertex n)
  "Return a list of edges connected to exactly `n` vertices (including `vertex`)"
  (remove-if-not (lambda (e) (= (edge-length e) n)) (vertex-edges graph vertex)))

(defun vertex-nary-edge-values (graph vertex n)
  "Return a list of values of edges connected to exactly `n` vertices (including `vertex`)"
  (mapcar #'edge-value (vertex-nary-edges graph vertex n)))
