(defpackage #:hypergraph/base
  (:use #:cl #:misc-utils #:hash-utils)
  (:export #:make-graph #:add-node #:graph-nodes #:node-count #:node-value #:node-nary-neighbours))

(in-package #:hypergraph/base)

(defun make-graph (&optional count)
  (make-hash-table :size count))

(defun add-node (graph &key key value neighbours)
  (on (cons value neighbours)
    (add-hash graph :key key :value this)
    (dolist (n neighbours)
        (pushnew this (cdr (get-hash graph n)))))
  key)

(defun graph-nodes (graph)
    (hash-table-keys graph))

(defun node-count (graph)
  (hash-table-count graph))

(defun node-value (node graph)
  (car (get-hash node graph)))

(defsetf node-value (node graph) (value)
  `(setf (car (get-hash ,node ,graph)) ,value))

(defun node-neighbours (node graph)
  (cdr (get-hash node graph)))

(defun node-neighbour-count (node graph)
  (length (node-neighbours node graph)))

(defun node-nary-neighbours (n node graph)
  "Return a list of neighbours of `node` with exactly `n` neighbours (including `node`)"
  (remove-if-not (lambda (neighbour) (= (node-neighbour-count neighbour graph) n))
                 (node-neighbours node graph)))
