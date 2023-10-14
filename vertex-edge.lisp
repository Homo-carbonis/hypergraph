(defpackage :hypergraph/vertex-edge
  (:nicknames :ve-hypergraph)
  (:use :cl :misc-utils :hash-utils)
  (:export
    :make-ve-graph
    :add-vertex
    :vertexp
    :vertex-value
    :vertex-edges
    :vertex-edge-count
    :vertex-nary-edges
    :graph-vertices
    :vertex-count
    :add-edge
    :edgep
    :edge-value
    :edge-vertices
    :edge-vertex-count
    :edge-nary-vertices
    :graph-edges
    :edge-count
    :key-absent-error))

(in-package :hypergraph/vertex-edge)

(defun make-ve-graph (&key edge-count vertex-count)
  (cons 
   (make-hash-table :size vertex-count)
   (make-hash-table :size edge-count)))


;;; Vertex functions
(defun add-vertex (graph &key key value)
  (add-hash (car graph) :key key :value (cons value nil))
  key)

(defun vertexp (vertex graph)
  (presentp vertex (car graph)))

(defun get-vertex-data (vertex graph)
  (get-hash vertex (car graph)))

(defun vertex-value (vertex graph)
  (car (get-vertex-data vertex graph)))

(defun vertex-edges (vertex graph)
  (cdr (get-vertex-data vertex graph)))

(defun vertex-edge-count (vertex graph)
  (length (vertex-edges vertex graph)))

(defun vertex-nary-edges (n vertex graph)
  (remove-if-not (lambda (edge) (= n (edge-vertex-count edge graph)))
                 (vertex-edges vertex graph)))

(defun graph-vertices (graph)
  (hash-table-keys (car graph)))

(defun vertex-count (graph)
  (hash-table-count (car graph)))


;;; Edge functions
(defun add-edge (graph &key key value vertices)
  (add-hash (cdr graph) :key key :value (cons value vertices))
  (dolist (n vertices)
    (pushnew key (cdr (get-hash n (car graph)))))
  key)

(defun edgep (edge graph)
  (presentp edge (cdr graph)))

(defun get-edge-data (edge graph)
  (get-hash edge (cdr graph)))

(defun edge-value (edge graph)
  (car (get-edge-data edge graph)))

(defun edge-vertices (edge graph)
  (cdr (get-edge-data edge graph)))

(defun edge-vertex-count (edge graph)
  (length (edge-vertices edge graph)))

(defun edge-nary-vertices (n edge graph)
  (remove-if-not (lambda (vertex) (= n (vertex-edge-count vertex graph)))
                 (edge-vertices edge graph)))

(defun graph-edges (graph)
  (hash-table-keys (cdr graph)))

(defun edge-count (graph)
  (hash-table-count (cdr graph)))
