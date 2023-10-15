(defpackage :hypergraph/vertex-edge
  (:nicknames :ve-hypergraph)
  (:import-from :alexandria :rcurry)
  (:use :cl :misc-utils :hash-utils)
  (:export
    :make-ve-graph
    :add-vertex
    :vertexp
    :vertex-value
    :vertex-edges
    :vertex-edge-values
    :vertex-edge-count
    :vertex-nary-edges
    :vertex-nary-edge-values
    :graph-vertices
    :vertex-count
    :add-edge
    :edgep
    :edge-value
    :edge-vertices
    :edge-vertex-values
    :edge-vertex-count
    :edge-nary-vertices
    :edge-nary-vertex-values
    :graph-edges
    :edge-count
    :link-vertex-edge
    :linkedp
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

(defun (setf vertex-value) (value vertex graph)
  (setf (car (get-vertex-data vertex graph)) value))

(defun vertex-edges (vertex graph)
  (cdr (get-vertex-data vertex graph)))

(defun (setf vertex-edges) (edges vertex graph)
  (setf (cdr (get-vertex-data vertex graph)) edges))

(defun vertex-edge-values (vertex graph)
  (mapcar (rcurry #'edge-value graph) (vertex-edges vertex graph)))

(defun vertex-edge-count (vertex graph)
  (length (vertex-edges vertex graph)))

(defun vertex-nary-edges (n vertex graph)
  (remove-if-not (lambda (edge) (= n (edge-vertex-count edge graph)))
                 (vertex-edges vertex graph)))

(defun vertex-nary-edge-values (n vertex graph)
  (mapcar (rcurry #'edge-value graph) (vertex-nary-edges n vertex graph)))

(defun graph-vertices (graph)
  (hash-table-keys (car graph)))

(defun vertex-count (graph)
  (hash-table-count (car graph)))


;;; Edge functions
(defun add-edge (graph &key key value vertices)
  (add-hash (cdr graph) :key key :value (cons value vertices))
  (dolist (v vertices)
    (pushnew key (vertex-edges v graph)))
  key)

(defun edgep (edge graph)
  (presentp edge (cdr graph)))

(defun get-edge-data (edge graph)
  (get-hash edge (cdr graph)))

(defun edge-value (edge graph)
  (car (get-edge-data edge graph)))

(defun (setf edge-value) (value edge graph)
  (setf (car (get-edge-data edge graph)) value))

(defun edge-vertices (edge graph)
  (cdr (get-edge-data edge graph)))

(defun (setf edge-vertices) (vertices edge graph)
  (setf (cdr (get-edge-data edge graph)) vertices))

(defun edge-vertex-values (edge graph)
  (mapcar (rcurry #'vertex-value graph) (edge-vertices edge graph)))

(defun edge-vertex-count (edge graph)
  (length (edge-vertices edge graph)))

(defun edge-nary-vertices (n edge graph)
  (remove-if-not (lambda (vertex) (= n (vertex-edge-count vertex graph)))
                 (edge-vertices edge graph)))

(defun edge-nary-vertex-values (n edge graph)
  (mapcar (rcurry #'vertex-value graph) (edge-nary-vertices n edge graph)))

(defun graph-edges (graph)
  (hash-table-keys (cdr graph)))

(defun edge-count (graph)
  (hash-table-count (cdr graph)))


(defun link-vertex-edge (vertex edge graph)
  (pushnew vertex (edge-vertices edge graph))
  (pushnew edge (vertex-edges vertex graph)))

(defun linkedp (vertex edge graph)
  (member vertex (edge-vertices edge graph)))
