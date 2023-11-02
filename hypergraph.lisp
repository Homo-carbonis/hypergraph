(defpackage :hypergraph
  (:import-from :alexandria :rcurry)
  ;(:use :cl :misc-utils :hash-utils)
  (:use :cl :hypergraph/utils/hash)
  (:export
    :make-graph
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

(in-package :hypergraph)

(defun make-graph (&key vertex-count edge-count)
  "Return a new empty graph."
  (cons 
    (if vertex-count
        (make-hash-table :size vertex-count)
        (make-hash-table)) 
    (if edge-count
        (make-hash-table :size edge-count)
        (make-hash-table))))

;;; Vertex functions
(defun add-vertex (graph &key key value)
  "Add a new vertex and return its key. A new key will be generated if none is supplied."
  (add-hash (car graph) :key key :value (cons value nil))
  key)

(defun vertexp (vertex graph)
  "True if vertex is present in graph."
  (presentp vertex (car graph)))

(defun get-vertex-data (vertex graph)
  (get-hash vertex (car graph)))

(defun vertex-value (vertex graph)
  "Return the value associated with vertex."
  (car (get-vertex-data vertex graph)))

(defun (setf vertex-value) (value vertex graph)
  "Set the value associated with vertex."
  (setf (car (get-vertex-data vertex graph)) value))

(defun vertex-edges (vertex graph)
  "Return a list of edges connected to vertex"
  (cdr (get-vertex-data vertex graph)))

(defun (setf vertex-edges) (edges vertex graph)
  (setf (cdr (get-vertex-data vertex graph)) edges))

(defun vertex-edge-values (vertex graph)
  "Return a list of the values of the edges connected to vertex"
  (mapcar (rcurry #'edge-value graph) (vertex-edges vertex graph)))

(defun vertex-edge-count (vertex graph)
  "Return the number of edges connected to vertex."
  (length (vertex-edges vertex graph)))

(defun vertex-nary-edges (n vertex graph)
  "Return a list of edges connected to vertex which are connected to exactly n vertices (including vertex)."
  (remove-if-not (lambda (edge) (= n (edge-vertex-count edge graph)))
                 (vertex-edges vertex graph)))

(defun vertex-nary-edge-values (n vertex graph)
  "Return a list of the values of edges connected to vertex which are connected to exactly n vertices (including vertex)."
  (mapcar (rcurry #'edge-value graph) (vertex-nary-edges n vertex graph)))

(defun graph-vertices (graph)
  "Return a list of all vertices."
  (hash-table-keys (car graph)))

(defun vertex-count (graph)
  "Return the number of vertices"
  (hash-table-count (car graph)))


;;; Edge functions
(defun add-edge (graph &key key value vertices)
  "Add a new edge and return its key. A new key will be generated if none is supplied."
  (add-hash (cdr graph) :key key :value (cons value vertices))
  (dolist (v vertices)
    (pushnew key (vertex-edges v graph)))
  key)

(defun edgep (edge graph)
  "True if edges is present in graph."
  (presentp edge (cdr graph)))

(defun get-edge-data (edge graph)
  (get-hash edge (cdr graph)))

(defun edge-value (edge graph)
  "Return the value associated with edge."
  (car (get-edge-data edge graph)))

(defun (setf edge-value) (value edge graph)
  "Set the value associated with edge."
  (setf (car (get-edge-data edge graph)) value))

(defun edge-vertices (edge graph)
  "Return a list of vertices connected to edge."
  (cdr (get-edge-data edge graph)))

(defun (setf edge-vertices) (vertices edge graph)
  (setf (cdr (get-edge-data edge graph)) vertices))

(defun edge-vertex-values (edge graph)
  "Return a list of the values of the vertices connected to edge."
  (mapcar (rcurry #'vertex-value graph) (edge-vertices edge graph)))

(defun edge-vertex-count (edge graph)
  "Return the number of vertices connected to edge."
  (length (edge-vertices edge graph)))

(defun edge-nary-vertices (n edge graph)
  "Return a list of vertices connected to edge which are connected to exactly n edges (including edge)."
  (remove-if-not (lambda (vertex) (= n (vertex-edge-count vertex graph)))
                 (edge-vertices edge graph)))

(defun edge-nary-vertex-values (n edge graph)
  "Return a list of the values of the vertices connected to edge which are connected to exactly n edges (including edge)."
  (mapcar (rcurry #'vertex-value graph) (edge-nary-vertices n edge graph)))

(defun graph-edges (graph)
  "Return a list of all edges"
  (hash-table-keys (cdr graph)))

(defun edge-count (graph)
  "Return the number of edges"
  (hash-table-count (cdr graph)))


(defun link-vertex-edge (vertex edge graph)
  "Connect a vertex with an edge."
  (pushnew vertex (edge-vertices edge graph))
  (pushnew edge (vertex-edges vertex graph)))

(defun linkedp (vertex edge graph)
  "True if vertex and edge are connected"
  (member vertex (edge-vertices edge graph)))
