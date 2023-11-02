# Hypergraph
This is a simple implemnetation of a hypergraph data structure which uses hash tables to store edges and vertices.

## Usage
```lisp
(use-package :hypergraph)
(defvar *graph* (make-graph)
(add-vertex *graph* :key 'v1 :value 1)
(add-vertex *graph* :key 'v2 :value 2)
(add-edge *graph* :key 'e1 :vertices '(v1 v2) :value 10))
(edge-nary-vertices 1 'e1 *graph*)
> (v1 v2)
```
### Interface
`(make-graph &key vertex-count edge-count)`
Return a new empty graph.

#### Vertices
`(add-vertex graph &key key value)`
Add a new vertex and return its key. A new key will be generated if none is supplied.

`(vertexp vertex graph)`
True if vertex is present in graph.

`(vertex-value vertex graph)`
Return the value associated with vertex.

`(setf (vertex-value vertex graph) value)`
Set the value associated with vertex.

`(vertex-edges vertex graph)`
Return a list of edges connected to vertex.

`(vertex-edge-values vertex graph)`
Return a list of the values of the edges connected to vertex.

`(vertex-edge-count vertex graph)`
Return the number of edges connected to vertex.

`(vertex-nary-edges n vertex graph)`
Return a list of edges connected to vertex which are connected to exactly n vertices (including vertex).

`(vertex-nary-edge-values n vertex graph)`
Return a list of the values of edges connected to vertex which are connected to exactly n vertices (including vertex).

`(graph-vertices graph)`
Return a list of all vertices.

`(vertex-count graph)`
Return the number of vertices.

#### Edges
`(add-edge graph &key key value vertices)`
Add a new edge and return its key. A new key will be generated if none is supplied.

`(edgep edge graph)`
True if edges is present in graph.
  (presentp edge (cdr graph)))`

`(edge-value edge graph)`
Return the value associated with edge.

`(setf (edge-value edge graph) value)`
Set the value associated with edge.

`(edge-vertices (edge graph)`
Return a list of vertices connected to edge.
  (cdr (get-edge-data edge graph)))`

`(edge-vertex-values edge graph)`
Return a list of the values of the vertices connected to edge.

`(edge-vertex-count edge graph)`
Return the number of vertices connected to edge.

`(edge-nary-vertices n edge graph)`
Return a list of vertices connected to edge which are connected to exactly n edges (including edge).

`(edge-nary-vertex-values n edge graph)`
Return a list of the values of the vertices connected to edge which are connected to exactly n edges (including edge).

`(graph-edges graph)`
Return a list of all edges.

`(edge-count graph)`
Return the number of edges.


`(link-vertex-edge vertex edge graph)`
Connect a vertex with an edge.

`(linkedp vertex edge graph)`
True if vertex and edge are connected.
