(defpackage #:hypergraph/test
  (:use #:cl #:misc #:hypergraph #:fiveam))
(in-package #:hypergraph/test)

(def-suite* hypergraph-test)

(test vertex-test
  (let ((g (make-graph 2)))
    (add-vertex g 'v1 1)
    (add-vertex g 'v2 2)
    (is (eq (vertex-value g 'v1) 1))
    (signals vertex-not-found-error (vertex-value g 'v3))
    (is (unordered-equal '(v1 v2) (graph-vertices g)))))

(test edge-test
  (let ((g (make-graph 2)))
      (add-vertex g 'v1 1)
      (add-vertex g 'v2 2)
    (let ((edge (add-edge g '(v1 v2) 1)))
        (is (eq (edge-value edge) 1)) 
        (is (eq edge (car (vertex-edges g 'v1))))
        (is (eq edge (car (vertex-edges g 'v2)))))))
