(defpackage #:hypergraph/test
  (:use #:cl #:misc #:alexandria #:hypergraph #:fiveam))
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

(test equal-test
  (let ((g1 (make-graph)))
    (add-vertex g1 'v1 1)
    (add-vertex g1 'v2 2)
    (let ((g2 (copy-hash-table g1))
          (g3 (copy-hash-table g1))
          (g4 (copy-hash-table g1))
          (g5 (copy-hash-table g1))
          (g6 (copy-hash-table g1)))
      (is (graph-equal g1 g2))
      (add-edge g1 '(v1 v2) 1)
      (add-edge g2 '(v1 v2) 1)
      (add-edge g3 '(v2 v1) 1)
      (add-edge g4 '(v1) 5)
      (is (graph-equal g1 g2))
      (is (graph-equal g1 g3))
      (print (vertex-edges g4 'v2))
      (is-false (graph-equal g1 g4))
      (add-vertex g2 'v3 3)
      (is-false (graph-equal g1 g2)))))

      






