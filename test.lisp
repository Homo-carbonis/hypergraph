(defpackage #:hypergraph/test
  (:use #:cl #:hypergraph #:fiveam))
(in-package #:hypergraph/test)

(defsuite* hypergraph)
(test edge-test
  (let ((g (hypergraph:make-graph 2)))
      (add-vertex g 'a 1)
      (add-vertex g 'b 2)
      (ok (eq (vertex-value g 'a) 1))
      (ok (eq (vertex-value g 'b) 2))
    (testing "add edge"
      (let ((edge (add-edge g 1 '(a b))))
        (ok (eq edge (car (vertex-edges 'a))))
        (ok (eq edge (car (vertex-edges 'b))))))))