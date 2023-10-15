(defpackage #:hypergraph/test
  (:use #:cl #:misc-utils #:hypergraph #:rove))

(in-package #:hypergraph/test)

(deftest ve-test
  (let ((g (make-ve-graph :vertex-count 2 :edge-count 0)))
    (add-vertex g :key 'v1 :value 1)
    (add-vertex g :key 'v2 :value 2)
    (add-edge g :key 'e1 :vertices '(v1 v2) :value 10)
    (ok (vertexp 'v1 g))
    (ng (vertexp 'e1 g))
    (ok (equal (vertex-value 'v1 g) 1))
    (setf (vertex-value 'v1 g) 3)
    (ok (equal (vertex-value 'v1 g) 3))
    (signals (vertex-value 'asdhfjkl g) 'key-absent-error)
    (ok (equal (vertex-edges 'v1 g) '(e1)))
    (ok (equal (vertex-edges 'v2 g) '(e1)))
    (ok (equal (vertex-edge-values 'v2 g) '(10)))
    (ok (equal (vertex-edge-count 'v1 g) 1))
    (ok (equal (vertex-nary-edges 2 'v1 g) '(e1)))
    (ok (equal (vertex-nary-edges 1 'v1 g) '()))
    (ok (equal (vertex-nary-edge-values 2 'v1 g) '(10)))
    (ok (unordered-equal (graph-vertices g) '(v1 v2)))
    (ok (equal (vertex-count g) 2))
    (ok (edgep 'e1 g))
    (ng (edgep 'v1 g))
    (ok (equal (edge-value 'e1 g) 10))
    (setf (edge-value 'e1 g) 11)
    (ok (equal (edge-value 'e1 g) 11))
    (signals (edge-value 'djafal g) 'key-absent-error)
    (ok (unordered-equal (edge-vertices 'e1 g) '(v1 v2)))
    (ok (unordered-equal (edge-vertex-values 'e1 g) '(3 2)))
    (ok (equal (edge-vertex-count 'e1 g) 2))
    (ok (unordered-equal (edge-nary-vertices 1 'e1 g) '(v1 v2)))
    (ok (unordered-equal (edge-nary-vertex-values 1 'e1 g) '(3 2)))
    (ok (equal (edge-nary-vertices 2 'e1 g) '()))
    (ok (equal (graph-edges g) '(e1)))
    (ok (equal (edge-count g) 1))
    (ok (unordered-equal '(v1 v2) (graph-vertices g)))
    (add-vertex g :key 'v3)
    (link-vertex-edge 'v3 'e1 g)
    (ok (member 'e1 (vertex-edges 'v3 g)))
    (ok (member 'v3 (edge-vertices 'e1 g)))
    (ok (linkedp 'v1 'e1 g))
    (add-vertex g :key 'v4)
    (ng (linkedp 'v4 'e1 g))))
