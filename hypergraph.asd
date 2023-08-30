(defsystem hypergraph 
  :author "Hugh Coleman"
  :version "0.1"
  :description "Hypergraph data structure"
  :depends-on ("misc" "alexandria")
  :components ((:file "hypergraph"))
  :in-order-to ((test-op (test-op "hypergraph/test"))))

(defsystem hypergraph/test
  :depends-on ("hypergraph" "fiveam")
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :hypergraph-test :hypergraph/test )))
  :components ((:file "test")))