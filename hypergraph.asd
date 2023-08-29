(defsystem hypergraph 
  :author "Hugh Coleman"
  :version "0.1"
  :description "Hypergraph data structure"
  :components ((:file "hypergraph"))
  :in-order-to ((test-op (test-op "hypergraph/test"))))


(defsystem hypergraph/test
  :depends-on ("hypergraph" "fiveam")
  :perform (test-op (op c) (symbol-call :rove :run c))
  :components ((:file "test")))