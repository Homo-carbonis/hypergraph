(defsystem hypergraph 
  :author "Hugh Coleman"
  :version "0.1"
  :description "Hypergraph data structure"
  :class :package-inferred-system
  :depends-on ("hypergraph/hypergraph")
  :in-order-to ((test-op (load-op "hypergraph/test")))
  :perform (test-op (op c) (symbol-call :rove :run-suite :hypergraph/test)))


(register-system-packages "hypergraph/utils/misc" '(:misc-utils))
(register-system-packages "hypergraph/utils/hash" '(:hash-utils))

