(defsystem hypergraph 
  :author "Hugh Coleman"
  :version "0.1"
  :description "Hypergraph data structure"
  :class :package-inferred-system
  :depends-on ("hypergraph/hypergraph")
  :in-order-to ((test-op (load-op "hypergraph/test")))
  :perform (test-op (op c) (symbol-call :rove :run-suite :hypergraph/test)))


(register-system-packages "utils/misc" '(:misc-utils))
(register-system-packages "utils/hash" '(:hash-utils))
(register-system-packages "let-over-lambda" '(:lol))

