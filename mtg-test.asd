(defsystem "mtg-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("mtg"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "mtg"))))
  :description "Test system for mtg"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
