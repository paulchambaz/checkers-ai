(in-package :asdf-user)

(defsystem "checkers-ai"
  :version "0.1.0"
  :author "Moncef Bouhabei, Paul Chambaz"
  :description "A very simple ai for the game of checkers"
  :license "GPLv3"
  :depends-on (:sdl2 :uiop)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "main")
                             (:file "constants")
                             (:file "interface")
                             (:file "logic")
                             (:file "intelligence")
                             (:file "training"))))
  :in-order-to ((test-op (test-op "checkers-ai/tests")))
  :build-operation "program-op"
  :build-pathname "checkers-ai"
  :entry-point "checkers-ai:main")

(defsystem "checkers-ai/tests"
  :author "Moncef Bouhabei, Paul Chambaz"
  :license "GPLv3"
  :description "Test system for checkers-ai"
  :depends-on ("checkers-ai"
               "rove")
  :components ((:module "tests"
                :components ((:file "main_test")
                             (:file "constants_test")
                             (:file "interface_test")
                             (:file "logic_test")
                             (:file "intelligence_test")
                             (:file "training_test"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
