;;;; bcl.asd

(asdf:defsystem #:bcl
  :description "Better CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :depends-on (#:cl-ppcre #:series #:closer-mop #:nil-compat #:zrdbc #:srfi-2)
  :components ((:file "bcl")
               (:file "with")
               (:file "get")
               (:file "iteration")
               (:file "regex")
               (:file "bcl-user")))
