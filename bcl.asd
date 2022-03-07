;;;; bcl.asd

(asdf:defsystem #:bcl
  :description "Better CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:cl-ppcre #:series #:closer-mop #:nil-compat #:zrdbc)
  :components ((:file "bcl")
               (:file "with")
               (:file "get")
               (:file "iteration")
               (:file "regex")
               (:file "bcl-user")))
