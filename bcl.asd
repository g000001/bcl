;;;; bcl.asd

(asdf:defsystem #:bcl
  :description "Better CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:series #:closer-mop #:nil-compat)
  :components ((:file "bcl")
               (:file "get")
               (:file "bcl-user")))
