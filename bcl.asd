;;;; bcl.asd

(asdf:defsystem #:bcl
  :description "Better CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :license  "MIT"
  :version "0.0.5"
  :serial t
  :depends-on (#:cl-ppcre
               #:allegretto-regexp
               #:zrseries
               #:closer-mop
               #:nil-compat
               #:zrdbc
               #:srfi-2
               #:st-json
               #+lispworks8 #:trivia)
  :components ((:file "package")
               (:file "bcl")
               (:file "with")
               (:file "get")
               (:file "iteration")
               (:file "regex")
               (:file "threadding")
               (:file "bcl-user")))
