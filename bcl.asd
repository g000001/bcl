;;;; bcl.asd

(asdf:defsystem #:bcl
  :description "Better CL"
  :author "CHIBA Masaomi <chiba.masaomi@gmail.com>"
  :license  "MIT"
  :version "2026.01.18"
  :serial t
  :depends-on (#:equal
               #:cl-ppcre
               #:allegretto-regexp
               #:zrseries
               #:zreclos
               #:nil-compat
               #:zrdbc
               #:srfi-2
               #:st-json
               #:srfi-62
               ;#+lispworks8 #:trivia
               )
  :components ((:file "package")
               (:file "bcl")
               (:file "with")
               (:file "get")
               (:file "iteration")
               (:file "regex")
               (:file "threadding")
               (:file "do")
               (:file "pkg")
               (:file "mweq")
               (:file "bcl-user")))
