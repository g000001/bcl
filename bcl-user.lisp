;;;; bcl.lisp

(cl:in-package #:bcl.internal)

#-lispworks
(cl:defpackage #:bcl-user
  (:use #:bcl))

#+lispworks
(cl:defpackage #:bcl-user
  (:use #:bcl :capi))


#+lispworks
(bcl::eval-always
  (loop :for s :being :the :external-symbols :of :capi
        :for sym := (intern (concatenate 'string "<" (string s) ">") :bcl-user)
        :when (find-class s nil)
        :do (eval (print `(defconstant ,sym (find-class ',s))))
        :and :collect (copy-symbol sym)))




 




