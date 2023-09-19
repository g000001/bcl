;;;; bcl.lisp

(bcl::in-sub-package)


#-lispworks
(cl:defpackage #:bcl-user
  (:use #:bcl))

#+lispworks
(cl:defpackage #:bcl-user
  (:use #:bcl :capi)
  (:export #:d))


;;; *EOF*
