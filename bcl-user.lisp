;;;; bcl.lisp

(bcl::in-sub-package)


#-lispworks
(cl:defpackage #:bcl-user
  (:use #:bcl))

#+lispworks
(cl:defpackage #:bcl-user
  (:use #:bcl :capi :zreclos)
  (:export #:d)
  (:shadowing-import-from :zreclos defclass))


;;; *EOF*
