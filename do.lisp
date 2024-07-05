;;; -*- mode: Lisp; coding: utf-8  -*-

(bcl::in-sub-package)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defgeneric expand-do (type varspec body))

(defmethod expand-do ((type list) varspec body)
  `(cl:do ,type ,varspec ,@body))

(progn
  (defmethod expand-do ((type integer) varspec body)
    (if (eq varspec 'bcl::times)
        `(cl:dotimes (_ ,type)
           (declare (ignore _))
           ,@body)
        (error "Unknown DO keyword: ~S" varspec)))

  (defmethod expand-do ((type symbol) varspec body)
    (if (eq varspec 'bcl::times)
        `(cl:dotimes (_ ,type)
           (declare (ignore _))
           ,@body)
        (error "Unknown DO keyword: ~S" varspec))))

(defmethod expand-do ((type (eql 'list)) varspec body)
  `(cl:dolist ,varspec ,@body))

(defmethod expand-do ((type (eql 'string)) varspec body)
  `(cl:loop :for ,(car varspec) :across ,(cadr varspec) :do ,@body))

(defmethod expand-do ((type (eql 'vector)) varspec body)
  `(cl:loop :for ,(car varspec) :across ,(cadr varspec) :do ,@body))

(defmethod expand-do ((type (eql 'bcl::symbols)) varspec body)
  `(cl:do-symbols ,varspec ,@body))

(defmethod expand-do ((type (eql 'bcl::all-symbols)) varspec body)
  `(cl:do-all-symbols ,varspec ,@body))

(defmethod expand-do ((type (eql 'bcl::times)) varspec body)
  `(cl:dotimes ,varspec ,@body))

(defmethod expand-do ((type (eql 'bcl::forever)) varspec body)
  `(cl:loop ,varspec ,@body))


(defmethod expand-do ((type (eql 'bcl:series)) varspec body)
  `(collect-ignore
    (map-fn T
            (lambda (,(car varspec))
              ,@body)
            ,(cadr varspec))))

(defmethod expand-do ((type (eql 'sequence)) varspec body)
  `(bcl:collect-ignore
    (bcl:map-fn T
                (lambda (,(car varspec))
                  ,@body)
                (bcl:scan ,(cadr varspec)))))

(progn
  (defmethod expand-do ((type (eql 'bcl:jso)) varspec body)
    `(bcl:mapjso (cl:lambda ,(car varspec)
                   ,@body)
                 ,(cadr varspec)))

  (defmethod expand-do ((type (eql 'bcl::json)) varspec body)
    `(bcl:mapjso (cl:lambda ,(car varspec)
                   ,@body)
                 ,(cadr varspec))))
)


(defmacro bcl:do (type varspec &body body)
  (expand-do type varspec body))




#||
(bcl:do ((x '(0 1 2) (cdr x)))
    ((endp x))
  (print x))

(bcl:do list (e '(0 1 2))
  (print e))

(bcl:do bcl::symbols (s 'alexandria)
  (print s))

(let ((x 100))
  (bcl:do x bcl::times
    (print "foo")))


(bcl:do sequence (e '(0 1 2 3))
  (print e))

||#