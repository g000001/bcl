;;;; bcl.lisp

(bcl::in-sub-package)


(deftype bcl:or (&rest args)
  `(cl:or ,@args))


(defmacro ^ ((&rest args) &body body)
  `(function (lambda (,@args) ,@body)))


(defmacro fun ((&rest args) &body body)
  `(function (lambda (,@args) ,@body)))


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


(macrolet ((def<> ()
             `(progn
                (cl:loop :for s :being :the :external-symbols :of :cl
                         :for sym := (intern (concatenate 'string "<" (string s) ">") :bcl)
                         :when (find-class s nil)
                           :collect `(defconstant ,sym (find-class ',s))))))
  (def<>))


(loop :for s :being :the external-symbols :of :ppcre
      :do (when (and (fboundp s)
                     (not (search #.(string '#:regex) (string s))))
            (let ((name (intern (concatenate 'string
                                             (string "RE.")
                                             (string s))
                                :bcl)))
              (if (macro-function s)
                  (setf (macro-function name)
                        (macro-function s))
                  (setf (fdefinition name)
                        (fdefinition s)))))
      :finally (progn
                 (setf (fdefinition 're.apropos-list)
                       (fdefinition 'cl-ppcre:regex-apropos-list))
                 (setf (fdefinition 're.apropos)
                       (fdefinition 'cl-ppcre:regex-apropos))
                 (setf (fdefinition 're.replace)
                       (fdefinition 'cl-ppcre:regex-replace))
                 (setf (fdefinition 're.replace-all)
                       (fdefinition 'cl-ppcre:regex-replace-all))))


(eval-always 
  (setf (fdefinition 'a)
        (fdefinition 'make-instance))
  (setf (fdefinition 'prop)
        (fdefinition 'cl:getf))
  (setf (fdefinition 'isa)
        (fdefinition 'typep))
  (setf (fdefinition 'call)
        (fdefinition 'funcall))
  )


(bcl:eval-always
  (setf (find-class 'bcl::json)
        (find-class 'st-json::jso)))

(declaim (inline fstring))


(defun fstring (control &rest args)
  (apply #'format nil control args))


(defun fpathname (control &rest args)
  (pathname (apply #'fstring control args)))


(defun openi (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file :direction :input
        :element-type element-type
        :external-format external-format))


(defun openo (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file
        :direction :output
        :element-type element-type
        :external-format external-format
        :if-exists :supersede
        :if-does-not-exist :create))


(defun opena (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file
        :direction :output
        :element-type element-type
        :external-format external-format
        :if-exists :append
        :if-does-not-exist :create))


(defmacro in-syntax (readtable)
  `(eval-always
     (setq *readtable* ,readtable)))


(eval-always
 (defun read-raw-string (srm chr arg)
   (declare (ignore chr arg))
   (with-output-to-string (out)
     (loop 
      (let ((c (read-char srm T nil T)))
        (if (eql #\" c)
            (if (eql #\" (peek-char nil srm T nil T))
                (progn
                  (write-char #\" out)
                  (read-char srm T nil T))
                (return))
            (write-char c out))))))


 (defun concat-string (srm chr arg)
   (declare (ignore arg))
   (with-output-to-string (out)
     (unread-char chr srm)
     (loop 
      (let ((s (read srm T nil T)))
        (write-string s out)
        (unless (eql #\" (peek-char T srm T nil T))
          (return))))))


 (defvar *bcl* (copy-readtable nil))


 (let ((*readtable* *bcl*))
   (make-dispatch-macro-character #\! T)
   (cl:set-dispatch-macro-character #\! #\" #'read-raw-string)
   (cl:set-dispatch-macro-character #\# #\" #'concat-string)
   (cl:set-dispatch-macro-character
    #\! #\(
    (lambda (srm chr arg)
      (declare (ignore chr arg))
      (cons 'cl:funcall (read-delimited-list #\) srm T))))
   (cl:set-dispatch-macro-character #\# #\Z
                                 #'zrseriesi::series-reader)
   (cl:set-dispatch-macro-character #\# #\M
                                 #'zrseriesi::abbreviated-map-fn-reader)
   (cl:set-dispatch-macro-character
    #\# #\@
    (lambda (srm chr arg)
      (declare (ignore chr arg))
      `(eval-always
        ,(read srm T nil T))))
   (cl:set-dispatch-macro-character
    #\# (character "")
    (lambda (srm chr arg)
      (declare (ignore chr arg))
      (cons 'eval-always
            (read-delimited-list (character "") srm T)))))
            
 (defconstant bcl-syntax *bcl*))


(defmacro then (&body body)
  `(progn . ,body))


(defmacro else (&body body)
  `(progn . ,body))


(defmacro for (&rest body)
  `(loop 
    ,@(reduce (lambda (res b)
                (append res (->loop-clause b)))
              body
              :initial-value nil)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ->loop-clause (xpr)
    (case (find (car xpr) '(let) :test #'string-equal)
      (let (destructuring-bind (let &rest args)
                               xpr
             (declare (ignore let))
             `(for ,@args)))
      (otherwise xpr))))


(defmacro Zdefun (name (&rest args) &body body)
  `(zrseries:defun ,name (,@args)
     (declare (zrseries:optimizable-series-function))
     ,@body))


(eval-always
  (setf (fdefinition 'bcl::keep-if)
        (fdefinition 'cl:remove-if-not)))


(defgeneric bcl:name (obj)
  (:method ((obj cell-error)) (cell-error-name obj))
  (:method ((obj character)) (char-name obj))
  (:method ((obj class)) (class-name obj))
  (:method ((obj generic-function)) (generic-function-name obj))
  (:method ((obj package)) (package-name obj))
  (:method ((obj restart)) (restart-name obj))
  (:method ((obj slot-definition)) (slot-definition-name obj))
  (:method ((obj symbol)) (symbol-name obj)))


(defvar .csetq-unbound.)


(defmacro bcl:csetq (var val)
  `(progn 
     (define-symbol-macro ,var .csetq-unbound.)
     (setq ,var ,val)))


(defmacro bcl:cset (var val)
  `(progn 
     (setf (symbol-value ,var) ,val)))


(defmacro bcl:classq (name &rest args)
  `(find-class ',name ,@args))


(defmacro bcl:makeq (name &rest args)
  `(make-instance ',name ,@args))


(defmacro bcl:← (&rest args)
  `(setf ,@args))


(defun bcl:object-named (name)
  (cl:or (find-class name (not :errorp))
         ))


(defmacro bcl:$ (name)
  `(object-named ',name))


#|(defmacro bcl:def (name (&rest args) 
                        (&rest values)
                        (&body in)
                        (&body out)
                        &body main)
  (check-type (car in) (eql :in))
  (check-type (car values) (eql values))
  (check-type (car out) (eql :out))
  (let ((in (cdr in))
        (out (cdr out))
        (result-vars (cdr values)))
    `(progn
       (defgeneric ,name (,@args)
         (:method-combination zrdbc:dbc))
       ,(and in `(defmethod ,name :in (,@args) ,@in))
       (defmethod ,name (,@args) ,@main)
       ,(and out `(defmethod ,name :out (,@args) (lambda (,@result-vars) ,@out))))))|#

()


;;; *EOF*
