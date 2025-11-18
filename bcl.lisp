;;;; bcl.lisp

(bcl::in-sub-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
(cl:defun expand-internal-ds (xs)
  (typecase xs
    (atom xs)
    ((cons (cons (eql bcl::de) *) *)
     `((labels (,(cdr (expand-internal-ds (car xs))))
         ,@(expand-internal-ds (cdr xs)))))
    ((cons (cons (eql bcl::dm) *) *)
     `((macrolet (,(cdr (expand-internal-ds (car xs))))
         ,@(expand-internal-ds (cdr xs)))))
    ((cons (cons (eql bcl::dv) (cons cons *)) *)
     `((cl:multiple-value-bind ,(nth 1 (car xs)) ,(expand-internal-ds (nth 2 (car xs)))
         ,@(nthcdr 3 (car xs))
         ,@(expand-internal-ds (cdr xs)))))
    ((cons (cons (eql bcl::dv) *) *)
     `((cl:let ((,(nth 1 (car xs)) ,(expand-internal-ds (nth 2 (car xs)))))
         ,@(nthcdr 3 (car xs))
         ,@(expand-internal-ds (cdr xs)))))
    ((cons cons *)
     (cons (expand-internal-ds (car xs))
           (expand-internal-ds (cdr xs))))
    (T (cons (car xs)
             (expand-internal-ds (cdr xs)))))))


(defmacro bcl:defun (name (&rest args) &body body)
  `(cl:defun ,name (,@args)
     ,@(expand-internal-ds body)))


(defmacro bcl:lambda ((&rest args) &body body)
  `(cl:lambda (,@args)
     ,@(expand-internal-ds body)))


(deftype bcl:or (&rest args)
  `(cl:or ,@args))


(defmacro ^ ((&rest args) &body body)
  `(bcl:lambda (,@args) ,@body))


(defmacro λ ((&rest args) &body body)
  `(bcl:lambda (,@args) ,@body))


(macrolet ((def^ (name)
             `(progn
                (defmacro ,(intern (concatenate 'string "^" (string name))) (&body body)
                  `(bcl:lambda (,(intern ,(string name))) ,@body))
                (defmacro ,(intern (concatenate 'string (string 'λ) (string name))) (&body body)
                  `(bcl:lambda (,(intern ,(string name))) ,@body)))))
  (def^ a)
  (def^ b)
  (def^ c)
  (def^ d)
  (def^ e)
  (def^ f)
  (def^ g)
  (def^ h)
  (def^ i)
  (def^ j)
  (def^ k)
  (def^ l)
  (def^ m)
  (def^ n)
  (def^ o)
  (def^ p)
  (def^ q)
  (def^ r)
  (def^ s)
  (def^ t)
  (def^ u)
  (def^ v)
  (def^ w)
  (def^ x)
  (def^ y)
  (def^ z))


(defmacro fun ((&rest args) &body body)
  `(function (cl:lambda (,@args) ,@body)))


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
              (cl:if (macro-function s)
                  (setf (macro-function name)
                        (macro-function s))
                  (setf (fdefinition name)
                        (fdefinition s)))))
      :finally (progn
                 (setf (fdefinition 're.apropos-list)
                       (fdefinition 'cl-ppcre:regex-apropos-list))
                 (setf (fdefinition 're.apropos)
                       (fdefinition 'cl-ppcre:regex-apropos))
                 #|(setf (fdefinition 're.replace)
                         (fdefinition 'cl-ppcre:regex-replace))|#
                 (setf (fdefinition 're.replace-all)
                       (fdefinition 'cl-ppcre:regex-replace-all))))


(cl:defun re.replace (regex target-string replacement
                         &key
                         count
                         (start 0)
                         (end (length target-string))
                         preserve-case
                         simple-calls
                         (element-type 'character))
  (let ((pos-list '())
        (reg-list '()))
    (ppcre:do-scans (match-start match-end reg-starts reg-ends regex target-string
                                 nil
                                 :start start :end end)
      (push match-start pos-list)
      (push match-end pos-list)
      (push reg-starts reg-list)
      (push reg-ends reg-list))
    (if (and count (<= (* 2 count) (length pos-list)))
        (setq pos-list (subseq (nreverse pos-list) 0 (* 2 count))
              reg-list (subseq (nreverse reg-list) 0 (* 2 count)))
        (setq pos-list (nreverse pos-list)
              reg-list (nreverse reg-list)))
    (if pos-list
        (values (ppcre::replace-aux target-string replacement
                                    pos-list
                                    reg-list
                                    start end preserve-case
                                    simple-calls element-type)
                t)
        (values (subseq target-string start end)
                nil))))


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


(defmacro bcl::aq (class-name &rest args)
  `(make-instance ',class-name ,@args))


(eval-always
  (setf (find-class 'bcl::json)
        (find-class 'st-json::jso)))


(declaim (inline fstring))


(cl:defun fstring (control &rest args)
  (apply #'format nil control args))


(cl:defun fpathname (control &rest args)
  (pathname (apply #'fstring control args)))


(cl:defun openi (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file :direction :input
        :element-type element-type
        :external-format external-format))


(cl:defun openo (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file
        :direction :output
        :element-type element-type
        :external-format external-format
        :if-exists :supersede
        :if-does-not-exist :create))


(cl:defun opena (file &key (element-type 'cl:character) (external-format :utf-8))
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
 (defvar bcl::*foreign-syntax-list* (make-hash-table :test #'equal))
 
 (cl:defun read-foreign-syntax (srm chr)
   (declare (ignore chr))
   (let ((syntax-designator (loop :for c := (read-char srm nil)
                                  :until (or (null c)
                                             (eql #\| c))
                                  :collect c)))
     (unread-char #\| srm)
     (let ((expr (string-trim #(#\Newline #\Return #\Space #\Tab) (coerce syntax-designator 'string))))
       (funcall (cl:gethash expr bcl::*foreign-syntax-list* #'values)
                (string (car (read-delimited-list #\] srm)))))))
 
 (setf (cl:gethash "" bcl::*foreign-syntax-list*)
       (cl:lambda (s) s))
 (setf (cl:gethash "cl" bcl::*foreign-syntax-list*)
       #'read-from-string))


(bcl::eval-always
 (cl:defun read-raw-string (srm chr arg)
   (declare (ignore chr arg))
   (with-output-to-string (out)
     (loop 
      (let ((c (read-char srm T nil T)))
        (cl:if (eql #\" c)
            (cl:if (eql #\" (peek-char nil srm T nil T))
                (progn
                  (write-char #\" out)
                  (read-char srm T nil T))
                (return))
            (write-char c out))))))


 (cl:defun concat-string (srm chr arg)
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
   (cl:set-macro-character (code-char 3) ;;#\ETX 
                           (cl:lambda (s c) 
                             (declare (ignore c)) 
                             (loop (or (read-char s nil) 
                                       (return))) 
                             (values)))
   (cl:set-syntax-from-char #\] #\))
   (cl:set-macro-character #\[ #'read-foreign-syntax)
   (make-dispatch-macro-character #\! T)
   (cl:set-dispatch-macro-character #\! #\" #'read-raw-string)
   (cl:set-dispatch-macro-character #\# #\" #'concat-string)
   (cl:set-dispatch-macro-character
    #\! #\(
    (cl:lambda (srm chr arg)
      (declare (ignore chr arg))
      (cons 'cl:funcall (read-delimited-list #\) srm T))))
   (cl:set-dispatch-macro-character #\# #\Z
                                 #'zrseriesi::series-reader)
   (cl:set-dispatch-macro-character #\# #\M
                                 #'zrseriesi::abbreviated-map-fn-reader)
   (cl:set-dispatch-macro-character
    #\# #\@
    (cl:lambda (srm chr arg)
      (declare (ignore chr arg))
      `(eval-always
        ,(read srm T nil T))))
   (cl:set-dispatch-macro-character #\# #\;
                                    #'srfi-62:s-expression-comments)
   (cl:set-dispatch-macro-character
    #\# (character "")
    (cl:lambda (srm chr arg)
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
    ,@(reduce (cl:lambda (res b)
                (append res (->loop-clause b)))
              body
              :initial-value nil)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun ->loop-clause (xpr)
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


(prog ()
  (defmacro bcl:← (&rest args)
    `(setf ,@args)))


(cl:defun bcl:object-named (name)
  (cl:or (find-class name (not :errorp))
         ))


(defmacro bcl:$ (name)
  `(object-named ',name))


(defmacro bcl:/* (&body body) 
  `(or *load-pathname* 
       *compile-file-pathname* 
       (eval-when (:execute) 
         ,@body)))


#|)(defmacro bcl:def (name (&rest args) 
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
       ,(and out `(defmethod ,name :out (,@args) (cl:lambda (,@result-vars) ,@out))))))|#

(defmacro bcl:define-bcl-package (name &rest options)
  `(cl:defpackage ,name
     (:shadowing-import-from #:bcl #:do #:dotimes #:defpackage #:dolist #:or #:map)
     ,@options))


(cl:defun bcl:magical-increment (str)
  (labels ((inc (c)
             (cond ((char= c #\9) (values #\0 t))
                   ((char= c #\Z) (values #\A t))
                   ((char= c #\z) (values #\a t))
                   (T (values (code-char (+ (char-code c) 1)) nil))))
           (rec (accum cs lastchar)
             (cond ((null cs)
                    (cons (cl:if (digit-char-p lastchar) #\1 lastchar)
                          accum))
                   ((alphanumericp (car cs))
                    (multiple-value-bind (next carry)
                                         (inc (car cs))
                      (cl:if carry
                          (rec (cons next accum)
                               (cdr cs)
                               next)
                          (append (reverse (cons next (cdr cs)))
                                  accum))))
                   (T (rec (cons (car cs) accum)
                           (cdr cs)
                           lastchar)))))
    (assert (some #'alphanumericp str)
            (str)
            "argument must contain at least one alphanumeric character, but got ~a" str)
    (coerce (rec '() (coerce (reverse str) 'list) #\Null)
            'string)))


(cl:defun bcl:tree-walk (walker proc tree) 
  (funcall walker (cl:lambda (elt) 
                    (cl:if (typep elt 'list) 
                        (bcl:tree-walk walker proc elt) 
                        (funcall proc elt))) 
           tree))


(let (#+lispworks (hcl:*packages-for-warn-on-redefinition* nil))
  (defmacro :bcl ()
    `(bcl:eval-always
       (cl:in-package bcl)
       (bcl:in-syntax bcl:*bcl*)))
  (defmacro :bcl-user ()
    `(bcl:eval-always
       (cl:in-package bcl-user)
       (bcl:in-syntax bcl:*bcl*)))
  (defmacro bcl:bcl ()
    `(bcl:eval-always
       (cl:in-package bcl)
       (bcl:in-syntax bcl:*bcl*))))


(defmacro bcl:zlet ((&rest specbind) &body body)
  `(zrseries:let (,@specbind) ,@body))


(defmacro bcl:zlet* ((&rest specbind) &body body)
  `(zrseries:let* (,@specbind) ,@body))


(defmacro bcl:multiple-value-zbind (vars vals &body body)
  `(zrseries:multiple-value-bind ,vars ,vals ,@body))


;; the if* macro used in Allegro:
;;
;; This is in the public domain... please feel free to put this definition
;; in your code or distribute it with your version of lisp.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar if*-keyword-list '("then" "thenret" "else" "elseif")))


(defmacro bcl:if (&rest args)
  (cl:if (notany (cl:lambda (x)
                   (find x if*-keyword-list
                         :test (cl:lambda (x y)
                                 (string-equal (princ-to-string x)
                                               (princ-to-string y)))))
                 args)
         `(cl:if ,@args)
         ;;`(cl:if ,@args)
         (cl:do ((xx (reverse args) (cdr xx))
                 (state :init)
                 (elseseen nil)
                 (totalcol nil)
                 (lookat nil nil)
                 (col nil))
                ((null xx)
                 (cond ((eq state :compl)
                        `(cond ,@totalcol))
                       (t (error "if*: illegal form ~s" args))))
           (cond ((and (typep (car xx) 'symbol)
                       (member (symbol-name (car xx))
                               if*-keyword-list
                               :test #'string-equal))
                  (setq lookat (symbol-name (car xx)))))

           (cond ((eq state :init)
                  (cond (lookat (cond ((string-equal lookat "thenret")
                                       (setq col nil
                                             state :then))
                                      (t (error
                                          "if*: bad keyword ~a" lookat))))
                        (t (setq state :col
                                 col nil)
                           (push (car xx) col))))
                 ((eq state :col)
                  (cond (lookat
                         (cond ((string-equal lookat "else")
                                (cond (elseseen
                                       (error
                                        "if*: multiples elses")))
                                (setq elseseen t)
                                (setq state :init)
                                (push `(t ,@col) totalcol))
                               ((string-equal lookat "then")
                                (setq state :then))
                               (t (error "if*: bad keyword ~s"
                                         lookat))))
                        (t (push (car xx) col))))
                 ((eq state :then)
                  (cond (lookat
                         (error
                          "if*: keyword ~s at the wrong place " (car xx)))
                        (t (setq state :compl)
                           (push `(,(car xx) ,@col) totalcol))))
                 ((eq state :compl)
                  (cond ((not (string-equal lookat "elseif"))
                         (error "if*: missing elseif clause ")))
                  (setq state :init))))
         ))


(defmacro bcl:when (&whole whole pred &body body)
  (typecase (nth 1 whole)
    ((member bcl:let bcl:let* :let :let*)
     (destructuring-bind (when let bindspec &rest body)
                         whole
       (declare (ignore when))
       `(,let ,bindspec
              (cl:when (and ,@(mapcar #'car bindspec)) ,@body))))
    (symbol
     (if (string= 'it (string (nth 1 whole)))
         (destructuring-bind (when it pred &rest body)
                             whole
           (declare (ignore when))
           `(cl:let ((,it ,pred))
              (cl:when ,it ,@body)))
         `(cl:when ,pred ,@body)))
    (T `(cl:when ,pred ,@body))))


(defmacro bcl:it (val &body body)
  `(cl:let ((bcl:it ,val))
     ,@body))


;;; *EOF*
