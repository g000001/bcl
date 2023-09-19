(bcl::in-sub-package)


(defgeneric ref (obj key &optional default))


;; symbol
(defmethod ref ((obj symbol) key &optional default)
  (cl:get obj key default))


(defmethod (setf ref) (val (obj symbol) key &optional default)
  (setf (cl:get obj key default) val))


;; standard-object
(defmethod ref ((obj standard-object) key &optional default)
  (cl:or (slot-value obj key) default))


(defmethod (setf ref) (val (obj standard-object) key
                       &optional default)
  (setf (slot-value obj key) val))


;; hash-table
(defmethod ref ((obj hash-table) key &optional default)
  (cl:gethash key obj default))


(defmethod (setf ref) (val (obj hash-table) key &optional default)
  (setf (cl:gethash key obj default) val))


;; sequence
(defmethod ref ((obj vector) (key integer) &optional default)
  (if (array-in-bounds-p obj key)
      (elt obj key)
      default))


(defmethod (setf ref) (val (obj vector) (key integer) &optional default)
  (setf (elt obj key) val))


;; plist
(defmethod ref ((obj cons) key &optional default)
  (cl:getf obj key default))


(defmethod ref ((obj cons) (key list) &optional default)
  (declare (ignore default))
  (cl:get-properties obj key))


(defmethod (setf ref) (val (obj cons) key &optional default)
  (if (find key obj)
      (setf (cl:getf obj key default) val)
      (let ((ans (append obj (list key val))))
        (setf (car obj) (car ans))
        (setf (cdr obj) (cdr ans))
        obj)))


;; readtable
(defmethod ref ((obj readtable) key &optional non-terminating-p)
  (declare (ignore non-terminating-p))
  (etypecase key
    (character (cl:get-macro-character key obj))
    (cons (destructuring-bind (dsp sub) key
            (cl:get-dispatch-macro-character dsp sub obj)))))


(defmethod (setf ref) (val (obj readtable) key
                       &optional non-terminating-p)
  (etypecase key
    (character
     (cl:set-macro-character key val non-terminating-p obj))
    (cons (destructuring-bind (dsp sub) key
            (cl:set-dispatch-macro-character dsp sub val obj)))))


;; string-output-stream

(eval-always
  (setf (find-class 'string-output-stream)
        (find-class
         #+allegro 'excl::string-output-stream
         #+lispworks 'sys::string-output-stream
         #+sbcl 'sb-impl::string-output-stream)))


(defmethod ref ((obj string-output-stream) (key (eql 'string))
                &optional default)
  (declare (ignore key default))
  (cl:get-output-stream-string obj))


;; function
(defmethod ref ((obj (eql 'cl:function)) key &optional default)
  (declare (ignore default obj key))
  (lambda (x) (ref x key)))


;; time
(defmethod ref ((obj (eql 'time)) (key (eql 'decoded))
                &optional default)
  (declare (ignore default obj key))
  (cl:get-decoded-time))


(defmethod ref ((obj (eql 'time)) (key (eql 'universal))
                &optional default)
  (declare (ignore default obj key))
  (cl:get-universal-time))


(defmethod ref ((obj (eql 'time)) (key (eql 'internal-real))
                &optional default)
  (declare (ignore default obj key))
  (cl:get-internal-real-time))


(defmethod ref ((obj (eql 'time)) (key (eql 'internal-run))
                &optional default)
  (declare (ignore default obj key))
  (cl:get-internal-run-time))


(defmethod ref ((obj jso) (key string)
                &optional default)
  (declare (ignore default obj key))
  (st-json:getjso key obj))


(defmethod (setf ref) (val (obj jso) key &optional default)
  (setf (getjso key obj) val))


(defgeneric put (obj key value))


(defmethod put ((obj symbol) key value)
  (setf (cl:get obj key) value))


(defmethod put (obj key value)
  (setf (slot-value obj key) value))


(defmacro ~ (obj &rest slot-names)
  (reduce (lambda (acc x)
            (list 'bcl:ref acc x))
          slot-names
          :initial-value obj))


(defmacro bcl:or (&rest test-forms)
  `(cl:or ,@test-forms))


(define-setf-expander or (place default &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
                       (cl:get-setf-expansion place env)
    (values temps subforms stores setterform `(,@getterform ,default))))


;;; *EOF*
