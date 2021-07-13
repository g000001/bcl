(cl:in-package bcl)


(defgeneric ref (obj key &optional default))


;; symbol
(defmethod ref ((obj symbol) key &optional default)
  (cl:get obj key default))


(defmethod (setf ref) (val (obj symbol) key &optional default)
  (setf (cl:get obj key default) val))


;; standard-object
(defmethod ref ((obj standard-object) key &optional default)
  (or (slot-value obj key) default))


(defmethod (setf ref) (val (obj standard-object) key
                       &optional default)
  (setf (slot-value obj key) val))


;; hash-table
(defmethod ref ((obj hash-table) key &optional default)
  (gethash key obj default))


(defmethod (setf ref) (val (obj hash-table) key &optional default)
  (setf (gethash key obj default) val))


;; plist
(defmethod ref ((obj cons) key &optional default)
  (getf obj key default))


(defmethod ref ((obj cons) (key list) &optional default)
  (declare (ignore default))
  (get-properties obj key))


(defmethod (setf ref) (val (obj cons) key &optional default)
  (if (find key obj)
      (setf (getf obj key default) val)
      (let ((ans (append obj (list key val))))
        (setf (car obj) (car ans))
        (setf (cdr obj) (cdr ans))
        obj)))

;; readtable
(defmethod ref ((obj readtable) key &optional non-terminating-p)
  (declare (ignore non-terminating-p))
  (etypecase key
    (character (get-macro-character key obj))
    (cons (destructuring-bind (dsp sub) key
            (get-dispatch-macro-character dsp sub obj)))))


(defmethod (setf ref) (val (obj readtable) key
                       &optional non-terminating-p)
  (etypecase key
    (character
     (set-macro-character key val non-terminating-p obj))
    (cons (destructuring-bind (dsp sub) key
            (set-dispatch-macro-character dsp sub val obj)))))


;; string-output-stream

(eval-always
  (setf (find-class 'string-output-stream)
        (find-class
         #+lispworks 'sys::string-output-stream
         #+sbcl 'sb-impl::string-output-stream)))

(defmethod ref ((obj string-output-stream) (key (eql 'string))
                &optional default)
  (declare (ignore key default))
  (get-output-stream-string obj))


(defmethod ref ((obj (eql 'time)) (key (eql 'decoded))
                &optional default)
  (declare (ignore default obj key))
  (get-decoded-time))


(defmethod ref ((obj (eql 'time)) (key (eql 'universal))
                &optional default)
  (declare (ignore default obj key))
  (get-universal-time))


(defmethod ref ((obj (eql 'time)) (key (eql 'internal-real))
                &optional default)
  (declare (ignore default obj key))
  (get-internal-real-time))


(defmethod ref ((obj (eql 'time)) (key (eql 'internal-run))
                &optional default)
  (declare (ignore default obj key))
  (get-internal-run-time))


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


;;; *EOF*
