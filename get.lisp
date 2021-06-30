(cl:in-package bcl)


(defgeneric get (obj key &optional default))


;; symbol
(defmethod get ((obj symbol) key &optional default)
  (cl:get obj key default))


(defmethod (setf get) (val (obj symbol) key &optional default)
  (setf (cl:get obj key default) val))


;; standard-object
(defmethod get ((obj standard-object) key &optional default)
  (or (slot-value obj key) default))


(defmethod (setf get) (val (obj standard-object) key
                       &optional default)
  (setf (slot-value obj key) val))


;; hash-table
(defmethod get ((obj hash-table) key &optional default)
  (gethash key obj default))


(defmethod (setf get) (val (obj hash-table) key &optional default)
  (setf (gethash key obj default) val))


;; plist
(defmethod get ((obj cons) key &optional default)
  (getf obj key default))


(defmethod get ((obj cons) (key list) &optional default)
  (declare (ignore default))
  (get-properties obj key))


(defmethod (setf get) (val (obj cons) key &optional default)
  (if (find key obj)
      (setf (getf obj key default) val)
      (let ((ans (append obj (list key val))))
        (setf (car obj) (car ans))
        (setf (cdr obj) (cdr ans))
        obj)))

;; readtable
(defmethod get ((obj readtable) key &optional non-terminating-p)
  (declare (ignore non-terminating-p))
  (etypecase key
    (character (get-macro-character key obj))
    (cons (destructuring-bind (dsp sub) key
            (get-dispatch-macro-character dsp sub obj)))))


(defmethod (setf get) (val (obj readtable) key
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

(defmethod get ((obj string-output-stream) (key (eql 'string))
                &optional default)
  (declare (ignore key default))
  (get-output-stream-string obj))


(defmethod get ((obj (eql 'time)) (key (eql 'decoded))
                &optional default)
  (declare (ignore default obj key))
  (get-decoded-time))


(defmethod get ((obj (eql 'time)) (key (eql 'universal))
                &optional default)
  (declare (ignore default obj key))
  (get-universal-time))


(defmethod get ((obj (eql 'time)) (key (eql 'internal-real))
                &optional default)
  (declare (ignore default obj key))
  (get-internal-real-time))


(defmethod get ((obj (eql 'time)) (key (eql 'internal-run))
                &optional default)
  (declare (ignore default obj key))
  (get-internal-run-time))


(defgeneric put (obj key value))


(defmethod put ((obj symbol) key value)
  (setf (cl:get obj key) value))


(defmethod put (obj key value)
  (setf (slot-value obj key) value))


;;; *EOF*
