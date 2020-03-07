(cl:in-package bcl.internal)


;;; with-accessors
(setf (macro-function 'w/accessors)
      (macro-function 'with-accessors))


;;; with-compilation-unit
(setf (macro-function 'w/compilation-unit)
      (macro-function 'with-compilation-unit))


;;; with-condition-restarts
(setf (macro-function 'w/condition-restarts)
      (macro-function 'with-condition-restarts))


;;; with-hash-table-iterator
(setf (macro-function 'w/hash-table-iterator)
      (macro-function 'with-hash-table-iterator))


;;; with-input-from-string
(defmacro w/instring ((var string) &body body)
  `(with-input-from-string (,var ,string)
     ,@body))


;;; with-open-file
(defmacro w/infile ((var file) &body body)
  `(w/stream (,var (openi ,file)) ,@body))


(defmacro w/outfile ((var file) &body body)
  `(w/stream (,var (openo ,file)) ,@body))


;;; with-open-stream
(setf (macro-function 'w/stream)
      (macro-function 'with-open-stream))


;;; with-output-to-string
(setf (macro-function 'w/outstring)
      (macro-function 'with-output-to-string))


;;; with-package-iterator
(setf (macro-function 'w/package-iterator)
      (macro-function 'with-package-iterator))


; with-simple-restart
(setf (macro-function 'w/simple-restart)
      (macro-function 'with-simple-restart))


; with-slots
(setf (macro-function 'w/slots)
      (macro-function 'with-slots))


; with-standard-io-syntax
(setf (macro-function 'w/standard-io-syntax)
      (macro-function 'with-standard-io-syntax))


;;; *EOF*
