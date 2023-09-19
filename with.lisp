(cl:in-package bcl)


;;; with-accessors
(setf (macro-function 'bcl:w/accessors)
      (macro-function 'with-accessors))


;;; with-compilation-unit
(setf (macro-function 'bcl:w/compilation-unit)
      (macro-function 'with-compilation-unit))


;;; with-condition-restarts
(setf (macro-function 'bcl:w/condition-restarts)
      (macro-function 'with-condition-restarts))


;;; with-hash-table-iterator
(setf (macro-function 'bcl:w/hash-table-iterator)
      (macro-function 'with-hash-table-iterator))


;;; with-input-from-string
(defmacro bcl:w/instring ((var string) &body body)
  `(with-input-from-string (,var ,string)
     ,@body))


;;; with-open-file
(defmacro bcl:w/infile ((var file) &body body)
  `(bcl:w/stream (,var (bcl:openi ,file)) ,@body))


(defmacro bcl:w/outfile ((var file) &body body)
  `(bcl:w/stream (,var (bcl:openo ,file)) ,@body))


;;; with-open-stream
(setf (macro-function 'bcl:w/stream)
      (macro-function 'with-open-stream))


;;; with-output-to-string
(setf (macro-function 'bcl:w/outstring)
      (macro-function 'with-output-to-string))


;;; with-package-iterator
(setf (macro-function 'bcl:w/package-iterator)
      (macro-function 'with-package-iterator))


; with-simple-restart
(setf (macro-function 'bcl:w/simple-restart)
      (macro-function 'with-simple-restart))


; with-slots
(setf (macro-function 'bcl:w/slots)
      (macro-function 'with-slots))


; with-standard-io-syntax
(setf (macro-function 'bcl:w/standard-io-syntax)
      (macro-function 'with-standard-io-syntax))


;;; *EOF*
