;;; -*- mode :Lisp -*-

(bcl::in-sub-package)


(bcl:in-syntax bcl:*bcl*)


(defun anonvar-name-p (symbol)
  (typecase symbol
    (symbol (string= "_" symbol))
    (T nil)))


(deftype anonvar ()
  `(and symbol (satisfies anonvar-name-p)))


(defun extract-rx (list)
  (with-output-to-string (out)
    (labels ((rec (list)
               (cond ((null list) (princ ")" out))
                     ((typep list 'string) (princ list out))
                     ((typep list 'cons)
                      (princ "(" out)
                      (when (typep (car list) 'anonvar)
                        (princ "?:" out))
                      (mapcar #'rec list)
                      (princ ")" out)))))
      (rec list))))


(defun process-rxbindspec (bindspec)
  (let ((vars '())
        (rxs (apply #'concatenate 'string
                    (mapcar #'extract-rx bindspec))))
    (labels ((trav (tree)
               (typecase tree
                 (null '())
                 ((cons (cons anonvar *) *)
                  (trav (cdr (car tree)))
                  (trav (cdr tree)))
                 ((cons (cons symbol *) *)
                  (push (car (car tree)) vars)
                  (trav (cdr (car tree)))
                  (trav (cdr tree)))
                 ((cons string *)
                  (trav (cdr tree))))))
      (trav bindspec))
    (values (nreverse vars)
            rxs)))


(defmacro re.bind ((&rest bindspec) target-string &body body)
  (multiple-value-bind (var-list regex)
                       (process-rxbindspec bindspec)
    `(re.register-groups-bind (,@var-list) (,regex ,target-string)
       ,@body)))


(defun read-delimited-sequence (result-type endchar &optional (input-stream *standard-input*) recursivep cont)
  (let* ((prev nil)
         (ans (list nil))
         (tem ans))
    (cl:do ((chr (read-char input-stream nil input-stream recursivep)
              (read-char input-stream nil input-stream recursivep)))
        ((or (eq chr input-stream)
             (and (eql chr endchar)
                  (not (eql #\\ prev))))
         (values (coerce (cdr ans) result-type)
                 (and cont (funcall cont input-stream))))
      (setf (cdr tem)
            (setq tem (list chr)))
      (setq prev chr))))


(bcl:eval-always
  (flet ((read-/ (srm chr arg)
           (declare (ignore arg))
           (multiple-value-bind (re case-fold-p)
                                (read-delimited-sequence 'string
                                                         chr
                                                         srm
                                                         T
                                                         (bcl:^ (in) 
                                                           (case (peek-char nil in nil)
                                                             ((#\i #\I) 
                                                              (read-char in)
                                                              :case-fold)
                                                             (otherwise nil))))
             `(ppcre:create-scanner ,re :case-insensitive-mode ,case-fold-p))))
    (setf (~ bcl:*bcl* '(#\# #\/))
          #'read-/)))


;;; *EOF*
