;;; -*- mode :Lisp -*-

(bcl::in-sub-package)


(bcl:in-syntax bcl:*bcl*)


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
