(cl:in-package bcl)

#||
(for (let x := ...))
(bcl:for (each x (the list xs)))

||#

(eval-always
  (defun ->loop-clause (xpr)
    (case (find (car xpr) '(let each) :test #'string-equal)
      (let (destructuring-bind (let &rest args)
                               xpr
             (declare (ignore let))
             `(for ,@args)))
      (each (destructuring-bind (each var (the type sequence))
                                xpr
              (declare (ignore each the))
              (ecase type
                ((cl:vector cl:string) `(for ,var :across ,sequence))
                ((cl:list) `(for ,var :in ,sequence)))))
      (otherwise xpr))))


(defmacro for (&rest body)
  `(loop 
    ,@(reduce (lambda (res b)
                (append res (->loop-clause b)))
              body
              :initial-value nil)))

(cl:in-package bcl)

(defmacro bcl::doseries ((var (the type col) &optional result-form) &body body)
  (declare (ignore the))
  `(progn
     (iterate ((,var (scan ',type ,col)))
       ,@body)
     (let ((,var nil)) ,result-form)))

(defmacro bcl::dolist ((var list &optional result-form) &body body)
  `(bcl::doseries (,var (the list ,col) ,result-form) ,@body))

(defmacro bcl::dovector ((var vec &optional result-form) &body body)
  `(bcl::doseries (,var (the vector ,col) ,result-form) ,@body))

(defmacro bcl::doseq ((var seq &optional result-form) &body body)
  `(bcl::doseries (,var (the T ,seq) ,result-form) ,@body))

(defmacro bcl::skip () ''#:skip)

(defmacro bcl::collist ((var list) &body body)
  `(collect 'list
            (choose-if (lambda (x)
                         (not (eq (skip) x)))
                       (map-fn 'list (lambda (,var) ,@body) 
                               (scan 'list ,list)))))
#|
(collist (i '(0 1 2 3))
  (if (evenp i)
      i
      (skip)))
|#


(defmacro map (result-type function first-sequence &rest more-sequences)
  (case (eval result-type)
    (series
     `(map-fn 'series
              ,function
              (scan ,first-sequence)
              ,@(mapcar (lambda (x) `(scan ,x)) more-sequences)))
    (otherwise
     `(cl:map ,result-type ,function ,first-sequence ,@more-sequences))))
