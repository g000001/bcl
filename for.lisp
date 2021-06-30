(cl:in-package bcl)


(defmacro for (&rest body)
  `(loop 
    ,@(reduce (lambda (res b)
                (append res (->loop-clause b)))
              body
              :initial-value nil)))


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
