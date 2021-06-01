(cl:in-package bcl)


(defmacro for (&rest body)
  `(loop 
    ,@(reduce (lambda (res b)
                (append res (->loop-clause b)))
              body
              :initial-value nil)))


(eval-always
  (defun ->loop-clause (xpr)
    (case (find (car xpr) '(let) :test #'string-equal)
      (let (destructuring-bind (let &rest args)
                               xpr
             (declare (ignore let))
             `(for ,@args)))
      (otherwise xpr))))