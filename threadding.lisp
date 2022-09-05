;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "CL-USER")


(defun canonicalize-bindspec (bindspec)
  (mapcar (lambda (v)
            (etypecase v
              (cons v)
              ((and symbol (not null)) `(,v nil)))) 
          bindspec))


(defun subst-symbol (new old sym)
  (if (string= old sym)
      new
      sym))


(defmacro foo-let* (sym (&rest bvl) &body body &environment env)
  (let* ((bvl (canonicalize-bindspec bvl))
         (bvlhd (car bvl))
         (bvltl (cdr bvl))
         (newsym (make-symbol (string sym))))
    `(let (,@(and bvlhd `((,(subst-symbol newsym sym (car bvlhd))
                           ,@(cdr bvlhd)))))
       ,(destructuring-bind (let* bvl . body)
                            (walker:walk-form `(let* (,@(mapcar (lambda (v)
                                                                  (destructuring-bind (var . val)
                                                                                      v
                                                                    `(,(subst-symbol newsym sym var) ,@val)))
                                                                bvltl))
                                                 ,@body)
                                              env
                                              (lambda (sub ctx env &aux (stop? nil))
                                                (declare (ignore env ctx))
                                                (if (symbolp sub)
                                                    (values (subst-symbol newsym sym sub) stop?)
                                                    (values sub stop?))))
          `(,let* ,bvl ,@body)))))


(defmacro bcl:seq (&body body)
  `(foo-let* * (,@(mapcar (lambda (form) `(* ,form)) body))
     ,@(and body `(*))))


;;; *EOF*
