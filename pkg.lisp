;;; -*- mode: Lisp; coding: utf-8  -*-

(bcl::in-sub-package)


(defclass intern-form ()
  ((name :initarg :name)
   (package :initarg :package)))


(defmethod print-object ((obj intern-form) stream)
  (format stream
          "#.(CL:INTERN ~S ~S)"
          (slot-value obj 'name)
          (slot-value obj 'package)))


(defun up-symbol (elt pkg)
  (typecase elt
    (symbol
       (let ((name (string elt)))
         (make-instance 'intern-form
                  :name name
               :package (package-name
                         (let ((elt-pkg (symbol-package elt)))
                           (cond ((eq elt-pkg (find-package pkg))
                                  pkg)
                                 ;;
                                 ((and (eq elt-pkg (find-package *package*))
                                       (find-symbol (string elt) pkg))
                                  pkg)
                                 ;;
                                 ('T elt-pkg)))))))
    ;;
    (otherwise elt)))


(defun symbol-to-intern-form (tree pkg)
  (cond ((null tree)
         tree)
        ;;
        ((atom (car tree))
         (let ((elt (car tree)))
           (cons (if (eq 'pkg-bind elt)
                     'pkg-bind
                     (up-symbol elt pkg))
                 (symbol-to-intern-form (cdr tree) pkg))))
        ;;
        ('T (cons (symbol-to-intern-form (car tree) pkg)
                  (symbol-to-intern-form (cdr tree) pkg)))))


(defmacro bcl:pkg-bind (pkg &body body)
  `(progn
     ,@(read-from-string
        (write-to-string
         (symbol-to-intern-form body (package-name pkg))))))


;;; *EOF*
