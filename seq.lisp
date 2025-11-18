(bcl::in-sub-package)


;;https://people.csail.mit.edu/jrb/goo/manual.46/goomanual_26.html
(defun bcl::elt-or (seq index default)
  (etypecase seq
    (null default)
    (cons (or (nth index seq)
              (let ((len (length seq)))
                (if (< index len)
                    (elt seq index)
                    default))))
    (sequence (let ((len (length seq)))
                (if (< index len)
                    (elt seq index)
                    default)))))


(declaim (inline mem))


(defun mem (pred item list)
  (member item list :test pred))


(declaim (inline fin))


(defun fin (pred item seq)
  (find item seq :test pred))


(defun || (&rest strings)
  "concatenate strings"
  (declare (optimize (safety 0) (speed 3))
           (dynamic-extent strings))
  (let ((len 0)
        (pos 0))
    (declare (fixnum len pos))
    (dolist (s strings)
      (declare (simple-string s))
      (incf len (length s)))
    (let ((result (make-string len)))
      (declare (simple-string result))
      (dolist (s strings)
        (declare (simple-string s))
        (loop :for c :across s
              :do (setf (schar result pos) c) (incf pos)))
      result)))


(defmacro multiple-value-collect-hash (&body series-values)
  (let ((ks (gensym))
        (vs (gensym)))
    `(zrseries:multiple-value-bind (,ks ,vs)
                          ,(car series-values)
       (zrseries:collect-hash ,ks ,vs :test 'equal))))


(defmacro multiple-value-collect-alist (&body series-values)
  (let ((ks (gensym))
        (vs (gensym)))
    `(zrseries:multiple-value-bind (,ks ,vs)
                          ,(car series-values)
       (zrseries:collect-alist ,ks ,vs))))


(defmacro to-hash (key-fn value-fn &body series-values)
  (let ((x (gensym)))
    `(multiple-value-collect-hash 
       (zrseries:map-fn '(values t t)
                        (lambda (,x)
                          (values (funcall ,key-fn ,x)
                                  (funcall ,value-fn ,x)))
                        ,(car series-values)))))

(defmacro to-alist (key-fn value-fn &body series-values)
  (let ((x (gensym)))
    `(multiple-value-collect-alist
      (zrseries:map-fn '(values t t)
                       (lambda (,x)
                         (values (funcall ,key-fn ,x)
                                 (funcall ,value-fn ,x)))
                       ,(car series-values)))))

(defmacro to-plist (key-fn value-fn &body series-values)
  (let ((x (gensym)))
    `(multiple-value-collect-plist
       (zrseries:map-fn '(values t t)
                        (lambda (,x)
                          (values (funcall ,key-fn ,x)
                                  (funcall ,value-fn ,x)))
                        ,(car series-values)))))


;;; *EOF*
