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


;;; *EOF*
