(cl:in-package bcl)


(declaim (inline mem))


(defun mem (pred item list)
  (member item list :test pred))


(declaim (inline fin))


(defun fin (pred item seq)
  (find item seq :test pred))


;;; *EOF*
