(in-package #:browse-cl)

(defclass scope () 
  ((table :initform (make-hash-table :test 'equal) :accessor table :type 'hash-table)
   (parent :initarg :parent :initform nil :accessor parent :type 'scope)))
(defmethod find-in-scope ((s scope) (name string))
  (let ((res (gethash name (table s))))
    (if res res (if (parent s) (find-in-scope (parent s) name) nil))))
(defmethod subscope ((s scope)) (make-instance 'scope :parent s))
(defmethod set-in-scope ((s scope) key val)
  (setf (gethash key (table s)) val))
(defmethod dump-to-list ((s scope))
  (let ((ret (list)))
    (maphash (lambda (k v) (push (cons k v) ret)) (table s))
    ret))
