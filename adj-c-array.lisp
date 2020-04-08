(in-package #:browse-cl)

;; A dynamically expanding cepl:c-array. Use make-adj-c-array.
;; IMPORTANT: manually call 'free' to free this memory!
(defclass adj-c-array ()
  ((cap :initarg :cap :initform (error "Must specify cap for adj-c-array") 
       :accessor cap :type 'integer)
   (len :initarg :len :initform 0 :accessor len :type 'integer)
   (inner :initarg :inner :initform (error "Must specify underlying c-array for adj-c-array") 
          :accessor inner :type 'cepl:c-array)))

(defun make-adj-c-array (ty &key (cap 0))
  "Make an adj-c-array, containing elements of the given type, with the given initial capacity.
   
   * ty - The (CFFI) type of each element
   * cap - The initial capacity, default 0
   "
  (let* ((c-array (make-c-array nil :dimensions (list cap) :element-type ty))
         (adj-c-array (make-instance 'adj-c-array :cap cap :inner c-array))) 
    adj-c-array))

(defmethod free ((a adj-c-array))
  "Once called, any operation on the given array will result in undefined behaviour."
  (assert (inner a))
  (free (inner a))
  (setf (inner a) nil))

(defmethod extend-ac ((a adj-c-array) new-cap)
  "Resize the inner capacity to something larger."
  (assert (inner a))
  (assert (> new-cap (cap a)))
  (let ((new-inner (make-c-array nil :element-type (element-type (inner a)) :dimensions (list new-cap))))
    ;; TODO use c's memcpy here
    (loop for ii below (len a) do 
          (setf (aref-c new-inner ii) (aref-ac a ii)))
    (free (inner a))
    (setf (inner a) new-inner)
    (setf (cap a) new-cap)))

(defmethod push-back-ac (x (a adj-c-array)) 
  (assert (inner a))
  (when (>= (len a) (cap a)) (extend-ac a (max 8 (* 2 (cap a)))))
  (setf (aref-c (inner a) (len a)) x)
  (incf (len a)))

(defmethod clear-ac ((a adj-c-array))
  "Doesn't free memory, sets len to 0."
  (setf (len a) 0))

(defun aref-ac (a ix) (aref-c (inner a) ix))
(defun (setf aref-ac) (value a ix)
  (assert (inner a))
  (setf (aref-c (inner a) ix) value))
