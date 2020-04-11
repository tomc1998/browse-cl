(in-package #:browse-cl)

(defclass expr () ())

;; Get the type of an expr
(defgeneric get-type (expr))
;; Eval an expr in the current end
(defgeneric eval-expr (env expr))

(defclass constant (expr) 
  ((val :initarg :val :accessor val)
   (ty :initarg :ty :accessor ty :type ty)))
(defmethod get-type ((e constant)) (ty e))
(defmethod eval-expr ((env env) (e constant)) 
  (cond
    ((ty-eq (ty e) *ty-bool*) (if (string= (string (val e)) "T") t nil))
    (t (val e))))

(defclass runtime-value (expr)
  ((id :initarg :id :accessor id :type var-id)
   (ty :initarg :ty :accessor ty :type ty))
  (:documentation "Represents a runtime value"))
(defmethod eval-expr ((env env) (e runtime-value)) 
  (if (>= (id e) 0)
      (aref (globals env) (id e))
      (aref (stack env) (- (- (id e)) 1))))
(defmethod get-type ((e runtime-value)) (ty e))
