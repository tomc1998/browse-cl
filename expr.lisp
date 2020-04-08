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
