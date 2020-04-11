(in-package #:browse-cl)

(defclass expr () ())

(defclass place () ())

;; Get the type of an expr
(defgeneric get-type (expr))
;; Eval an expr in the current env
(defgeneric eval-expr (env expr))
;; Eval an expr, but return a place which can be used to assign to in the
;; current env, OR nil if not possible.
(defgeneric eval-expr-place (env expr))

(defgeneric set-in-place (env place val)
  (:documentation "Set the given place to the given value. val should be a
                   constant."))

(defmethod eval-expr-place (env (e expr))
  (error "Unimpl getting place from ~S" e))

(defclass id-place (place)
  ((id :initarg :id :accessor id :type var-id))
  (:documentation "A simple place that represents a space in the env"))

(defmethod set-in-place ((env env) (p id-place) val)
  (if (>= (id p) 0)
      (setf (aref (globals env) (id p)) val)
      (setf (aref (stack env) (- (- (id p)) 1)) val)))

(defclass constant (expr) 
  ((val :initarg :val :accessor val)
   (ty :initarg :ty :accessor ty :type ty)))
(defmethod get-type ((e constant)) (ty e))
(defmethod eval-expr ((env env) (e constant)) 
  (cond
    ((ty-eq (ty e) *ty-bool*) (if (string= (string (val e)) "T") t nil))
    (t (val e))))

(defclass set-expr (expr)
  ((place :initarg :place :accessor place :type expr)
   (val :initarg :val :accessor val :type expr)))
(defmethod eval-expr ((env env) (e set-expr)) 
  (set-in-place env (eval-expr-place env (place e)) (eval-expr env (val e))))

(defclass apply-expr (expr)
  ((fn :initarg :fn :accessor fn
       :documentation "A function which takes a list of values, corresponding
                       to the given args, and returns a common lisp value.")
   (ty :initarg :ty :accessor ty :type ty
       :documentation "The type of value returned from fn.")
   (args :initarg :args :accessor args :type list
         :documentation "List of expr")))

(defmethod eval-expr ((env env) (e apply-expr))
  (apply (fn e) (mapcar (curry #'eval-expr env) (args e))))
(defmethod get-type ((e apply-expr)) (ty e))

(defclass runtime-value (expr)
  ((id :initarg :id :accessor id :type var-id)
   (ty :initarg :ty :accessor ty :type ty))
  (:documentation "Represents a runtime value"))
(defmethod eval-expr ((env env) (e runtime-value)) 
  (if (>= (id e) 0)
      (aref (globals env) (id e))
      (aref (stack env) (- (- (id e)) 1))))
(defmethod get-type ((e runtime-value)) (ty e))
(defmethod eval-expr-place ((env env) (e runtime-value)) 
  (make-instance 'id-place :id (id e)))
