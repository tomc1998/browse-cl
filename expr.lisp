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

(defclass arr-constructor (expr)
  ((vals :initarg :vals :accessor vals :type 'list
         :documentation "List of expr, must not be empy")))
(defmethod get-type ((e arr-constructor))
  (assert (> (length (vals e)) 0))
  (make-ty-arr (get-type (nth 0 (vals e)))))
(defmethod eval-expr ((env env) (e arr-constructor))
  (let ((ret (make-array (list (length (vals e)))
                         :adjustable t
                         :fill-pointer 0
                         :initial-element nil)))
    (loop for x in (vals e) do
          (vector-push-extend (eval-expr env x) ret))
    ret))

(defclass if-expr (expr)
  ((cond-expr :initarg :cond-expr :accessor cond-expr :type expr)
   (t-expr :initarg :t-expr :accessor t-expr :type expr)
   (f-expr :initarg :f-expr :accessor f-expr :type expr)))
(defmethod get-type ((e if-expr)) (get-type (t-expr e)))
(defmethod eval-expr ((env env) (e if-expr))
  (if (eval-expr env (cond-expr e))
      (eval-expr env (t-expr e))
      (eval-expr env (f-expr e))))

;; TODO Typecheck that all body exprs return the same type, since the value of
;; a loop expr is the array containing all the iterations of all the body exprs
(defclass loop-expr (expr)
  ((item-loc :initarg :item-loc :accessor item-loc :type expr)
   (target :initarg :target :accessor target :type expr)
   (body :initarg :body :accessor body :type list
         :documentation "List of expr")))

(defmethod get-type ((e loop-expr))
  (if (not (body e))
      *ty-void*
      (make-ty-arr (get-type (car (body e))))))

(defmethod eval-expr ((env env) (e loop-expr))
  (let* ((arr (eval-expr env (target e)))
        (ret (make-array (list (* (length (body e)) (length arr))) 
                         :adjustable t
                         :fill-pointer 0
                         :initial-element nil))) 
    (loop for x across arr do
          (set-in-place env (eval-expr-place env (item-loc e)) x)
          (loop for b in (body e) do
                (vector-push-extend (eval-expr env b) ret)))
    ret))

(defclass stack-alloc (expr)
  ((id :initform nil :accessor id :type (or null var-id))
   (ty :initarg :ty :accessor ty :type ty)))
(defmethod get-type ((e stack-alloc))(ty e))
(defmethod assure-stack-alloc ((env env) (e stack-alloc))
  (when (not (id e)) (setf (id e) (alloc-stack env))))
(defmethod eval-expr ((env env) (e stack-alloc))
  (assure-stack-alloc env e)
  (env-lookup env (id e)))
(defmethod eval-expr-place ((env env) (e stack-alloc))
  (assure-stack-alloc env e)
  (make-instance 'id-place :id (id e)))

(defclass runtime-value (expr)
  ((id :initarg :id :accessor id :type var-id)
   (ty :initarg :ty :accessor ty :type ty))
  (:documentation "Represents a runtime value"))
(defmethod eval-expr ((env env) (e runtime-value)) 
  (env-lookup env (id e)))
(defmethod get-type ((e runtime-value)) (ty e))
(defmethod eval-expr-place ((env env) (e runtime-value)) 
  (make-instance 'id-place :id (id e)))
