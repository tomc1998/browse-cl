(in-package #:browse-cl)

(defclass expr () ())

(defclass place () ())

(defgeneric walk-expr (expr fn &optional val)
  (:documentation "Walk the tree, calling (fn node val) on each node in a
                   depth-first walk of the tree. 'val' is the value of fn
                   called with the parent function. For the root node, a value
                   of 'nil' is provided: although another default val can be provided
                   through the optional 'val' parameter."))
;; Get the type of an expr
(defgeneric get-type (expr))
;; Eval an expr in the current env
(defgeneric eval-expr (env expr))
;; Eval an expr, but return a place which can be used to assign to in the
;; current env, OR nil if not possible.
(defgeneric eval-expr-place (env expr))
;; Return a list of global var-id that this expr depends on. When these global
;; IDs are changed, the value of this expression could potentially change.
(defgeneric find-dependent-env-vals (expr))

(defgeneric set-in-place (env place val)
  (:documentation "Set the given place to the given value. val should be a
                   constant."))

(defmethod eval-expr-place (env (e expr))
  (error "Unimpl getting place from ~S" e))

(defclass id-place (place)
  ((id :initarg :id :accessor id :type var-id))
  (:documentation "A simple place that represents a space in the env"))
(defmethod set-in-place ((env env) (p id-place) val)
  (setf (env-lookup env (id p)) val))

(defclass constant (expr) 
  ((val :initarg :val :accessor val)
   (ty :initarg :ty :accessor ty :type ty)))
(defmethod walk-expr ((e constant) fn &optional val) (funcall fn e val))
(defmethod get-type ((e constant)) (ty e))
(defmethod eval-expr ((env env) (e constant)) 
  (cond
    ((ty-eq (ty e) *ty-bool*) (if (string= (string (val e)) "T") t nil))
    (t (val e))))
(defmethod find-dependent-env-vals ((e constant)) nil)

(defclass set-expr (expr)
  ((place :initarg :place :accessor place :type expr)
   (val :initarg :val :accessor val :type expr)))
(defmethod walk-expr ((e set-expr) fn &optional val) 
  (let ((val (funcall fn e val)))
    (walk-expr (place e) fn val)
    (walk-expr (val e) fn val)))
(defmethod eval-expr ((env env) (e set-expr)) 
  (set-in-place env (eval-expr-place env (place e)) (eval-expr env (val e))))
(defmethod find-dependent-env-vals ((e set-expr))
  (let ((place-id 
          (cond
            ((typep (place e) 'id-place)
             (id (place e)))
            (t (error "Unimpl finding dependent env vals for place ~S" (place e)))
            ))
        (val-ids (find-dependent-env-vals (val e))))
    ;; If global var
    (concatenate 
      'list
      (if (>= place-id 0) (list place-id) nil)
      val-ids)))

(defclass apply-expr (expr)
  ((fn :initarg :fn :accessor fn
       :documentation "A function which takes a list of values, corresponding
                       to the given args, and returns a common lisp value.")
   (ty :initarg :ty :accessor ty :type ty
       :documentation "The type of value returned from fn.")
   (with-env :initarg :with-env :initform nil :accessor with-env :type boolean
             :documentation "When true, calls fn with the first arg as the env
                             when evaluated")
   (args :initarg :args :accessor args :type list
         :documentation "List of expr")))
(defmethod walk-expr ((e apply-expr) fn &optional val)
  (let ((val (funcall fn e val)))
    (loop for a in (args e) do (funcall fn a val))))
(defmethod find-dependent-env-vals ((e apply-expr))
  (apply (curry #'concatenate 'list) (mapcar #'find-dependent-env-vals (args e))))
(defmethod eval-expr ((env env) (e apply-expr))
  (apply 
    (if (with-env e) (curry (fn e) env) (fn e)) 
    (mapcar (curry #'eval-expr env) (args e))))
(defmethod get-type ((e apply-expr)) (ty e))

(defclass arr-constructor (expr)
  ((vals :initarg :vals :accessor vals :type 'list
         :documentation "List of expr, must not be empy")))
(defmethod walk-expr ((e arr-constructor) fn &optional val)
  (let ((val (funcall fn e val)))
    (loop for v in (vals e) do (funcall fn v val))))
(defmethod find-dependent-env-vals ((e arr-constructor))
  (remove-if-not #'identity 
                 (apply (curry #'concatenate 'list) 
                        (mapcar #'find-dependent-env-vals (vals e)))))
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
(defmethod walk-expr ((e if-expr) fn &optional val)
  (let ((val (funcall fn e val)))
    (loop for v in (list (cond-expr e) (t-expr e) (f-expr e)) do 
          (funcall fn v val))))
(defmethod find-dependent-env-vals ((e if-expr))
  (concatenate 'list
               (find-dependent-env-vals (cond-expr e))
               (find-dependent-env-vals (t-expr e))
               (find-dependent-env-vals (f-expr e))))
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
(defmethod walk-expr ((e loop-expr) fn &optional val)
  (let ((val (funcall fn e val)))
    (funcall fn (item-loc e) val)
    (funcall fn (target e) val)
    (loop for v in (body e) do (funcall fn v val))))
(defmethod find-dependent-env-vals ((e loop-expr))
  (concatenate 'list 
               (find-dependent-env-vals (item-loc e))
               (find-dependent-env-vals (target e))
               (apply (curry #'concatenate 'list) 
                      (mapcar #'find-dependent-env-vals (body e)))))
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
(defmethod walk-expr ((e stack-alloc) fn &optional val) (funcall fn e val))
(defmethod find-dependent-env-vals ((e stack-alloc)) nil)
(defmethod get-type ((e stack-alloc)) (ty e))
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
(defmethod walk-expr ((e runtime-value) fn &optional val) (funcall fn e val))
(defmethod find-dependent-env-vals ((e runtime-value)) (if (id e) (list (id e)) nil))
(defmethod eval-expr ((env env) (e runtime-value)) 
  (env-lookup env (id e)))
(defmethod get-type ((e runtime-value)) (ty e))
(defmethod eval-expr-place ((env env) (e runtime-value)) 
  (make-instance 'id-place :id (id e)))
