(in-package #:browse-cl)

(defclass param ()
  ((name :initarg :name :accessor name :type string)
   (ty :initarg :ty :accessor ty :type ty)))

(defclass ty-method () 
  ((name :initarg :name :accessor name :type string)
   ;; List of 'param. implicit first param for receiver, not present in params
   (params :initarg :params :accessor params :type list)
   (ret-ty :initarg :ret-ty :accessor ret-ty :type ty)
   (body :initarg :body :accessor body :type expr)))

;; An un-named method, which acts like a macro
(defclass ty-inline-method ()
  ((name :initarg :name :accessor name :type string)
   (params :initarg :params :accessor params :type list
           :documentation "List of param")
   (ret-ty :initarg :ret-ty :accessor ret-ty :type ty)
   ;; A function which takes params of the types given above (with the first
   ;; param being the 'this' argument, then the rest of the params taking the
   ;; types of the params specified in the 'params' list), and returns a common
   ;; lisp value.  
   ;; Parameters should be given in their common lisp values.
   (eval-fn :initarg :eval-fn :accessor eval-fn :type function)))

(defclass ty-extern-method ()
  ((name :initarg :name :accessor name :type string)
   ;; Doesn't contain receiver as a param, but that is an implicit first param.
   (ty :initarg :ty :accessor ty :type ty)))

(defgeneric get-method-type (m))

(defmethod get-method-type ((m ty-method))
  (make-ty-fn (mapcar (lambda (x) (ty x)) (params m)) (ret-ty m)))

(defmethod get-method-type ((m ty-inline-method))
  (make-ty-fn (mapcar (lambda (x) (ty x)) (params m)) (ret-ty m)))

(defmethod get-method-type ((m ty-extern-method)) (ty m))

;; A primitive type is just a builtin type without any similarity to other
;; kinds when talking about its properties (e.g. it doesn't have struct
;; fields)
(deftype kind () '(member prim fn component struct opt arr kind))

(defclass ty ()
  (;; Used for printing and equality checking
   (name :initarg :name :accessor name :type string)
   ;; List of ty-method or ty-extern-method
   (methods :initarg :methods :initform (list) :accessor methods :type list)
   ;; Differs depending on type, see individual instantiations for details. 
   ;; Often contains any inner parameterised types, e.g. for arrs.
   (metadata :initarg :metadata :initform nil :accessor metadata)
   (kind :initarg :kind :accessor kind :type kind)))

;; The field of a struct
(defclass struct-field ()
  ((name :initarg :name :accessor name :type string)
   (ty :initarg :ty :accessor ty :type ty)))

(defparameter *ty-any* (add-type-to-builtin-store (make-instance 'ty :name "ANY" :kind 'prim)))
(defparameter *ty-void* (add-type-to-builtin-store (make-instance 'ty :name "VOID" :kind 'prim)))
(defparameter *ty-string* (add-type-to-builtin-store (make-instance 'ty :name "STRING" :kind 'prim)))
(defparameter *ty-int* (add-type-to-builtin-store (make-instance 'ty :name "INT" :kind 'prim)))
(defparameter *ty-num* (add-type-to-builtin-store (make-instance 'ty :name "NUM" :kind 'prim)))
(defparameter *ty-bool* (add-type-to-builtin-store (make-instance 'ty :name "BOOL" :kind 'prim)))
(defparameter *ty-event* (add-type-to-builtin-store (make-instance 'ty :name "EVENT" :kind 'prim)))
(defparameter *ty-mouse-event* (add-type-to-builtin-store (make-instance 'ty :name "MOUSE-EVENT" :kind 'prim)))
(defparameter *ty-kind* (add-type-to-builtin-store (make-instance 'ty :name "KIND" :kind 'kind)))

;; More of a placeholder ty, if an item in the scope is labelled as a
;; 'concrete-dom', it serves as a function which instantiates a
;; template-concrete-dom-node. Mainly used to disambiguate actual function
;; calls from dom node instantiations.
;; TODO parameterise this, use make-ty-view-node
(defparameter *ty-concrete-dom* 
  (add-type-to-builtin-store (make-instance 'ty :name "CONCRETE-DOM" :kind 'component)))
;; Placeholder ty, instance of a dom node
(defparameter *ty-dom-node-instance* 
  (add-type-to-builtin-store (make-instance 'ty :name "DOM-INSTANCE" :kind 'prim)))

(defun parse-ty-expr (s form)
  "Given the scope, parse the form as a type and return a ty"
  ;; TODO replace builtin types here with scope lookups
  (cond 
    ((symbolp form) 
     (let ((res (find-in-scope s (string form))))
       (if res res (error "Unrecognised type '~a'" form))))
    ((and (listp form) (symbolp (car form)) (string= (string (car form)) "ARR")) 
     (make-ty-arr (parse-ty-expr s (nth 1 form))))
    ((and (listp form) (symbolp (car form)) (string= (string (car form)) "OPT")) 
     (make-ty-opt (parse-ty-expr s (nth 1 form))))
    ((and (listp form) (symbolp (car form)) (string= (string (car form)) "FN")) 
     (make-ty-fn (loop for p in (nth 1 form) collect (parse-ty-expr s p)) 
                 (parse-ty-expr s (nth 2 form))))
    (t (error "Unrecognised type '~S'" form)))) 

(defun make-ty-view-node (params)
  "params - a list of 'param
   Returns a ty with metadata == params"
  (make-instance 'ty :name (format nil "VIEW-NODE<~{~a:~a~^,~}>" 
                                   (loop for p in params 
                                         append (list (name p) (name (ty p)))))
                 :metadata params :kind 'prim))

(defun make-ty-struct (fields)
  "Makes a struct type.
   
   The internal representation of a struct is an array, where each item of the
   array corresponds with one of the given struct fields. Struct fields are
   ordered."

  (let ((ret (make-instance 
               'ty :name (format nil "STRUCT<~{~a:~a~^,~}>"
                                 (loop for f in fields append
                                       (list (name f) (name (ty f))))) 
               :metadata fields
               :kind 'struct)))
    (loop for f in fields for ii from 0 do 
          (push (make-instance 
                  'ty-inline-method 
                  :name (name f)
                  :params (list)
                  :ret-ty (ty f)
                  :eval-fn (eval `(lambda (this) (aref this ,ii))))
                (methods ret)))
    ret)) 

(defun make-ty-arr (inner)
  "inner - the inner type of the arr"
  (make-instance 'ty :name (format nil "ARR<~a>" (name inner)) 
                 :metadata inner :kind 'arr))

(defun make-ty-opt (inner)
  "An optional type.
   inner - the inner type of the optional"
  (make-instance 'ty :name (format nil "OPT<~a>" (name inner))
                 :metadata inner :kind 'opt))

(defclass ty-fn-metadata ()
  (;; List of ty
   (params :initarg :params :accessor params :type list)
   (ret :initarg :ret :accessor ret :type ty)))

(defun make-ty-fn (params ret)
  "Function type.
   params - a list of 'ty, NOT a list of params (since functions have positional params)
   ret - the return type of the function"
  (let ((fn-ty
         (make-instance 'ty :name (format nil "fn(~{~a~^,~}):~a" (mapcar #'name params) (name ret))
                        :kind 'fn
                        :metadata (make-instance 'ty-fn-metadata :params params :ret ret))))
    (setf (methods fn-ty) (list (make-instance 'ty-extern-method :name "--CALL" :ty fn-ty)))
    fn-ty))

(defmethod ty-eq ((t0 ty) (t1 ty))
  (string= (name t0) (name t1)))

(defmethod remove-first-param ((ty ty))
  "For a function type (ty == fn), return the type with the first param
   removed, useful for dealing with receivers"
  (assert (eq (kind ty) 'fn))
  (make-ty-fn (cdr (params (metadata ty))) (ret (metadata ty))))

(defmethod can-fn-be-called ((s scope) (ty ty) param-types)
  "Assuming (eq (kind ty) 'fn) (ty is a function type), check if it can be
   called by the given list of types (in param-types). param-types is a list
   of 'ty." 
  (assert (eq (kind ty) 'fn))
  (when (/= (length (params (metadata ty))) (- (length param-types) 1))
    (return-from can-fn-be-called nil))
  (funcall (curry #'every 
                  (lambda (t-list) (is-subtype s (nth 0 t-list) 
                                              (nth 1 t-list))))
           (loop for t0 in (cdr param-types) 
                 for t1 in (params (metadata ty))
                 collect (list t0 t1))))

(defmethod find-ty-method ((s scope) (ty ty) method-name param-types)
  "Search for a all method based on method name and params, return a list or
   nil if not found. If param-types fit into the method, returns true (uses is-subtype)."
  (loop for m in (methods ty) when (string= method-name (name m)) 
        when (can-fn-be-called s (get-method-type m) param-types)
        collect m))

(defun is-method-complete-subtype (s m0 m1)
  "Returns t if m0 is a 'complete' subtype of m1 (i.e. if all of its parameters
   are subtypes of all of m1's parameters, and they have the same number of
   parameters). 
  Returns nil otherwise."
  (let ((m0-params (params (metadata (get-method-type m0))))
        (m1-params (params (metadata (get-method-type m1))))) 
    (when (/= (length m0-params) (length m1-params)) 
      (return-from is-method-complete-subtype nil))
   (every (curry #'is-subtype s) m0-params m1-params)))

(defun remove-nth (n l)
  "Return a new list which is l with the nth item removed."
  (assert (< n (length l)))
  (if (= n 0) (cdr l)
      (cons (car l) (remove-nth (- n 1) (cdr l)))))

(defmacro remove-nth* (n p)
  "A destructive version of remove-nth, for the place 'p'."
  `(setf ,p (remove-nth ,n ,p)))

(defun disambiguate-methods (s method-list)
  "Given a list of possible methods, return a list of 'disambiguated' methods.
   If this list is of length 1, this method can be considered an unambiguous
   override of the others. If this list is longer than 1, the original list of
   methods can be considered ambiguous given the parameter types. A method is
   chosen as an override to another if all of its parameter types are a subtype
   of the other method's."

  ;; Maintain a working list of methods. Initially, this list contains the car
  ;; of method-list.
  ;; For each method in (cdr method-list), compare it to each method in the
  ;; working list.
  ;; If it's a complete subtype to one of them, replace each item of which it's
  ;; a subtype with this method. Deduplicate.
  ;; If it's not, check if it's a supertype. If it is, ignore it. If there
  ;; exists a case where it isn't a supertype, add it to the working list.
  (let ((working-list (list (car method-list)))
        (ii 0))
    (loop for m0 in (cdr method-list) 
          ;; If should-add-m0-to-list is set to true by the end of the inner
          ;; loop, add m0 to the working-list
          with should-add-m0-to-list do 
          (setf ii 0)
          (setf should-add-m0-to-list nil)
          (loop while (< ii (length working-list)) 
                with already-replaced = nil do
                ;; Test for m0 being a subtype
                (if (is-method-complete-subtype s m0 (nth ii working-list)) 
                    (if (not already-replaced)
                        ;; Replace
                        (progn (setf (nth ii working-list) m0)
                               (setf already-replaced t))
                        ;; Delete (i.e. replace + dedup). Remember to decrement
                        ;; ii, since we'll be double-traversing over this
                        ;; position in the list.
                        (progn (remove-nth ii working-list)
                               (setf ii (- ii 1))))
                    ;; Otherwise, if m0 not a subtype, test for m0 being a supertype
                    ;; If it is, make sure we remember to add m0 to the working
                    ;; list after this.
                    (when (not (is-method-complete-subtype s (nth ii working-list) m0))
                      (setf should-add-m0-to-list t)))
                ;; Increment ii
                do (setf ii (+ ii 1)))
          (when should-add-m0-to-list (push m0 working-list)))
    working-list))

(defmethod is-structural-subtype (s t0 t1)
  "Returns t if t0 < t1 on a structural basis.
   If t0 contains all the methods of t1 (same name, same type), it's
   considered a structural subtype.
   Fields, builtin accessors, are all considered methods. Accessing an arr
   by index is simply a method on that type. This way, a struct can be
   considered a subtype of an arr, so long as it implements an arr's
   methods."

  (loop for m1 in (methods t1) do
        ;; For all methods of the same name
        (loop for m0 in (methods t0) 
              when (string= (name m0) (name m1)) do 
              ;; If method doesn't exist, return false
              (when (not m0) (return-from is-structural-subtype nil))
              ;; If method isn't the same type, return false
              (when (not (ty-eq (get-method-type m0) (get-method-type m1)))
                (return-from is-structural-subtype nil))))
  t)

(defmethod is-subtype ((s scope) t0-ty t1-ty)
  "Returns t if t0 < t1, nil otherwise. Non-strict. Returns nil if t0 or t1
   isn't in the type graph."
  ;; Void is a subtype of everything
  (when (ty-eq t0-ty *ty-void*) (return-from is-subtype t))
  ;; Anything is a subtype of any
  (when (ty-eq t1-ty *ty-any*) (return-from is-subtype t))
  ;; Int is a subtype of number
  (when (and (ty-eq t1-ty *ty-num*) (is-subtype s t0-ty *ty-int*)) 
    (return-from is-subtype t))
  ;; When equal, return true
  (when (ty-eq t0-ty t1-ty) (return-from is-subtype t))
  (when (is-structural-subtype s t0-ty t1-ty) (return-from is-subtype t)))
