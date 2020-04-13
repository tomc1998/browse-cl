(in-package #:browse-cl)

(defclass cst-node () ()
  (:documentation "A concrete syntax tree node."))

(defgeneric to-expr (s c) 
  (:documentation "Turn a cst-node into an expr"))
(defgeneric to-ty (s c) 
  (:documentation "Turn a cst-node into a ty. Not possible for many cst
                   nodes."))
(defmethod to-ty ((s scope) (c cst-node))
  (error "Not possible to turn ~S into a ty." c))

(defclass cst-key-arg (cst-node)
  ((name :initarg :name :accessor name :type string)
   (val :initarg :val :accessor val :type cst-node)))

(defclass cst-fn-call (cst-node)
  ((fn-name :initarg :fn-name :accessor fn-name :type string)
   (key-args :initarg :key-args :accessor key-args :type list
             :documentation "A list of cst-key-arg")
   (pos-args :initarg :pos-args :accessor pos-args :type list
         :documentation "A list of cst-node")))

(defmethod to-expr ((s scope) (c cst-fn-call))
  (let* ((target (find-in-scope s (fn-name c)))
         (arg0 (when (> (length (pos-args c)) 0) 
                 (to-expr s (nth 0 (pos-args c)))))
         (param-types (loop for x in (pos-args c) collect 
                            (get-type (to-expr s x))))
         (method-target 
           (when arg0 
             (car
               (disambiguate-methods 
                 s
                 (find-ty-method s (get-type arg0) 
                                 (fn-name c) param-types))))))
    (when (and (not method-target) (not target)) 
      ;; TODO proper error
      (error "Can't find ~a" (fn-name c)))
    (if (and method-target (= 0 (length (key-args c))))
        ;; If this is a valid method call & there are no key args (which aren't
        ;; valid when callind methods... yet), call a method
        (if (typep method-target 'ty-inline-method) 
            (make-instance 'apply-expr :fn (eval-fn method-target)
                           :ty (ret-ty method-target)
                           :args (mapcar (curry #'to-expr s) (pos-args c)))
            (error "Unimpl method call"))
        ;; Call freestanding fn or create component
        (cond 
          ((eq 'component (kind (get-type target)))
           (let ((attrs (loop for a in (key-args c) collect 
                              (make-instance 'attr :name (name a) 
                                             :val (to-expr s (val a)))))
                 (children (mapcar (curry #'to-expr s) (pos-args c))))
             (cond
               ((string= "TEXT" (fn-name c))
                (make-instance 'template-text-node :attrs attrs :exprs children))
               (t 
                (make-instance 'template-concrete-dom-node 
                               :tag (intern (fn-name c)) :attrs attrs :children children)))))
          ((eq 'fn (kind (get-type target)))
           (error "Unimplemented function calls"))
          (t (error "Expected fn / component"))))))

(defclass cst-set-expr (cst-node)
  ((target :initarg :target :accessor target :type cst-node)
   (val :initarg :val :accessor val :type cst-node))
  (:documentation "A set expression
                   
                   # Examples
                   (set x 123)
                   (set y (+ y 123))"))

(defmethod to-expr ((s scope) (c cst-set-expr))
  (make-instance 'set-expr
                 :place (to-expr s (target c))
                 :val (to-expr s (val c))))

(defclass cst-loop (cst-node)
  ((item-name :initarg :item-name :accessor item-name :type string)
   (target :initarg :target :accessor target :type expr)
   (body :initarg :body :accessor body :type list
         :documentation "A list of cst-node"))
  (:documentation "A for loop
                   
                   # Example
                   (for x in my-list
                        (row (empty :w 40) (text x) (empty :w 40))
                        (text x))"))

(defmethod to-expr ((s scope) (c cst-loop))
  (let* ((target (to-expr s (target c)))
         (target-ty (get-type target))
         (item-loc (make-instance 'stack-alloc 
                                  :ty (metadata target-ty)))
         (loop-scope (subscope s)))
    (set-in-scope s (item-name c) item-loc)
    (make-instance 
      'loop-expr
      :item-loc item-loc
      :target target
      :body (loop for b in (body c) collect 
                  (to-expr loop-scope b)))))

(defclass cst-param (cst-node)
  ((name :initarg :name :accessor name :type string)
   (ty :initarg :ty :accessor ty :type cst-node)))

(defclass cst-fn (cst-node)
  ((params :initarg :params :accessor params :type list
           :documentation "List of cst-param")
   (ret-ty :initarg :ret-ty :accessor ret-ty :type cst-node)
   (body :initarg :body :accessor body :type list
         :documentation "List of cst-node"))
  (:documentation "An anonymous function declaration

                   # Example
                   (fn (a ty b ty c ty) ret-ty 
                       (do-stuff a b c) 
                       (more-stuff))"))

(defmethod to-expr ((s scope) (c cst-fn))
  (make-instance 
    'constant 
    :ty (make-ty-fn (loop for p in (params c) collect (to-ty s (ty p))) 
                    (to-ty s (ret-ty c)))
    :val (eval `(lambda (--internal-env-- ,@(loop for p in (params c) collect (intern (name p))))
                  ,@(loop for b in (body c) collect 
                          `(eval-expr --internal-env-- ,(to-expr s b)))))))

(defclass cst-var (cst-node)
  ((name :initarg :name :accessor name :type string)))

(defmethod to-expr ((s scope) (c cst-var))
  (let ((res (find-in-scope s (name c))))
    (assert (typep res 'expr))
    res))

(defmethod to-ty ((s scope) (c cst-var))
  (let ((res (find-in-scope s (name c))))
    (assert (typep res 'ty))
    res))

(defclass cst-string-lit (cst-node)
  ((val :initarg :val :accessor val :type string)))
(defmethod to-expr ((s scope) (c cst-string-lit)) 
  (make-instance 'constant :ty *ty-string* :val (val c)))

(defclass cst-int-lit (cst-node)
  ((val :initarg :val :accessor val :type integer)))
(defmethod to-expr ((s scope) (c cst-int-lit)) 
  (make-instance 'constant :ty *ty-int* :val (val c)))

(defclass cst-num-lit (cst-node)
  ((val :initarg :val :accessor val :type number)))
(defmethod to-expr ((s scope) (c cst-num-lit)) 
  (make-instance 'constant :ty *ty-num* :val (val c)))

(defclass cst-bool-lit (cst-node)
  ((val :initarg :val :accessor val :type boolean)))
(defmethod to-expr ((s scope) (c cst-bool-lit)) 
  (make-instance 'constant :ty *ty-bool* :val (val c)))

(defclass cst-arr-lit (cst-node)
  ((val :initarg :val :accessor val :type list
        :documentation "List of cst-node")))
(defmethod to-expr ((s scope) (c cst-arr-lit)) 
  (make-instance 'arr-constructor 
                 :vals (mapcar (curry #'to-expr s) (val c))))

(defclass cst-var-decl ()
  ((name :initarg :name :accessor name :type string)
   (ty :initarg :ty :accessor ty :type (or null cst-node))
   (val :initarg :val :accessor val :type cst-node)))

(defmethod to-expr ((s scope) (c cst-var-decl)) 
  (let ((e (to-expr s (val c)))
        (var (find-in-scope s (name c)))) 
    (assert (typep var 'runtime-value))
    (make-instance 'var-decl :var (id var) 
                 :ty (if (ty c) (to-ty s (ty c)) (get-type e))
                 :val e)))
