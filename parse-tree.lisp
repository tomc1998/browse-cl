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
         (pos-arg-exprs (loop for x in (pos-args c) collect 
                            (to-expr s x)))
         (arg0 (when (> (length (pos-args c)) 0) (first pos-arg-exprs)))
         (param-types (loop for x in pos-arg-exprs collect 
                            (get-type x)))
         (method-target 
           (when arg0 
             (car
               (disambiguate-methods 
                 s
                 (find-ty-method s (get-type arg0) 
                                 (fn-name c) param-types))))))
    (when (and (not method-target) (not target)) 
      (error 'var-not-found :name (fn-name c)))
    (if (and method-target (= 0 (length (key-args c))))
        ;; If this is a valid method call & there are no key args (which aren't
        ;; valid when callind methods... yet), call a method
        (if (typep method-target 'ty-inline-method) 
            (make-instance 'apply-expr 
                           :name (fn-name c) :fn (eval-fn method-target)
                           :ty (ret-ty method-target)
                           :args pos-arg-exprs)
            (error "Unimpl method call"))
        ;; Call freestanding fn or create component
        (cond 
          ((eq 'component (kind (get-type target)))
           (let ((attrs (loop for a in (key-args c) collect 
                              (make-instance 'attr :name (name a) 
                                             :val (to-expr s (val a)))))
                 (children pos-arg-exprs))
             (cond
               ((string= "TEXT" (fn-name c))
                (make-instance 'template-text-node :attrs attrs :exprs children))
               ((not (metadata (get-type target))) 
                (make-instance 'template-concrete-dom-node 
                               :tag (intern (fn-name c)) :attrs attrs :children children))
               ;; Fat component instantiation (not just a template dom node
               (t (make-instance 
                    'template-fat-dom-node
                    :component 
                    (expand-component s (metadata (get-type target)) 
                                      attrs children)
                    :attrs attrs
                    :children children)))))
          ((eq 'fn (kind (get-type target)))
           ;; Call the function
           (let ((params (params (metadata (get-type target)))))
             (when (/= 0 (length params)) 
               (error "Unimplemented function calls with params"))
             (make-instance 'apply-expr 
                            :name (fn-name c)
                            :fn (val target)
                            :args (list)
                            :with-env t
                            :ty (ret (metadata (get-type target))))))
          (t (error "Expected fn / component"))))))

(defclass cst-push-expr (cst-node)
  ((target :initarg :target :accessor target :type cst-node)
   (val :initarg :val :accessor val :type cst-node))
  (:documentation "A push expression
                   
                   # Examples
                   (var my-list (arr 1 2 3))
                   (push 4 my-list)"))

(defmethod to-expr ((s scope) (c cst-push-expr))
  (make-instance 'push-expr
                 :place (to-expr s (target c))
                 :val (to-expr s (val c))))

(defclass cst-request-expr (cst-node)
  ((ret-ty :initarg :ret-ty :accessor ret-ty :type cst-node)
   (url :initarg :url :accessor url :type cst-node)
   (args :initarg :args :accessor args :type list))
  (:documentation "# Examples
                   (request int \"localhost:9898/num-page-views\" ...)"))
(defmethod to-expr ((s scope) (c cst-request-expr))
  (make-instance 'request-expr 
                 :ret-ty (to-ty s (ret-ty c)) 
                 :url (to-expr s (url c))
                 :args (mapcar (curry #'to-expr s) (args c))))

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

(defclass cst-if (cst-node)
  ((cond-expr :initarg :cond-expr :accessor cond-expr :type cst-node)
   (t-expr :initarg :t-expr :accessor t-expr :type cst-node)
   (f-expr :initarg :f-expr :accessor f-expr :type cst-node)))
(defmethod to-expr ((s scope) (c cst-if))
  (make-instance 'if-expr
                 :cond-expr (to-expr s (cond-expr c))
                 :t-expr (to-expr s (t-expr c))
                 :f-expr (to-expr s (f-expr c))))

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

(defclass cst-fn-ty (cst-node)
  ((params :initarg :params :accessor params :type list
           :documentation "List of cst-node")
   (ret-ty :initarg :ret-ty :accessor ret-ty :type cst-node))
  (:documentation "A function type specifier. For a function
                   declaration, see cst-fn.

                   # Example
                   (fn (int int) int)"))

(defmethod to-ty ((s scope) (c cst-fn-ty))
  (make-ty-fn (mapcar (curry #'to-ty s) (params c))
              (to-ty s (ret-ty c))))

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
                  (declare (ignorable --internal-env--
                                      ,@(loop for p in (params c) collect 
                                              (intern (name p)))))
                  ,@(loop for b in (body c) collect 
                          `(eval-expr --internal-env-- ,(to-expr s b)))))))

(defclass cst-var (cst-node)
  ((name :initarg :name :accessor name :type string)))

(defmethod to-expr ((s scope) (c cst-var))
  (let ((res (find-in-scope s (name c))))
    (assert res () 'var-not-found :name (name c))
    (assert (typep res 'expr) ())
    res))

(defmethod to-ty ((s scope) (c cst-var))
  (let ((res (find-in-scope s (name c))))
    (assert res () 'var-not-found :name (name c))
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

(defclass cst-struct-lit (cst-node)
  ((ty :initarg :ty :accessor ty :type cst-node
       :documentation "The type of the struct to make")
   (key-args :initarg :key-args :accessor key-args :type list
             :documentation "List of cst-key-arg"
             ))
  (:documentation "# Example
                   (make-struct foo :name \"Tom\" :id 0)"))
(defmethod to-expr ((s scope) (c cst-struct-lit))
  (make-instance 
    'struct-constructor :ty (to-ty s (ty c))
    :fields (loop for a in (key-args c) collect
                  (make-instance 'attr :name (name a) :val (to-expr s (val a))))))

(defclass cst-arr-ty (cst-node)
  ((val :initarg :val :accessor val :type list
        :documentation "A type descriptor for an array
                        # Example
                        (arr-t int)")))
(defmethod to-ty ((s scope) (c cst-arr-ty))
  "Turn this into a type descriptor, assuming there is only one argument, and
   this argument can be turned into a type."
  (make-ty-arr (to-ty s (val c))))

(defclass cst-struct-field (cst-node)
  ((name :initarg :name :accessor name :type string)
   (ty :initarg :ty :accessor ty :type cst-node)))

(defclass cst-struct-ty (cst-node)
  ((fields :initarg :fields :accessor fields :type list
           :documentation "List of cst-struct-field")) 
  (:documentation "A type descriptor for a struct
                   # Example
                   (struct
                     name string
                     age int)"))
(defmethod to-ty ((s scope) (c cst-struct-ty))
  "Turn this into a type descriptor, assuming there is only one argument, and
   this argument can be turned into a type."
  (make-ty-struct 
    (loop for f in (fields c) collect
          (make-instance 'struct-field
                         :name (name f)
                         :ty (to-ty s (ty f))))))

(defclass cst-typedef ()
  ((name :initarg :name :accessor name :type string)
   (val :initarg :val :accessor val :type cst-node))
  (:documentation "# Example
                   (type entity-id int)
                   (type int-array (arr int))
                   (type 2d-int-array (arr (arr (int)))"))

(defclass cst-var-decl ()
  ((name :initarg :name :accessor name :type string)
   (ty :initarg :ty :accessor ty :type (or null cst-node))
   (val :initarg :val :accessor val :type cst-node)))
(defmethod to-expr ((s scope) (c cst-var-decl)) 
  (let ((e (to-expr s (val c)))
        (var (find-in-scope s (name c)))) 
    (assert var () 'var-not-found :name (name c))
    (assert (typep var 'runtime-value))
    (make-instance 'var-decl :var (id var) 
                 :ty (if (ty c) (to-ty s (ty c)) (get-type e))
                 :val e)))

(defclass cst-every ()
  ((millis :initarg :millis :accessor millis :type expr)
   (exprs :initarg :exprs :accessor exprs :type list))
  (:documentation "# Example
                   ;; Run the exprs every second
                   (every 1000 ...)"))
