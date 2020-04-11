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

(defclass cst-dom-node (cst-node)
  ((tagname :initarg :tagname :accessor tagname :type symbol)
   (attrs :initarg :attrs :accessor attrs :type list
          :documentation "A list of cst-key-arg")
   (children :initarg :children :accessor children :type list
             :documentation "A list of cst-node")))

(defmethod to-expr ((s scope) (c cst-dom-node))
  (let
    ((attrs (loop for a in (attrs c) collect 
                  (make-instance 'attr :name (name a) 
                                 :val (to-expr s (val a)))))
     (children (mapcar (curry #'to-expr s) (children c))))
    (cond
    ((eq 'text (tagname c))
     (make-instance 'template-text-node :attrs attrs :exprs children))
    (t 
     (make-instance 'template-concrete-dom-node 
                    :tag (tagname c) :attrs attrs :children children)))))

(defclass cst-fn-call (cst-node)
  ((fn-name :initarg :fn-name :accessor fn-name :type string)
   (key-args :initarg :key-args :accessor key-args :type list
             :documentation "A list of cst-key-arg")
   (pos-args :initarg :pos-args :accessor pos-args :type list
         :documentation "A list of cst-node")))

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
