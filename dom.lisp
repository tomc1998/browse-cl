(in-package #:browse-cl)

(deftype concrete-tag () 
  "Concrete DOM nodes - for HTML, this list would contain div, ul, ol, li,
   span, etc"
  '(member col row empty))

(defclass attr ()
  ((name :initarg :name :accessor name :type string)
   (val :initarg :val :accessor val :type expr))
  (:documentation "A DOM node attribute."))

(defclass const-attr ()
  ((name :initarg :name :accessor name :type string)
   (val :initarg :val :accessor val :type constant))
  (:documentation "A const-attr is like an attr, but with only constant values.
                   It can therefore be used for rendering, without any
                   additional processing.")) 

(defclass dom-node (expr) ()
  (:documentation "Any DOM node, either belonging to the template dom or the
                   concrete dom. A concrete dom can be rendered, whereas a
                   template dom cannot. A template dom will produce a concrete
                   dom."))

(defgeneric walk-dom (root fn &optional val)
  (:documentation "Walk the dom, calling (fn node val) on each node in a
                   depth-first walk of the tree. 'val' is the value of fn
                   called with the parent function. For the root node, a value
                   of 'nil' is provided: although another default val can be provided
                   through the optional 'val' parameter."))

(defmethod get-type ((e dom-node)) *ty-dom-node-instance*)

(defclass template-dom-node (dom-node) ()
  (:documentation "Any template DOM node. This can be higher level control
                   structures, like loops and conditionals, or normal DOM nodes
                   with templated attributes."))

(defmethod eval-expr ((env env) (n template-dom-node))
  (expand-template-dom-node env n))

(defclass template-text-node (template-dom-node)
  ((attrs :initarg :attrs :accessor attrs :type list
         :documentation "List of attr")
   (exprs :initarg :exprs :accessor exprs :type list
          :documentation "A list of exprs, which when evaluated, formatted to a
                          string, then combined together, produce a single
                          string for use in a concrete-text-node")))

(defclass template-concrete-dom-node (template-dom-node)
 ((tag :initarg :tag :accessor tag :type concrete-tag)
  (attrs :initarg :attrs :accessor attrs :type list
         :documentation "List of attr")
  (children :initarg :children :accessor children :type list :initform (list)
            :documentation "A list of template-concrete-dom-node children."))
 (:documentation "This functions exactly like a normal concrete-dom-node, but
                  has templated attributes."))

(defclass concrete-dom-node (dom-node) 
  ((attrs :initarg :attrs :accessor attrs :type list
          :documentation "List of const-attr")
   (layout-annot :initform nil :accessor layout-annot 
                 :type (or layout-annot null)
                 :documentation "This is set when layed out - see layout.lisp.")
   (render-annot :initform nil :accessor render-annot 
                 :type (or render-annot null)
                 :documentation "This is set when rendered - see renderer.lisp.
                                 The true value of this depends on the type of
                                 DOM node.")) 
 (:documentation "This class is a node, many of which makes up a concrete DOM
                   which can be rendered.")) 

(defclass simple-concrete-dom-node (concrete-dom-node)
  ((tag :initarg :tag :accessor tag :type concrete-tag)
   (children :initarg :children :accessor children :type list :initform (list)
             :documentation "A list of concrete-dom-node children.")))

(defclass concrete-text-node (concrete-dom-node)
  ((val :initarg :val :accessor val :type string))
  (:documentation "This is a special case of a concrete-dom-node. It is a text
                   node, where 'children' is always empty. Instead, it contains
                   a single 'val', which contains the string of this text
                   node."))

(defmethod walk-dom ((d concrete-text-node) fn &optional (val nil)) (funcall fn d val))
(defmethod walk-dom ((d simple-concrete-dom-node) fn &optional (val nil)) 
  (let ((val (funcall fn d val)))
    (loop for c in (children d) do (walk-dom c fn val))))


(defmethod find-font-name-for-text-node ((n concrete-text-node))
  ;; TODO implement
  "IBMPlexSans-Regular.otf")

(defmethod find-font-size-for-text-node ((n concrete-text-node))
  (let ((fs-attr (find-attr n "FONT-SIZE"))) 
    (if fs-attr (val (val fs-attr)) 18)))

(defmethod find-font-for-text-node ((n concrete-text-node))
  (load-font (find-font-name-for-text-node n) 
             (find-font-size-for-text-node n)))

(defmethod find-attr ((n concrete-dom-node) name)
  "name - a string, name of the attr (upcase)
   Returns nil if no attr found"
  (find-if (lambda (x) (string= name (name x))) (attrs n)))

(defmethod expand-template-dom-node ((e env) (expr expr))
  (let ((val (eval-expr e expr)))
    (cond
      ((vectorp val) (loop for v across val append 
                           (cond ((vectorp v) (coerce v 'list))
                                 ((listp v) v)
                                 (ti (list v)))))
      ((listp val) val)
      (t (list
           (make-instance
             'concrete-text-node
             :attrs '()
             :val (format nil "~a" val)))))))

(defmethod expand-template-dom-node ((e env) (n template-concrete-dom-node))
  (list (make-instance
    'simple-concrete-dom-node
    :tag (tag n) 
    :attrs (loop for a in (attrs n) collect 
                 (make-instance 
                   'const-attr 
                   :name (name a)
                   :val (make-instance 'constant :ty (get-type (val a)) 
                                       :val (eval-expr e (val a)))))
    :children (loop for c in (children n) append
                    (expand-template-dom-node e c)))))

(defmethod expand-template-dom-node ((e env) (n template-text-node))
  (list (make-instance
    'concrete-text-node
    :attrs (loop for a in (attrs n) collect 
                 (make-instance 
                   'const-attr
                   :name (name a) 
                   :val (make-instance 'constant :ty (get-type (val a)) 
                                       :val (eval-expr e (val a)))))
    :val (apply (curry #'concatenate 'string) 
                (loop for c in (exprs n) collect
                      (format nil "~a" (eval-expr e c))))))
  
  )

(defmethod expand-template-dom-node ((e env) (n concrete-dom-node)) (list n))
