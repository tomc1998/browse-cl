(in-package #:browse-cl)

(deftype concrete-tag () 
  "Concrete DOM nodes - for HTML, this list would contain div, ul, ol, li,
   span, etc"
  '(member col row text-input empty))

(defclass dom-node-state-value ()
  ((val :initarg :val :accessor val)
   (needs-push :initform nil :accessor needs-push :type boolean
               :documentation 
               "Set when this value changes in such a way that
                it needs to be pushed out to whatever state has
                bound to it. This is useful in the case of 2 way
                bindings, where internal DOM changes need to be
                'pushed' out to global state, but global state
                changes need to be 'pulled' into the DOM. This
                being true indicates the DOM value should be
                pushed out to the global state, rather than the
                other way around

                This should be set to false after pushing out.")))

(defmethod set-internal-dnsv ((d dom-node-state-value) value)
  "Set some dom-node-state-value, updating the needs-push flag to true.
   Setting with this method, rather than a typical setf, will ensure this
   value is pushed out to any bound global vars."
  (setf (val d) value)
  (setf (needs-push d) t))

(defparameter *curr-persist-id* 0)
(defun gen-persist-id () 
  "Generate a new persistent ID"
  (- (incf *curr-persist-id*) 1))

(defclass dom-node-state ()
  ((persist-id :initform (gen-persist-id) :accessor persist-id :type integer
               :documentation 
               "An ID which can be used to track nodes through DOM updates -
                assuming the persistence is not lost. State persists when the
                previous and following dom expansions both expand to a single
                node.")
   (hover :initform (make-instance 'dom-node-state-value :val nil) 
          :accessor hover :type dom-node-state-value
          :documentation "True when this DOM node is hovered")
   (scroll-y :initform (make-instance 'dom-node-state-value :val 0.0)
             :accessor scroll-y :type dom-node-state-value
             :documentation "Offsets children by the given y value")
   (focused :initform (make-instance 'dom-node-state-value :val nil)
             :accessor focused :type dom-node-state-value
             :documentation "Only applicable for text-input")
   (text :initform (make-instance 'dom-node-state-value :val "")
         :accessor text :type dom-node-state-value
         :documentation "Only applicable for text-input")))

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

(defmethod get-type ((e dom-node)) *ty-dom-node-instance*)

(defclass template-dom-node (dom-node) 
  ((dependent-env-vals 
     :initarg :dependent-env-vals 
     :initform (make-array '(0) :element-type 'var-id 
                           :fill-pointer 0 :adjustable t)
     :accessor dependent-env-vals 
     :type (array var-id)
     :documentation "An array of environment IDs |should all be positive, since
                     referring to stack IDs here makes no sense|
                     If these environment IDs are changed, this node should be
                     re-expanded.
                     This can contain duplicates, and may well do in most cases.")
   (related-concrete-nodes
     :initarg :related-concrete-nodes
     :accessor related-concrete-nodes
     :type (or null list)
     :documentation "A reference to the concrete dom node(s) this expanded to
                     previously - or nil if not expanded yet"))
  (:documentation "Any template DOM node. This can be higher level control
                   structures, like loops and conditionals, or normal DOM nodes
                   with templated attributes."))

(defmethod find-dependent-env-vals ((n template-dom-node))
  (loop for x across (dependent-env-vals n) collect x))

(defgeneric find-nodes-which-need-expanding (env template-dom-node)
  (:documentation "Given a root-level node, find the first top level nodes that
                   need expanding in this tree, in order to sync the tree up to
                   the program's state."))
(defmethod find-nodes-which-need-expanding ((e env) (n template-dom-node))
  (let ((res (list)))
   (walk-expr n (lambda (x val)
                  (when (and (not val) 
                             (typep x 'template-dom-node) 
                             (dom-needs-expanding e x)) 
                    (push x res)
                    x)) 
             nil)
   res))

(defgeneric dom-needs-expanding (env template-dom-node)
  (:documentation "Returns true if the global state that this dom node relies
                   on has changed since the last call to clear-dirty-globals.
                   NOTE: Make sure to call init-dependent-env-vals before this."))
(defmethod dom-needs-expanding ((env env) (e expr))
  (loop for x in (find-dependent-env-vals e)
        when (not (realp x)) do (inspect e)
        when (is-dirty env x) return t))
(defmethod dom-needs-expanding ((e env) (n template-dom-node))
  (loop for x across (dependent-env-vals n) when (is-dirty e x) return t))

(defgeneric init-dependent-env-vals (template-dom-node) 
  (:documentation "Call after creating a template-dom-node to set the
                   dependent-env-vals value for this node. will NOT set the
                   dependent-env-vals for its children, so use walk-expr for
                   this."))

(defmethod init-dependent-env-vals ((e expr)))

(defmethod init-dependent-env-vals ((e loop-expr))
  (setf (dependent-env-vals e) (apply #'vector (find-dependent-env-vals e))))

(defmethod eval-expr ((env env) (n template-dom-node))
  (expand-template-dom-node env n))

(defclass template-text-node (template-dom-node)
  ((attrs :initarg :attrs :accessor attrs :type list
         :documentation "List of attr")
   (exprs :initarg :exprs :accessor exprs :type list
          :documentation "A list of exprs, which when evaluated, formatted to a
                          string, then combined together, produce a single
                          string for use in a concrete-text-node")))
(defmethod init-dependent-env-vals ((n template-text-node))
  (let ((res (make-array '(0) 
                         :element-type 'var-id 
                         :fill-pointer 0 :adjustable t))) 
    (loop for a in (attrs n) do 
          (loop for x in (find-dependent-env-vals (val a)) do
                (vector-push-extend x res)))
    (loop for e in (exprs n) do
          (loop for x in (find-dependent-env-vals e) do
                (vector-push-extend x res)))
    (setf (dependent-env-vals n) res)))

(defclass template-concrete-dom-node (template-dom-node)
 ((tag :initarg :tag :accessor tag :type concrete-tag)
  (attrs :initarg :attrs :accessor attrs :type list
         :documentation "List of attr")
  (children :initarg :children :accessor children :type list :initform (list)
            :documentation "A list of template-concrete-dom-node children."))
 (:documentation "This functions exactly like a normal concrete-dom-node, but
                  has templated attributes."))
(defmethod init-dependent-env-vals ((n template-concrete-dom-node))
  (let ((res (make-array '(0) 
                         :element-type 'var-id 
                         :fill-pointer 0 :adjustable t))) 
    (loop for a in (attrs n) do 
          (loop for x in (find-dependent-env-vals (val a)) do
                (vector-push-extend x res)))
    (setf (dependent-env-vals n) res)))
(defmethod walk-expr ((d expr) fn &optional (val nil)) (walk-expr d fn val))
(defmethod walk-expr ((d template-text-node) fn &optional (val nil)) (funcall fn d val))
(defmethod walk-expr ((d template-concrete-dom-node) fn &optional (val nil)) 
  (let ((val (funcall fn d val)))
    (loop for c in (children d) do (walk-expr c fn val))))

(defclass template-fat-dom-node (template-dom-node)
 ((component :initarg :component :accessor component :type component)
  (attrs :initarg :attrs :accessor attrs :type list
         :documentation "List of attr")
  (children :initarg :children :accessor children :type list :initform (list)
            :documentation "A list of template-concrete-dom-node children."))
 (:documentation "A 'fat' dom node, i.e. an instantiation of a component with
                  user-defined internal state"))
(defmethod walk-expr ((d template-fat-dom-node) fn &optional (val nil)) 
  (walk-expr (view (component d)) fn val))
(defmethod init-dependent-env-vals ((n template-fat-dom-node))
  (let ((res (make-array '(0) 
                         :element-type 'var-id 
                         :fill-pointer 0 :adjustable t))) 
    (loop for a in (attrs n) do 
          (loop for x in (find-dependent-env-vals (val a)) do
                (vector-push-extend x res)))
    (setf (dependent-env-vals n) res)))
(defmethod dom-needs-expanding ((e env) (n template-fat-dom-node))
  (loop for x across (dependent-env-vals n) when (is-dirty e x) return t))

(defclass concrete-dom-node (dom-node) 
  ((attrs :initarg :attrs :accessor attrs :type list
          :documentation "List of const-attr")
   (state :initform (make-instance 'dom-node-state) 
          :accessor state :type dom-node-state
          :documentation "The internal state of this DOM node. This can be
                          bound with the bind-* attribs, which will keep in
                          sync some place & this state.")
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

(defmethod find-with-persist-id ((root concrete-dom-node) id)
  "Given a persist-id, find the node that matches, or nil if not available (it
   probably failed to persist through an update)"
  (let ((ret nil)) 
    (walk-expr root 
               (lambda (n val) 
                 (declare (ignore val)) 
                 (when (= id (persist-id (state n))) (setf ret n))))
    ret))

(defmethod sync-bindings ((env env) (d concrete-dom-node))
  "Walk the given tree & sync all 2 way bindings to the global state"
  (walk-expr 
    d (lambda (x val)
        (declare (ignore val))
        (when (typep x 'concrete-dom-node)
          ;; Find bind attrs
          ;; TODO loop over all bind-state attrs, rather than hardcoding all here
          (let ((attr (find-attr x "BIND-STATE-HOVER")))
            (when attr
              (let ((dnsv (hover (state x))))
                (if (needs-push dnsv)
                    (progn
                      (set-in-place env 
                                  (eval-expr-place env (val attr))
                                  (val dnsv))
                      (setf (needs-push dnsv) nil))
                    (setf (val dnsv) (eval-expr env (val attr)))))))
         (let ((attr (find-attr x "BIND-STATE-FOCUSED")))
            (when attr
              (let ((dnsv (focused (state x))))
                (if (needs-push dnsv)
                    (progn
                      (set-in-place env 
                                  (eval-expr-place env (val attr))
                                  (val dnsv))
                      (setf (needs-push dnsv) nil))
                    (setf (val dnsv) (eval-expr env (val attr))))))) 
         (let ((attr (find-attr x "BIND-STATE-TEXT")))
           (when attr
             (let ((dnsv (text (state x))))
               (if (needs-push dnsv)
                   (progn
                     (set-in-place env 
                                   (eval-expr-place env (val attr))
                                   (val dnsv))
                     (setf (needs-push dnsv) nil))
                   (progn
                     (setf (val dnsv) (eval-expr env (val attr))))))))))))

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

(defmethod walk-expr ((d concrete-text-node) fn &optional (val nil)) (funcall fn d val))
(defmethod walk-expr ((d simple-concrete-dom-node) fn &optional (val nil)) 
  (let ((val (funcall fn d val)))
    (loop for c in (children d) do (walk-expr c fn val))))


(defmethod find-font-name-for-text-node ((n concrete-dom-node))
  ;; TODO implement
  "IBMPlexSans-Regular.otf")

(defmethod find-font-size-for-text-node ((n concrete-dom-node))
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
      ((vectorp val) 
       (let ((ret (loop for v across val append 
                        (cond ((vectorp v) (coerce v 'list))
                              ((listp v) v)
                              (t (list v))))))
         (when (typep expr 'loop-expr)
           (setf (related-concrete-nodes expr) ret))))
      ((listp val) val)
      (t (list
           (make-instance
             'concrete-text-node
             :attrs '()
             :val (format nil "~a" val)))))))

(defmethod expand-template-dom-node ((e env) (n template-concrete-dom-node))
  (let ((ret (list (make-instance
                     'simple-concrete-dom-node
                     :tag (tag n) 
                     :attrs (loop for a in (attrs n) 
                                  if (and (>= (length (name a)) 11) 
                                          (string= "BIND-STATE-" (subseq (name a) 0 11))) collect
                                  (make-instance 
                                    'attr 
                                    :name (name a)
                                    :val (val a))
                                  else collect 
                                  (make-instance 
                                    'const-attr 
                                    :name (name a)
                                    :val (make-instance 'constant :ty (get-type (val a)) 
                                                        :val (eval-expr e (val a)))))
                     :children (loop for c in (children n) append
                                     (expand-template-dom-node e c))))))
    (setf (related-concrete-nodes n) ret)
    ret))

(defmethod expand-template-dom-node ((e env) (n template-text-node))
  (let ((ret (list (make-instance
                     'concrete-text-node
                     :attrs (loop for a in (attrs n) collect 
                                  (make-instance 
                                    'const-attr
                                    :name (name a) 
                                    :val (make-instance 'constant :ty (get-type (val a)) 
                                                        :val (eval-expr e (val a)))))
                     :val (apply (curry #'concatenate 'string) 
                                 (loop for c in (exprs n) collect
                                       (format nil "~a" (eval-expr e c))))))))
    (setf (related-concrete-nodes n) ret)
    ret))

(defmethod expand-template-dom-node ((e env) (n template-fat-dom-node))
  (expand-template-dom-node e (view (component n))))


(defmethod expand-template-dom-node ((e env) (n concrete-dom-node)) (list n))
