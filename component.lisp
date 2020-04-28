;; Handles expanding a cst-component into a regular component

(in-package #:browse-cl)

(defclass component ()
  ((view :initarg :view :accessor view :type template-dom-node)))

(defmethod expand-component ((c cst-component) attrs children)
  "Expand c into a regular template component, given the args

   @param attrs - a list of 'attr
   @param children - a list of 'expr"
  (let ((s (create-global-scope)))
    (loop for a in attrs do (set-in-scope s (name a) (val a)))
    (loop for p in (remove-if-not #'pos (params c)) 
          for val in children do 
          (set-in-scope s (name p) val))
    ;; Setup constants
    (loop for const in (const c) do
          (set-in-scope s (name const) (to-expr s (val const))))
    (loop for child in children for p in (params c) do 
          (set-in-scope s (name p) child))
    (make-instance 'component
                   :view (to-expr s (root (view c))))))

