;; Handles expanding a cst-component into a regular component

(in-package #:browse-cl)

(defclass component ()
  ((view :initarg :view :accessor view :type template-dom-node)))

(defmethod expand-component ((s scope) (c cst-component) attrs children)
  "Expand c into a regular template component, given the args

   @param attrs - a list of 'attr
   @param children - a list of 'expr"
  (let ((s (subscope s)))
    (loop for a in attrs do (set-in-scope s (name a) (val a)))
    (loop for p in (remove-if-not #'pos (params c)) 
          for val in children do 
          (set-in-scope s (name p) val))

    ;; Setup constants. Repeatedly eval, catching var-not-found, until
    ;; everything evals correctly.
    (loop named eval-loop with succeeded-at-least-once and evaluated = (list) do 
          (setf succeeded-at-least-once nil)
          when (not (loop named inner for const in (const c) with errored = nil 
                          when (not (member (name const) evaluated :test #'string=))
                          do (handler-case
                               (progn 
                                 (set-in-scope s (name const) (to-expr s (val const)))
                                 (push (name const) evaluated)
                                 (setf succeeded-at-least-once t))
                               (var-not-found () (setf errored t)))
                          finally (return-from inner errored)))
          do (return-from eval-loop)
          when (not succeeded-at-least-once) do
          ;; Since a var hasn't been found, trigger error... TODO cache this
          ;; from previous step
          (error
            "Can't find vars ~{~a~^, ~}" 
            (remove-if #'null (loop for const in (const c) collect 
                                    (handler-case (progn (to-expr s (val const)) nil) 
                                      (var-not-found (e) (name e)))))))

     ;; Eval view
     (make-instance 'component
                    :view (to-expr s (root (view c))))))

