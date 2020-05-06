(in-package #:browse-cl)

(define-condition browser-parse-error (error)
  ((msg :initform :msg :type string :reader msg)
   (expr :initform :expr :reader expr
         :documentation "The sexpr that failed to parse")
   (tl-expr :initform :tl-expr :reader tl-expr
            :documentation "The top-level expr this parse error occurred in"
            )))

(define-condition var-not-found (error)
  ((name :initarg :name :type string :reader name
         :documentation "The name of the var")))

(define-condition layout-overflow-error (error)
  ((n :initarg :n :reader n :type concrete-dom-node
      :documentation "The concrete-dom-node that failed to layout")
   (wcons :initarg :wcons :reader wcons :type constraint)
   (hcons :initarg :hcons :reader hcons :type constraint)))

(define-condition constraint-intersection-error (error)
  ((n :initarg :n :reader n)
   (c0 :initarg :c0 :reader c0)
   (c1 :initarg :c1 :reader c1))
  (:documentation "If this is signalled, it indicates two constraints (c0 and
                   c1) were incompatible and could not produce a valid
                  intersection.
                  For example, 0 to 300 and 400 to 700. No constraint satisfied
                  both of these constraints."))

(define-condition layout-error (simple-error) ())

(defgeneric pretty-print-dom-node (d))
(defmethod pretty-print-dom ((d concrete-dom-node)) "Unknown")
(defmethod pretty-print-dom ((d simple-concrete-dom-node))
  (format nil "(~a ...)" (tag d)))
(defmethod pretty-print-dom ((d concrete-text-node))
  (format nil "(TEXT \"~a   . . .\")" (subseq (val d) 0 16)))

(defun pretty-print-error (e)
  "Return a string, the pretty-printed form of the given error"
  (cond
    ((typep e 'browser-parse-error)
     (format nil "Parse of ~S failed: ~a" (tl-expr e) (msg e)))
    ((typep e 'layout-overflow-error)
     (format nil "Node ~a failed to layout under constraints (~a:~a, ~a:~a)"  
             (pretty-print-dom-node (n e))
             (min-val (wcons e)) (max-val (wcons e))
             (min-val (hcons e)) (max-val (hcons e))))
    ((typep e 'constraint-intersection-error)
     (format nil "Node ~a failed to layout, constraint ~a : ~a is impossible to satisfy."
             (pretty-print-dom (n e))
             (floor (min-val (c1 e))) (floor (max-val (c1 e)))))
    ((typep e 'var-not-found)
     (format nil "Var '~(~a~)' not found" (name e)))
    (t (format nil "~S" e))))

