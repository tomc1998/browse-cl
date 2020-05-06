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

(define-condition layout-error (simple-error) ())
