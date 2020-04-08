(in-package #:browse-cl)

(define-condition browser-parse-error (error)
  ((msg :initform :msg :type string :reader msg)
   (expr :initform :expr :reader expr
         :documentation "The sexpr that failed to parse")
   (tl-expr :initform :tl-expr :reader tl-expr
            :documentation "The top-level expr this parse error occurred in"
            )))
