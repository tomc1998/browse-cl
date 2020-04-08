(in-package #:browse-cl)

(defconstant +pos-inf+ sb-ext:single-float-positive-infinity)
(defconstant +neg-inf+ sb-ext:single-float-negative-infinity)

(defclass layout-annot ()
  ((pos :initarg :pos :accessor pos :type vec2)
   (size :initarg :size :accessor size :type vec2))
  (:documentation "Attached to a concrete DOM node after layout, this gives the
                   node a concrete pixel position relative to its parent."))

(defclass constraint ()
  ((min-val :initarg :min-val :accessor min-val :type float
            :documentation "A value of sb-ext:single-float-negative-infinity
                            indicates an unconstrained value.")
   (max-val :initarg :max-val :accessor max-val :type float
            :documentation "A value of sb-ext:single-float-positive-infinity
                            indicates an unconstrained value.")))

(defun constraint (&optional (min-val +neg-inf+) (max-val +pos-inf+))
  (make-instance 'constraint :min-val min-val :max-val max-val))

(define-condition constraint-intersection-error (error)
  ((c0 :initarg :c0 :reader c0)
   (c1 :initarg :c1 :reader c1))
  (:documentation "If this is signalled, it indicates two constraints (c0 and
                   c1) were incompatible and could not produce a valid
                  intersection.
                  For example, 0 to 300 and 400 to 700. No constraint satisfied
                  both of these constraints."))

(defmethod intersect ((c0 constraint) (c1 constraint))
  "Given 2 constraints, intersect them and calculate a new constraint that
   satisfies both c0 and c1, or error with 'constraint-intersection-error"
  (let ((res (make-instance 'constraint 
                            :min-val (max (min-val c0) (min-val c1))
                            :max-val (min (max-val c0) (max-val c1)))))
    (when (< (max-val res) (min-val res))
      (error 'constraint-intersection-error :c0 c0 :c1 c1))
    res))

(defmethod extract-constraint-hints ((n concrete-dom-node))
  "Based on the following attrs, extract a wcons and hcons:

   min-w min-h max-w max-h w h"
  (let* ((min-w (or (find-attr n "MIN-W") +neg-inf+))
         (max-w (or (find-attr n "MAX-W") +pos-inf+))
         (w (find-attr n "W"))
         (wcons (if w (constraint w w) (constraint min-w max-w)))
         (min-h (or (find-attr n "MIN-H") +neg-inf+))
         (max-h (or (find-attr n "MAX-H") +pos-inf+))
         (h (find-attr n "H"))
         (hcons (if h (constraint h h) (constraint min-h max-h))))
    (values wcons hcons)))

(defmethod layout 
  ((n concrete-dom-node) 
   &optional (wcons (constraint)) (hcons (constraint)))
  "Layout a concrete DOM node"
  (multiple-value-bind 
    (internal-wcons internal-hcons) (extract-constraint-hints n)
    (let ((wcons (intersect wcons internal-wcons))
          (hcons (intersect hcons internal-hcons))) 
      (print (list (min-val wcons) (max-val wcons) 
                   (min-val hcons) (max-val hcons)))
      (cond 
        ((typep n 'concrete-text-node)
         )
        ((and (typep n 'simple-concrete-dom-node) (member (tag n) '(col row))))
        (t (error "Unimpl layout for ~S" n))))))
