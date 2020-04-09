(in-package #:browse-cl)

(defconstant +pos-inf+ sb-ext:single-float-positive-infinity)

(defclass layout-annot ()
  ((pos :initform (vec2 0.0 0.0) :initarg :pos :accessor pos :type vec2)
   (size :initarg :size :accessor size :type vec2))
  (:documentation "Attached to a concrete DOM node after layout, this gives the
                   node a concrete pixel position relative to its parent."))

(defclass constraint ()
  ((min-val :initarg :min-val :accessor min-val :type float
            :documentation "A value of 0 indicates an unconstrained value.")
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
  (let* ((a-min-w (find-attr n "MIN-W"))
         (a-max-w (find-attr n "MAX-W"))
         (min-w (if a-min-w (val (val a-min-w)) 0))
         (max-w (if a-max-w (val (val a-max-w)) +pos-inf+))
         (w (find-attr n "W"))
         (wcons (if w (constraint (val (val w)) (val (val w))) 
                    (constraint min-w max-w)))

         (a-min-h (find-attr n "MIN-H"))
         (a-max-h (find-attr n "MAX-H"))
         (min-h (if a-min-h (val (val a-min-h)) 0))
         (max-h (if a-max-h (val (val a-max-h)) +pos-inf+))
         (h (find-attr n "H"))
         (hcons (if h (constraint (val (val h)) (val (val h))) 
                    (constraint min-h max-h))))
    (values wcons hcons)))

(defmethod get-layout-weight ((n concrete-dom-node))
  "Find the value of the :weight attr
   Returns 0 if weight isn't specified in attrs"
  (let ((attr (find-attr n "WEIGHT")))
    (if attr (val (val attr)) 0)))

(defmethod layout-flex ((n concrete-dom-node) wcons hcons)
  "Given a node which MUST be a col or row, layout a col/row with the given
   wcons and hcons. wcons and hcons should be fully pre-calculated."
  (assert (member (tag n) '(col row)))
  (let*
    ((is-col (eq (tag n) 'col))
     (space-left (if is-col (max-val hcons) (max-val wcons)))
     ;; Cross-axis constraint
     (child-cross-cons (if is-col (constraint 0 (max-val wcons)) 
                           (constraint 0 (max-val hcons)))))
    (loop for c in (children n)
          when (= (get-layout-weight n) 0) do
          (if is-col 
              (layout c 0 0 child-cross-cons (constraint 0 space-left))
              (layout c 0 0 (constraint 0 space-left) child-cross-cons))
          (assert (layout-annot c))
          (decf space-left (if is-col (y (size (layout-annot c))) 
                               (x (size (layout-annot c)))))
          (when (< space-left 0)
            (error 'layout-overflow-error :n n 
                   :wcons wcons :hcons hcons))
          )
    (error "Unimpl")))

(defmethod find-font-for-text-node ((n concrete-text-node))
  ;; TODO implement
  (load-font "IBMPlexSans-Regular.otf" 18))

(defun measure-wrapped-text (font text &optional (width +pos-inf+))
  "Return (values w h), which is the width / height for this text, wrapped
   with the given width.
   text - the string to measure
   font - the font to use for rendering
   width - the maximum width of the text, used for wrapping "
  (when (= 0 (length text))
    (return-from measure-wrapped-text (values 0 0)))
  
  (let* ((lines (wrap-text font text width))
         (line-size (multiple-value-list (sdl2-ttf:size-text font (car lines)))))
    (if (= 1 (length lines))
        (values-list line-size)
        (values width (* (length lines) (nth 1 line-size))))))

(defmethod layout-text ((n concrete-text-node) wcons hcons)
  "Layout a text node"
  (let* ((font (find-font-for-text-node n))
         (text-size (multiple-value-list (measure-wrapped-text font (val n) (max-val wcons)))))
    (when (> (nth 1 text-size) (max-val hcons))
      (error 'layout-overflow-error :n n :wcons wcons :hcons hcons))
    (setf (layout-annot n) 
          (make-instance 'layout-annot 
                         :size (vec2 (float (max (min-val wcons) (nth 0 text-size)))
                                     (float (max (min-val hcons) (nth 1 text-size))))))))

(defmethod layout 
  ((n concrete-dom-node) 
   &optional 
   (x 0) (y 0)
   (wcons (constraint)) (hcons (constraint)))
  "Layout a concrete DOM node

   n - the node to layout
   x, y - The offset RELATIVE TO THE PARENT of this dom node
   wcons, hcons - the width / height constraint for this layout
   
   Returns the given node"
  (multiple-value-bind 
    (internal-wcons internal-hcons) (extract-constraint-hints n)
    (let ((wcons (intersect wcons internal-wcons))
          (hcons (intersect hcons internal-hcons))) 
      (print (list (min-val wcons) (max-val wcons) 
                   (min-val hcons) (max-val hcons)))
      (cond 
        ((typep n 'concrete-text-node)
         (layout-text n wcons hcons)
         )
        ((and (typep n 'simple-concrete-dom-node) (member (tag n) '(col row)))
         (layout-flex n wcons hcons)
         )
        (t (error "Unimpl layout for ~S" n)))
      (setf (x (pos (layout-annot n))) (float x))
      (setf (y (pos (layout-annot n))) (float y))))
  n)
