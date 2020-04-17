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

(defun constraint (&optional (min-val 0.0) (max-val +pos-inf+))
  (make-instance 'constraint 
                 :min-val (float min-val) 
                 :max-val (float max-val)))

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
         (min-w (if a-min-w (val (val a-min-w)) 0.0))
         (max-w (if a-max-w (val (val a-max-w)) +pos-inf+))
         (w (find-attr n "W"))
         (wcons (if w (constraint (val (val w)) (val (val w))) 
                    (constraint min-w max-w)))

         (a-min-h (find-attr n "MIN-H"))
         (a-max-h (find-attr n "MAX-H"))
         (min-h (if a-min-h (val (val a-min-h)) 0.0))
         (max-h (if a-max-h (val (val a-max-h)) +pos-inf+))
         (h (find-attr n "H"))
         (hcons (if h (constraint (val (val h)) (val (val h))) 
                    (constraint min-h max-h))))
    (values wcons hcons)))

(defmethod get-layout-weight ((n concrete-dom-node))
  "Find the value of the :weight attr
   Returns 0 if weight isn't specified in attrs"
  (let ((attr (find-attr n "WEIGHT")))
    (if attr (float (val (val attr))) 0.0)))

(defmethod layout-flex ((n concrete-dom-node) wcons hcons)
  "Given a node which MUST be a col or row, layout a col/row with the given
   wcons and hcons. wcons and hcons should be fully pre-calculated."
  (assert (member (tag n) '(col row)))
  (let*
    ((is-col (eq (tag n) 'col))
     (space-left (if is-col (max-val hcons) (max-val wcons)))
     ;; Cross-axis constraint
     (child-cross-cons (if is-col (constraint 0 (max-val wcons)) 
                           (constraint 0 (max-val hcons))))
     ;; Total weight of all components, for distributing 'slack' weight
     (total-weight (loop for c in (children n) sum (get-layout-weight c)))
     (weighted-spaces (make-array (list (length (children n))) 
                                  :initial-element -1.0 
                                  :element-type 'single-float)))
    (labels 
      ((get-main-axis-max-constraint-hint (n) 
         (multiple-value-bind (w h) (extract-constraint-hints n)
           (if is-col h w)))
       (set-main-axis-pos (n pos)
         (if is-col (setf (y (pos (layout-annot n))) pos)
             (setf (x (pos (layout-annot n))) pos)))
       (get-main-axis-size (n) 
         (if is-col (y (size (layout-annot n))) (x (size (layout-annot n)))))
       (get-cross-axis-size (n) 
         (if is-col (x (size (layout-annot n))) (y (size (layout-annot n)))))
       (calculate-final-size ()
         "Return (vec2 w h), also applying our given wcons / hcons"
         (let*
           ((main (loop for c in (children n) do
                        (assert (layout-annot c))
                        sum (get-main-axis-size c)))
            (cross (loop for c in (children n) do
                         (assert (layout-annot c))
                         maximizing (get-cross-axis-size c)))
            (wh (if is-col (vec2 cross main) (vec2 main cross))))
           (vec2 (max (min-val wcons) (x wh))
                 (max (min-val hcons) (y wh))))))
      ;; Layout non-weighted children
      (loop for c in (children n)
            when (= (get-layout-weight c) 0) do
            (if is-col 
              (layout c 0 0 child-cross-cons (constraint 0 space-left))
              (layout c 0 0 (constraint 0 space-left) child-cross-cons))
            (assert (layout-annot c))
            (decf space-left (if is-col (y (size (layout-annot c))) 
                                 (x (size (layout-annot c)))))
            (when (< space-left 0)
              (error 'layout-overflow-error :n n 
                     :wcons wcons :hcons hcons)))
      (loop named outer with free-space = 0 and no-space-consumed do
            (setf no-space-consumed t)
            ;; Using space-left, figure out how much space to allocate per weighted
            ;; component.
            (loop for c in (children n) for ii below (length weighted-spaces) 
              with weight do (setf weight (get-layout-weight c))
              when (and (/= -2.0 (aref weighted-spaces ii)) (> weight 0)) do
              (setf no-space-consumed nil)
              (setf (aref weighted-spaces ii) 
                    (* space-left (/ (get-layout-weight c) total-weight)))
              ;; Redistribute any space due to elements having a max-w/max-h constraint
              ;; less than their allocated space
              ;; Set the weighted-spaces value to -2, so we know for next loop
              ;; that we don't need to redistribute any more weighted space to
              ;; this node
              (when (< (max-val (get-main-axis-max-constraint-hint c)) (aref weighted-spaces ii))
                (incf free-space (- (aref weighted-spaces ii) 
                                    (max-val (get-main-axis-max-constraint-hint c))))
                (setf (aref weighted-spaces ii) -2.0)))
            ;; If we haven't redistributed any more space, return.
            (when (or no-space-consumed (= 0.0 free-space)) (return-from outer)))
      ;; Re-layout everything with the additional weighted spaces
      (loop for c in (children n) 
            for ws across weighted-spaces 
            with curr-pos = 0.0

            ;; If weight = 0, we've already layed this out, so we can just skip
            ;; it & assign its position later
            if (= 0.0 (get-layout-weight c)) do
              (set-main-axis-pos c curr-pos)
            else if (= ws -2.0) do ;; Weighted component which has been trimmed
              (if is-col         ;; to its max size, no need to constrain here 
                  (layout c 0.0 curr-pos child-cross-cons (constraint))
                  (layout c curr-pos 0.0 (constraint) child-cross-cons))
            else do ;; Weighted component which is stretched to the size in ws
              (assert (>= ws 0.0))
              (if is-col
                  (layout c 0.0 curr-pos child-cross-cons (constraint ws ws))
                  (layout c curr-pos 0.0 (constraint ws ws) child-cross-cons))
            end

            do
            (assert (layout-annot c))
            (incf curr-pos (get-main-axis-size c)))
      ;; Set this node's layout annot
      (setf (layout-annot n) 
            (make-instance 'layout-annot :size (calculate-final-size))))))

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
      (inspect n)
      (error 'layout-overflow-error :n n :wcons wcons :hcons hcons))
    (setf (layout-annot n) 
          (make-instance 'layout-annot 
                         :size (vec2 (float (max (min-val wcons) (nth 0 text-size)))
                                     (float (max (min-val hcons) (nth 1 text-size))))))))

(defmethod layout-overflow ((n simple-concrete-dom-node) wcons hcons)
  (assert (= 1 (length (children n))))
  (let ((ox (find-attr n "X"))
        (oy (find-attr n "Y")))
    (layout (first (children n)) 0.0 0.0 
            (if (and ox (eq t (val (val ox)))) (constraint) (constraint 0 (max-val wcons))) 
            (if (and oy (eq t (val (val oy)))) (constraint) (constraint 0 (max-val hcons)))))
  (setf (layout-annot n) 
        (make-instance 'layout-annot
                       :size (vec2 (float (min-val wcons)) 
                                   (float (min-val hcons))))))

(defmethod layout 
  ((n concrete-dom-node) 
   &optional 
   (x 0.0) (y 0.0)
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
      (cond 
        ((typep n 'concrete-text-node)
         (layout-text n wcons hcons)
         )
        ((and (typep n 'simple-concrete-dom-node) (member (tag n) '(col row)))
         (layout-flex n wcons hcons))
        ((and (typep n 'simple-concrete-dom-node) (member (tag n) '(overflow)))
         (layout-overflow n wcons hcons))
        ((and (typep n 'simple-concrete-dom-node) (eq (tag n) 'empty))
         (setf (layout-annot n) 
               (make-instance 'layout-annot :size (vec2 (min-val wcons) 
                                                        (min-val hcons)))))
        (t (error "Unimpl layout for ~S" n)))
      (setf (x (pos (layout-annot n))) (float x))
      (setf (y (pos (layout-annot n))) (float y))))
  n)
