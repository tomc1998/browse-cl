(in-package #:browse-cl)

(defclass render-annot () ()
  (:documentation "Attached to nodes after / during rendering, to track some
                   render state between state changes, e.g. cached textures"))

(defclass text-render-annot ()
  ((cached-tex-name :initarg :cached-tex-name 
                    :accessor cached-tex-name :type 'string
                    :documentation "The texture name in the atlas manager of
                                    the painter"))
  (:documentation "Attached to concrete-text-nodes"))

(defgeneric render-dom (p n x y &key depth is-debug))

(defmethod render-dom ((p painter) (n simple-concrete-dom-node) x y &key (depth 0) (is-debug nil))
  "is-debug - when true, renders coloured boxes around all nodes"
  (assert (layout-annot n))
  (let ((x (float (+ x (x (pos (layout-annot n))))))
        (y (float (+ y (y (pos (layout-annot n))))))
        (debug-col (vec4 (* depth 0.2) (* depth 0.2) (* depth 0.2) 1.0)))
    (when is-debug
      (fill-rect p (vec3 x y (float depth)) (size (layout-annot n)) debug-col))
    (loop for c in (children n) do (render-dom p c x y :depth (+ 1 depth) :is-debug is-debug))))

(defmethod render-dom ((p painter) (n concrete-text-node) x y &key (depth 0) (is-debug nil))
  "is-debug - when true, renders coloured boxes around all nodes"
  (assert (layout-annot n))
  (let ((x (float (+ x (x (pos (layout-annot n))))))
        (y (float (+ y (y (pos (layout-annot n))))))
        (debug-col (vec4 (* depth 0.2) (* depth 0.2) (* depth 0.2) 1.0))
        (tra (render-annot n)))
    (when tra (assert (typep tra 'text-render-annot)))
    (when is-debug
      (fill-rect p (vec3 x y (float depth)) 
                 (size (layout-annot n)) debug-col))
    (when (not tra)
      ;; Render text, store in cached-tex-name
      (setf (render-annot n) 
            (make-instance 
              'text-render-annot
              :cached-tex-name 
              (render-wrapped-text-to-atlas-manager 
                (atlas-manager p) 
                (find-font-name-for-text-node n)
                (find-font-size-for-text-node n)
                (val n)
                (floor (x (size (layout-annot n))))
                255 255 255)))
      (flush (atlas-manager p)))
    (fill-tex p (cached-tex-name (render-annot n))
              (vec3 x y (+ 1.0 (float depth))))
    ))
