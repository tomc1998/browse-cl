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

(defun int-to-col (val)
  "Return a vec4 containing the color, from a single #xffffffff int."
  (vec4 (/ (float (logand #xff (ash val -24))) 255.0)
        (/ (float (logand #xff (ash val -16))) 255.0)
        (/ (float (logand #xff (ash val  -8)))  255.0)
        (/ (float (logand #xff (ash val  -0)))  255.0)))

(defmethod get-font-col ((n concrete-dom-node))
  "Return a vec4 col"
  (let ((attr (find-attr n "FONT-COL")))
    (if attr
        (cond ((integerp (val (val attr))) (int-to-col (val (val attr))))
              (t (error "Unknown font col val ~S" (val (val attr)))))
        (vec4 0.0 0.0 0.0 1.0) ;; Black text by default
        )))

(defmethod render-dom-background ((p painter) (n concrete-dom-node) x y &key (depth 0) (clip-depth 0))
  "Will flush p & write into the stencil buffer if :clip t (regardless of
   whether bg is transparent). Returns t if written to stencil buffer, nil
  otherwise."
  (let* ((bg-col-attr (find-attr n "BG-COL"))
        (bg-col (when (and bg-col-attr (typep (val (val bg-col-attr)) 'integer))
                  (int-to-col (val (val bg-col-attr)))))
        (clip-attr (find-attr n "CLIP"))
        (clip (or (and (typep n 'simple-concrete-dom-node) (eq 'text-input (tag n))) 
                  (when clip-attr (eq t (val (val clip-attr)))))))
    (when (or clip bg-col) 
      (when clip ;; Setup stencil
        ;;(print-stencil-bits)
        (flush-and-render p)
        (gl:stencil-func :always clip-depth #xff)
        (gl:stencil-op :incr :incr :incr)
        (when (not bg-col) (%gl:color-mask 0 0 0 0))) ;; Disable colour buffer if no bg col
      (fill-rect p (vec3 x y (float depth)) (size (layout-annot n)) (if bg-col bg-col (vec4 0.0 0.0 0.0 0.0)))
      (when clip ;; Reset stencil
        (flush-and-render p)
        (gl:stencil-op :keep :keep :keep)
        (gl:stencil-func :less clip-depth #xff)
        (when (not bg-col) (%gl:color-mask 1 1 1 1))))
    (if clip t nil)))

(defmethod remove-stencil ((p painter) (n concrete-dom-node) x y &key (depth 0) (clip-depth 0))
  "If n is clipped, remove the stencil by decrementing the stencil value &
   rendering over the background"
 (let* ((clip-attr (find-attr n "CLIP"))
        (clip (or (and (typep n 'simple-concrete-dom-node) (eq 'text-input (tag n))) 
                  (when clip-attr (eq t (val (val clip-attr)))))))
    (when clip
      ;; Setup stencil
      (flush-and-render p)
      (gl:stencil-func :always clip-depth #xff)
      (gl:stencil-op :decr :decr :decr)
      (%gl:color-mask 0 0 0 0) ;; Disable colour buffer for this rect
      (fill-rect p (vec3 x y (float depth)) (size (layout-annot n)) (vec4 0.0 0.0 0.0 0.0))
      ;; Set stencil back to read-only
      (flush-and-render p)
      (gl:stencil-op :keep :keep :keep)
      (gl:stencil-func :lequal clip-depth #xff)
      (%gl:color-mask 1 1 1 1))
    (if clip t nil)) 
 )

(defmethod render-dom ((p painter) (n simple-concrete-dom-node) x y &key (depth 0) (is-debug nil) (clip-depth 0))
  "is-debug - when true, renders coloured boxes around all nodes"
  (assert (layout-annot n))
  (let ((x (float (+ x (x (pos (layout-annot n))))))
        (y (float (+ y (y (pos (layout-annot n))))))
        (debug-col (vec4 (* depth 0.2) (* depth 0.2) (* depth 0.2) 1.0)))
    (when is-debug
      (fill-rect p (vec3 x y (float depth)) (size (layout-annot n)) debug-col))
    (when (render-dom-background p n x y :depth depth :clip-depth clip-depth) (incf clip-depth))
    (loop for c in (children n) do 
          (render-dom p c x (- y (val (scroll-y (state n)))) 
                      :depth (+ 1 depth) :is-debug is-debug :clip-depth clip-depth))
    (when (eq 'text-input (tag n))
      ;; Render text
      (let ((tra (render-annot n)))
        (if tra 
            (assert (typep tra 'text-render-annot))
            (progn 
              (setf (render-annot n)
                    (make-instance 
                      'text-render-annot
                      :cached-tex-name 
                      (when (> (length (val (text (state n)))) 0) 
                        (render-wrapped-text-to-atlas-manager 
                          (atlas-manager p) 
                          (find-font-name-for-text-node n)
                          (find-font-size-for-text-node n)
                          (val (text (state n)))
                          (floor (x (size (layout-annot n))))
                          255 255 255))))
              (flush (atlas-manager p))))
        (let ((font-col (get-font-col n))) 
          (when (cached-tex-name (render-annot n))
            (fill-tex p (cached-tex-name (render-annot n))
                      (vec3 x y (+ 1.0 (float depth)))
                      :col font-col)))))
    (remove-stencil p n x y :depth depth :clip-depth (- clip-depth 1))))

(defmethod render-dom ((p painter) (n concrete-text-node) x y &key (depth 0) (is-debug nil) (clip-depth 0))
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
    (when (render-dom-background p n x y :depth depth :clip-depth clip-depth) (incf clip-depth))
    (when (not tra)
      ;; Render text, store in cached-tex-name
      (setf (render-annot n) 
            (make-instance 
              'text-render-annot
              :cached-tex-name 
              (when (> (length (val n)) 0) 
                (render-wrapped-text-to-atlas-manager 
                  (atlas-manager p) 
                  (find-font-name-for-text-node n)
                  (find-font-size-for-text-node n)
                  (val n)
                  (floor (x (size (layout-annot n))))
                  255 255 255))))
      (flush (atlas-manager p)))
    (let ((font-col (get-font-col n))) 
      (when (cached-tex-name (render-annot n))
        (fill-tex p (cached-tex-name (render-annot n))
                  (vec3 x y (+ 1.0 (float depth)))
                  :col font-col)))
    (remove-stencil p n x y :depth depth :clip-depth (- clip-depth 1))))
