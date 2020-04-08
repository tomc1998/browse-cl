(in-package #:browse-cl)

;; gpu version of 'vert'
(defstruct-g vert
  (pos :vec3 :accessor pos)
  (tex :vec2 :accessor tex)
  (col :vec4 :accessor col))

;; A wrapper around an adj-c-array of verts. Use make-painter to auto-free the
;; containing adj-c-array.
;; Use the draw-* and fill-* functions to buffer CPU-side verts
;; Use 'flush' to flush any CPU-side verts to the GPU buffer for rendering.
;; This will override the previous GPU buffer's content.
;; Use 'clear-painter' to clear the CPU-side verts
;; Use 'render' to render the current contents of this painter's GPU buffer.
(defclass painter ()
  ((buf :initarg :buf 
        :initform (error "Provide a backing adj-c-array for the painter")
        :accessor buf)
   (g-buf :initform (make-gpu-array nil :dimensions 0 :element-type 'vert)
          :accessor g-buf)
   (g-stream :initform nil :accessor g-stream)
   (atlas-manager :initarg :atlas-manager :accessor atlas-manager :type atlas-manager)
   (white-tex :initarg :white-tex :accessor white-tex :type 'tex-rect)))

(defun make-painter (atlas-manager &key (cap 1024))
  "Make a painter with an atlas-manager and optional vert buf capacity (defaults to 1024).
   The painter will buffer an additional 1x1 'white' texture, which will be
   used for drawing coloured shapes."
  (let* ((buf (make-adj-c-array 'vert :cap cap))
         (white-tex (load-tex-from-c-array 
                      atlas-manager 
                      (make-c-array (loop for x below 2 collect 
                                          (loop for y below 2 collect 
                                                (make-array '(4) :element-type '(unsigned-byte 8)
                                                            :initial-contents '(255 255 255 255)))) 
                                    :dimensions '(2 2) 
                                    :element-type :uint8-vec4)))
         (painter (make-instance 'painter 
                                 :atlas-manager atlas-manager :buf buf
                                 :white-tex white-tex)))
    (sb-ext:finalize painter (lambda () (free buf)))
    (flush (atlas-manager painter))
    painter))

(defmethod fill-tex ((p painter) tex-name pos &key size (col (vec4 1.0 1.0 1.0 1.0)))
  "Render a tex with the given name (must be present in the painter's atlas)"
  (let* ((tex (find-tex-rect (atlas-manager p) tex-name))
         (size (if size size 
                   (vec2 (float (width tex)) 
                         (float (height tex)))))
         (tl (top-left tex))
         (br (bottom-right tex))
         (tr (vec2 (x br) (y tl)))
         (bl (vec2 (x tl) (y br))))

   ;; Top left tri
   (push-back-ac (vector pos bl col) (buf p))
   (push-back-ac (vector (vec3 (+ (x pos) (x size)) 
                               (+ (y pos) (y size))  
                               (z pos)) tr col) (buf p))
   (push-back-ac (vector (vec3 (x pos)
                               (+ (y pos) (y size))
                               (z pos)) tl col) (buf p))

   ;; Lower right tri
   (push-back-ac (vector pos bl col) (buf p))
   (push-back-ac (vector (vec3 (+ (x pos) (x size)) 
                               (y pos) 
                               (z pos)) br col) (buf p))
   (push-back-ac (vector (vec3 (+ (x pos) (x size)) 
                               (+ (y pos) (y size))  
                               (z pos)) tr col) (buf p))))

(defmethod fill-rect ((p painter) pos size &optional (col (vec4 1.0 1.0 1.0 1.0)))
  "Fill a rectangle with top left at pos, with the given size and color."
  (fill-tex p (white-tex p) pos :size size :col col))

(defmethod flush ((p painter))
  (if (> (len (buf p)) 0)
    (setf (g-buf p) 
          (make-gpu-array (subseq-c (inner (buf p)) 0 (len (buf p))) 
                          :dimensions (len (buf p))
                          :element-type 'vert))
    (setf (g-buf p) 
          (make-gpu-array nil
                          :dimensions (len (buf p))
                          :element-type 'vert)))
  (setf (g-stream p) (make-buffer-stream (g-buf p) :length (len (buf p)))))

(defmethod clear-painter ((p painter)) (clear-ac (buf p)))

(defmethod render ((p painter) program)
  "Renders the painter with the painter's atlas texture sample bound to :tex."
  (map-g program (g-stream p) :tex (sample (tex (atlas (atlas-manager p))))))
