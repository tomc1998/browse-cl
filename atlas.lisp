(in-package #:browse-cl)

(defclass atlas ()
  ((bin :initarg :bin :accessor bin :type bin-pack-node)
   ;; The tex in CPU memory. This can be freed after flushing to the GPU - if
   ;; this is done, future flushes will flush an empty texture.
   (cpu-tex :initarg :cpu-tex :accessor cpu-tex :type c-array)
   ;; The backing GPU tex
   (tex :initarg :tex :accessor tex :type texture)
   ;; Width of atlas in pixels
   (width :initarg :width :accessor width :type integer)
   ;; Height of atlas in pixels
   (height :initarg :height :accessor height :type integer)))

(defmethod flush ((a atlas))
  "Flush the atlas's buffered CPU data to the GPU."
  (print "Flushing atlas")
  (cepl:copy-c-array-to-texture-backed-gpu-array (cpu-tex a) (texref (tex a))))

(defclass tex-rect ()
   ;; top left in uv coords
  ((top-left :initarg :top-left :accessor top-left :type vec2)
   ;; bottom right in UV coords
   (bottom-right :initarg :bottom-right :accessor bottom-right :type vec2)
   ;; Width of tex in pixels
   (width :initarg :width :accessor width :type integer)
   ;; Height of tex in pixels
   (height :initarg :height :accessor height :type integer)))

(defmethod dimensions ((tr tex-rect))
  (list (width tr) (height tr)))

(defmethod blit-c-array ((dst c-array) (src c-array) &optional (x 0) (y 0))
  "Blit a 2d src c-array onto the 2d dst c-array, at the given position."
  (let ((w (first (dimensions src)))
        (h (second (dimensions src)))) 
    (assert (eq (c-array-element-type src) (c-array-element-type dst)))
    (assert (= (length (dimensions dst)) (length (dimensions src)) 2))
    (assert (>= (first (dimensions dst)) (+ x w)))
    (assert (>= (second (dimensions dst)) (+ y h)))
    (loop for xx below w do
          (loop for yy below h do
                (setf (aref-c dst (+ xx x) (+ yy y)) (aref-c src xx yy))))))

(defparameter *num-anon-mem-tex* 0)
(defun gen-anon-mem-tex-name ()
  (incf *num-anon-mem-tex*)
  (format nil "mem-tex-~a" (- *num-anon-mem-tex* 1)))

(defmethod load-tex-from-c-array ((a atlas) img-data &optional (name (gen-anon-mem-tex-name)))
  "Given a c-array, load that texture data into the atlas. c-array element type
   should be a :uint8-vec4, each int representing a single rgba8888 pixel.

   NOTE: c-array must be 2 dimensional!
   NOTE: img-data will not be freed by this function, free it yourself!"
  (assert (= 2 (length (dimensions img-data))))
  (let* ((w (nth 0 (dimensions img-data)))
         (h (nth 1 (dimensions img-data)))
         (img-node (pack (bin a) name w h))
         (x (x-off img-node))
         (y (y-off img-node))
         (uvx (/ (+ 0.5 (float x)) (float (width a))))
         (uvy (/ (+ 0.5 (float y)) (float (height a))))
         (uvw (/ (- (float w) 1) (float (width a))))
         (uvh (/ (- (float h) 1) (float (height a)))))
    (blit-c-array (cpu-tex a) img-data x y)
    (make-instance 'tex-rect 
                   :top-left (vec2 uvx (+ uvy uvh))
                   :bottom-right (vec2 (+ uvx uvw) uvy)
                   :width w :height h)))

(defmethod load-tex ((a atlas) filename)
  "Load a texture to a, throws bin-packer-full if atlas full. Returns a
   tex-rect, valid for this atlas."
  (let ((data (dirt:load-image-to-c-array filename))) 
    (load-tex-from-c-array a data filename)
    ;; TODO unclear whether or not c-array is finalized,
    ;; seem to get mem errors with this line, check source
    ;;(free-c-array data)
    ))

(defmethod clear-atlas ((a atlas))
  (setf (bin a) (make-bin-packer (width a) (height a))))

(defun make-atlas (width height)
  (let* ((cpu-tex (make-c-array nil :dimensions (list width height) :element-type :uint8-vec4))
         (gpu-tex (make-texture nil :dimensions (list width height) :element-type :rgba8 
                                :pixel-format (cepl.pixel-formats::make-pixel-format 
                                                :components :rgba :type :uint8)))
         (a (make-instance 
             'atlas 
             :bin (make-bin-packer width height)
             :cpu-tex cpu-tex
             :tex gpu-tex
             :width width :height height)))
    (sb-ext:finalize a (lambda () 
                         ;; TODO unclear whether or not c-array is finalized,
                         ;; seem to get mem errors with this line, check source
                         ;;(free-c-array cpu-tex)
                         ;;(free-texture gpu-tex)
                         (free-c-array cpu-tex)))
    (setf (%cepl.types:c-array-element-pixel-format (cpu-tex a))
         (cepl.pixel-formats::make-pixel-format :components :rgba :type :uint8))
    a))
