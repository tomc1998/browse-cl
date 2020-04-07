;;;; browse-cl.lisp

(in-package #:browse-cl)

(defparameter *proj-mat* (rtg-math.projection:orthographic-v2 
                           (vec2 800.0 600.0) -1.0 1.0))
(defparameter *test-tex* nil)
(defparameter *test-tex-sample* nil)
(defparameter *blend-params* (make-blending-params))

(defclass render-buf ()
  ((g-buf :initform (error "g-buf required") :initarg :g-buf :accessor g-buf)
   (g-stream :initform (error "g-buf required") :initarg :g-stream :accessor g-stream))
  (:documentation "A combination of a vertex buffer and accompanying stream."))

(defparameter *render-buf-list* (list))

;; gpu version of 'vert'
(defstruct-g vert
  (pos :vec3 :accessor pos)
  (uv :vec2 :accessor uv)
  (col :vec4 :accessor col))

(defun-g tri-vert ((vert vert))
  (values (* *proj-mat* (v! (pos vert) 1.0))
          (:smooth (uv vert))
          (:smooth (col vert))))

(defun-g tri-frag ((uv :vec2) (col :vec4) &uniform (tex :sampler-2d)) 
  (* col (texture tex uv)))

(defpipeline-g prog-1 ()
  (tri-vert vert)
  (tri-frag :vec2 :vec4))

(defun onresize (w h)
  (setf (cepl:resolution (cepl:current-viewport)) 
        (vec2 (float w) (float h)))
  (setf *proj-mat* (rtg-math.projection:orthographic-v2 
                           (vec2 (float w) (float h)) -1.0 1.0)))

(defun oninput (e)
  (cond
    ((eq :windowevent (sdl2:get-event-type e))
     (let ((event (plus-c:c-ref e sdl2-ffi:sdl-event :window :event))
           (d1 (plus-c:c-ref e sdl2-ffi:sdl-event :window :data1))
           (d2 (plus-c:c-ref e sdl2-ffi:sdl-event :window :data2)))
       (cond
         ((eq event sdl2-ffi:+sdl-windowevent-resized+ )
          (onresize d1 d2)))
       ))))

(register-event-listener (lambda (e) (oninput e)))

(defun render ()
  (with-blending *blend-params*
    (loop for b in *render-buf-list* do
        (map-g #'prog-1 (g-stream b) :tex *test-tex-sample*))))

(defun wrap-text (font text width)
  "Returns a list of lines"
  (let ((lines (list))
        (words (cl-ppcre:split "\\s" text))
        (curr-line ""))
    (loop for w in words do
          (let* ((new-line (if (> (length curr-line) 0) 
                               (concatenate 'string curr-line " " w)
                               w))
                 (line-width (sdl2-ttf:size-text font new-line)))
            (if (> line-width width)
                (progn
                  (push curr-line lines)
                  (setf curr-line w))
                (setf curr-line new-line))))
    (when (> (length curr-line) 0) (push curr-line lines))
    (reverse lines)))

(defun render-wrapped-text (font text width &optional (r 255) (g 255) (b 255))
  "Render some wrapped text
   
   * font - the font to use
   * text - the text to insert into the texture
   * width - the width available to layout the text
   * r, g, b - the colour to render the text"

  (if (= 0 (length text)) (error "Text is empty"))

  ;; Wrap text into lines, find the line height, use that to calculate the
  ;; final texture height
  (let* ((lines (wrap-text font text width))
         (line-height (multiple-value-bind (w h)
                        (sdl2-ttf:size-text font (car lines))
                        (declare (ignore w)) 
                        h))
         (height (* line-height (length lines)))
         ;; Allocate a surface to store the final texture
         (surface (sdl2:create-rgb-surface 
                    width height 32 
                    :r-mask #xff000000 :g-mask #x00ff0000 
                    :b-mask #x0000ff00 :a-mask #x000000ff))
         ;; Render all lines of text to a separate surface
         (line-surfaces (loop for l in lines collect (sdl2-ttf:render-utf8-blended font l r g b 0)))
         ;; Hold our return value
         (ret nil))
    ;; Blit all line-surfaces onto surface
    (loop for ii from 0 for ls in line-surfaces do
          (sdl2:blit-surface ls (sdl2:make-rect 0 0 width line-height)
                             surface (sdl2:make-rect 0 (* ii line-height) width line-height)))
    ;; Convert surface to a tex
    (setf ret (make-texture 
                (make-c-array-from-pointer 
                  (list width height) 
                  :uint8-vec4 (sdl2:surface-pixels surface))))
    ;; Free all surfaces
    (sdl2:free-surface surface)
    (loop for ls in line-surfaces do (sdl2:free-surface ls))
    ret))

(defun make-quad-rect (x y w h r g b a)
  (list 
    (list (v!    x       y    0.0) (v! 0.0 1.0) (v! r g b a))
    (list (v! (+ x w) (+ y h) 0.0) (v! 1.0 0.0) (v! r g b a))
    (list (v!    x    (+ y h) 0.0) (v! 0.0 0.0) (v! r g b a))
    (list (v!    x       y    0.0) (v! 0.0 1.0) (v! r g b a))
    (list (v! (+ x w)    y    0.0) (v! 1.0 1.0) (v! r g b a))
    (list (v! (+ x w) (+ y h) 0.0) (v! 1.0 0.0) (v! r g b a))) )

(defun init ()
  (setf (clear-color cepl.context::*primary-context*) 
        (vec4 0.1 0.1 0.3 1.0))
  (cepl.sdl2-ttf:init-cepl-sdl2-ttf)
  (setf *test-tex* (cepl.sdl2-ttf:with-font (font "IBMPlexSans-Regular.otf" 18) 
                     (render-wrapped-text font "Hello, world! My name is Tom Cheng." 200)))
  (setf *test-tex-sample* (sample *test-tex*))
  (setf *render-buf-list* (list))
  (let ((buf (make-gpu-array
               (make-quad-rect 0.0 0.0 (first (dimensions *test-tex*)) (second (dimensions *test-tex*)) 1.0 1.0 1.0 1.0)
               :element-type 'vert)))
    (push 
      (make-instance 'render-buf :g-buf buf :g-stream (make-buffer-stream buf)) 
      *render-buf-list*)))

(defun update ()
  (step-host) ;; Poll events
  (update-repl-link)
  (clear)
  (render)
  (swap))

(let ((running nil))
  (defun main ()
    (setf running t)
    (init)
    (loop while running do
          (livesupport:continuable 
            (update))))
  (defun stop-main ()
    (setf running nil)))
