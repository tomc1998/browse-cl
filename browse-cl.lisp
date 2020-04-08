(in-package #:browse-cl)

(defparameter *proj-mat* (rtg-math.projection:orthographic-v2 
                           (vec2 800.0 600.0) -1.0 1.0))
(defparameter *test-tex* nil)
(defparameter *blend-params* (make-blending-params))

(defparameter *painter* nil)
(defparameter *atlas-manager* nil)

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

(defun render-main ()
  (clear)
  (with-blending *blend-params*
    (render *painter* #'prog-1)))

(defun init ()
  (setf (clear-color cepl.context::*primary-context*) 
        (vec4 0.1 0.1 0.3 1.0))
  (cepl.sdl2-ttf:init-cepl-sdl2-ttf)
  (setf *atlas-manager* (make-instance 'atlas-manager))
  (setf *test-tex* (render-wrapped-text-to-atlas-manager 
                     *atlas-manager* 
                     "IBMPlexSans-Regular.otf" 24 "Hello, world! My name is Tom Cheng." 200)) 
  (setf *painter* (make-painter *atlas-manager*)))

(defun update ()
  (step-host) ;; Poll events
  (update-repl-link)
  (flush *painter*)
  (render-main)
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
