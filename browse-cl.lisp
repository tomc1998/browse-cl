(in-package #:browse-cl)

(defparameter *proj-mat* (rtg-math.projection:orthographic-v2 
                           (vec2 800.0 600.0) -1000.0 1000.0))
(defparameter *blend-params* (make-blending-params))

(defparameter *painter* nil)
(defparameter *root* nil)
(defparameter *root-concrete* nil)
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
                           (vec2 (float w) (float h)) -1000.0 1000.0)))

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
    (render-painter *painter* #'prog-1)))

(defun init ()
  (setf (clear-color cepl.context::*primary-context*) 
        (vec4 0.1 0.1 0.3 1.0))
  (cepl.sdl2-ttf:init-cepl-sdl2-ttf)
  (setf *atlas-manager* (make-atlas-manager))
  (setf *painter* (make-painter *atlas-manager*))

  ;; Setup test DOM
  (setf *root* 
        (parse-dom-node (create-global-scope) 
                          '(row 
                             (empty :w 100 :h 100) 
                             (empty :w 50 :h 50) 
                             (col :max-h 200 
                              (text :max-w 48 :max-h 48 "Hello " "World") 
                              (empty :w 32 :weight 1) 
                              (text :font-size 36 "My name is tom")))))
  (setf *root-concrete* (expand-template-dom-node (make-instance 'env) *root*))
  (layout *root-concrete*))

(defun update ()
  (step-host) ;; Poll events
  (update-repl-link)
  (clear-painter *painter*)
  (render-dom *painter* *root-concrete* 0.0 0.0 :is-debug t)
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
