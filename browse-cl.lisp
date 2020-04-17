(in-package #:browse-cl)

(defparameter *screen-size* (vec2 800.0 600.0))
(defparameter *proj-mat* (rtg-math.projection:orthographic-v2 
                           (vec2 800.0 600.0) -1000.0 1000.0))
(defparameter *blend-params* (make-blending-params))

(defparameter *painter* nil)
(defparameter *root* nil)
(defparameter *env* nil)
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
  (setf *screen-size* (vec2 (float w) (float h)))
  (setf *proj-mat* (rtg-math.projection:orthographic-v2 
                           (vec2 (float w) (float h)) -1000.0 1000.0)))

(defun update-root-concrete (&key (force nil))
  "Update the root

   force - If true, this will update the whole root, rather than finding
   subtrees which need updating due to state changes"
  (if force
      (setf *root-concrete* (car (expand-template-dom-node *env* *root*)))
      (loop for x in (find-nodes-which-need-expanding *env* *root*) do
            ;; First, find this node's parent
            (let ((parent nil))
              (walk-expr *root-concrete* 
                         (lambda (n parent*)
                           (when (member n (related-concrete-nodes x)) 
                             (progn (setf parent parent*) nil)) 
                           n) nil)
              ;; Then, set this child in the parent's children
              (assert parent)
              (assert (typep parent 'simple-concrete-dom-node))
              ;; Remove children associated with this template node
              ;; TODO Unload texture caches
              (let ((to-remove (list))
                    (first-ix nil)) 
                (loop for ii below (length (children parent))
                      when (member (nth ii (children parent)) 
                                   (related-concrete-nodes x))
                      do (when (not first-ix) (setf first-ix ii)) (push ii to-remove))
                (setf (children parent) 
                      (loop for ii from 0 for x in (children parent)
                            when (not (member ii to-remove)) collect x))

                ;; Add children back
                (assert first-ix)
                (if (not (children parent))
                    (setf (children parent) (expand-template-dom-node *env* x))
                    (if (= 0 first-ix) 
                        (setf (children parent) 
                              (concatenate 'list (expand-template-dom-node *env* x) (children parent)))
                        (let ((old-cdr (cdr (if (= 0 first-ix) (children parent) 
                                                (nthcdr (- first-ix 1) (children parent))))))
                          (setf (cdr (nthcdr (- first-ix 1) (children parent)))
                                (concatenate 'list (expand-template-dom-node *env* x)
                                             old-cdr)))))))))
  (clear-dirty-globals *env*)
  (layout *root-concrete*)
  (clear-painter *painter*)
  (render-dom *painter* *root-concrete* 0.0 0.0 :is-debug t)
  (flush *painter*))

(defun unload-all-cache-textures ()
  "Using the root DOM node, unload all cached textures from the atlas"
  (walk-expr *root-concrete* 
            (lambda (d v)
              (when (render-annot d)
                (when (typep (render-annot d) 'text-render-annot)
                  (unload-tex *atlas-manager* 
                              (cached-tex-name (render-annot d)))
                  (setf (render-annot d) nil)
                  )))))

(defun process-on-click (env dom x y)
  "Call on-click events on dom nodes

   dom - a concrete DOM"
  ;; Dumb hack here, we should correct our view mat to make 0,0 the top left
  (let ((x (- x (/ (x *screen-size*) 2.0)))
        (y (- (- y (/ (y *screen-size*) 2.0)))))
    (walk-expr dom
              (lambda (d parent-pos)
                (assert (layout-annot d))
                (let* ((la (layout-annot d))
                       (dx (+ (x parent-pos) (x (pos la))))
                       (dy (+ (y parent-pos) (y (pos la))))
                       (dw (x (size la)))
                       (dh (y (size la))))
                  (when (and (>= x dx) (<= x (+ dw dx))
                             (>= y dy) (<= y (+ dh dy)))
                    (let ((on-click-fn (find-attr d "ON-CLICK")))
                      (when on-click-fn  (funcall (val (val on-click-fn)) env nil))))
                  (pos la))) (vec2 0.0 0.0))))

(defun process-dom-hover (dom x y)
  (let ((x (- x (/ (x *screen-size*) 2.0)))
        (y (- (- y (/ (y *screen-size*) 2.0)))))
    (walk-expr dom
               (lambda (d parent-pos)
                 (assert (layout-annot d))
                 (let* ((la (layout-annot d))
                        (dx (+ (x parent-pos) (x (pos la))))
                        (dy (+ (y parent-pos) (y (pos la))))
                        (dw (x (size la)))
                        (dh (y (size la))))
                   (if (and (>= x dx) (<= x (+ dw dx))
                            (>= y dy) (<= y (+ dh dy)))
                       (set-internal-dnsv (hover (state d)) t)
                       (set-internal-dnsv (hover (state d)) nil)
                       )
                   (pos la))) (vec2 0.0 0.0))))

(defun oninput (e)
  (cond
    ((eq :mousebuttonup (sdl2:get-event-type e))
     (let* ((x (plus-c:c-ref e sdl2-ffi:sdl-event :button :x))
            (y (plus-c:c-ref e sdl2-ffi:sdl-event :button :y)))
       (process-on-click *env* *root-concrete* x y)
       (sync-bindings *env* *root-concrete*)
       ;; TODO check if we need to update, rather than doing it regardless
       ;; ALso, try subtree updates
       (update-root-concrete)))
    ((eq :mousemotion (sdl2:get-event-type e))
     (let* ((x (plus-c:c-ref e sdl2-ffi:sdl-event :button :x))
            (y (plus-c:c-ref e sdl2-ffi:sdl-event :button :y)))
       (process-dom-hover *root-concrete* x y)
       (sync-bindings *env* *root-concrete*)
       ;; TODO check if we need to update, rather than doing it regardless
       ;; ALso, try subtree updates
       (update-root-concrete)))
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
        (vec4 0.99 0.99 0.99 1.0))
  (cepl.sdl2-ttf:init-cepl-sdl2-ttf)
  (setf *atlas-manager* (make-atlas-manager))
  (setf *painter* (make-painter *atlas-manager*))

  ;; Setup test DOM
  (multiple-value-bind (tree env) 
    (compile-browser-program
      '((var my-font-size int 48)
        (var button-hovered bool f)
        (row 
          (empty :w (if button-hovered 120 100) 
                 :h (if button-hovered 120 100)
                 :bind-state-hover button-hovered
                 :on-click (fn (e mouse-event) void (set my-font-size (- my-font-size 1)))) 
          (col :max-h 400 
               (text :max-w 48 :max-h 48 "Hello " "World") 
               (text :font-col #xffffffff :bg-col #xff0000ff :font-size my-font-size "My name is tom")))))
    (setf *root* tree)
    (setf *env* env))
  (walk-expr *root* (lambda (x val) (declare (ignore val)) (init-dependent-env-vals x)))
  (update-root-concrete :force t))

(defun update ()
  (step-host) ;; Poll events
  (update-repl-link)
  (clear-painter *painter*)
  (render-dom *painter* *root-concrete* 0.0 0.0 :is-debug nil)
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
