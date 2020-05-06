(in-package #:browse-cl)

(defun make-ortho (le to ri bo ne fa)
  (make-array '(16) 
              :initial-contents 
              (list 
                (/ 2.0 (- ri le))           0.0                         0.0                         0.0
                0.0                         (/ 2.0 (- to bo))           0.0                         0.0
                0.0                         0.0                         (/ -2.0 (- fa ne))          0.0 
                (- (/ (+ ri le) (- ri le))) (- (/ (+ to bo) (- to bo))) (- (/ (+ fa ne) (- fa ne))) 1.0)
              :element-type 'single-float))

(defparameter *screen-size* (vec2 800.0 600.0))
(defparameter *proj-mat* (make-ortho 0.0 0.0 800.0 600.0 -1000.0 1000.0))
(defparameter *blend-params* (make-blending-params))

(defparameter *site-origin* "127.0.0.1")
(defparameter *site-port* "9898")

(defparameter *painter* nil)
(defparameter *root* nil)
(defparameter *env* nil)
(defparameter *root-concrete* nil)
(defparameter *atlas-manager* nil)
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)

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
  (setf *proj-mat* (make-ortho 0.0 0.0 (float w) (float h) -1000.0 1000.0)))

(defun update-root-concrete (&key (force nil) (with-redraw t))
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
              (let ((to-remove (list))
                    (first-ix nil)) 
                (loop for ii below (length (children parent))
                      when (member (nth ii (children parent)) 
                                   (related-concrete-nodes x))
                      do (when (not first-ix) (setf first-ix ii)) (push ii to-remove))
                (let* ((old-node-state (when (= 1 (length (related-concrete-nodes x)))
                                         (state (car (related-concrete-nodes x)))))
                       (new-nodes (expand-template-dom-node *env* x))) 
                  ;; Special case when num children == 1, preserve the state of
                  ;; the old node
                  (when (and (= 1 (length new-nodes)) old-node-state)
                    (setf (state (nth 0 new-nodes)) old-node-state))
                  (setf (children parent) 
                       (loop for ii from 0 for x in (children parent)
                             if (not (member ii to-remove)) collect x
                             else do 
                             ;; Unload textures
                             (when (and (typep (render-annot x) 'text-render-annot) 
                                        (cached-tex-name (render-annot x))) 
                               (unload-tex *atlas-manager* (cached-tex-name (render-annot x))))))

                 ;; Add children back
                 (assert first-ix)
                 (if (not (children parent))
                     (setf (children parent) new-nodes)
                     (if (= 0 first-ix) 
                         (setf (children parent) 
                               (concatenate 'list new-nodes (children parent)))
                         (let ((old-cdr (cdr (if (= 0 first-ix) (children parent) 
                                                 (nthcdr (- first-ix 1) (children parent))))))
                           (setf (cdr (nthcdr (- first-ix 1) (children parent)))
                                 (concatenate 'list new-nodes
                                              old-cdr))))))))))
  (clear-dirty-globals *env*)
  (layout *root-concrete*)
  (when with-redraw 
    (clear-painter *painter*)
    (render-dom *painter* *root-concrete* 0.0 0.0 :is-debug nil)
    (flush *painter*)))

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

;; Tracks the current text input
(defparameter *text-input-buffer* 
  (make-array 0 
              :fill-pointer 0 
              :adjustable t 
              :initial-contents "" 
              :element-type 'character))
(defparameter *curr-focused-elem* nil 
  "Set to a persist-id of the currently focused node")

(defun make-str-buf (str)
  (make-array (length str) 
              :fill-pointer (length str) 
              :adjustable t 
              :initial-contents str
              :element-type 'character))

(defun focus-elem (e)
  "Focus the given dom node"
  (setf *curr-focused-elem* (persist-id (state e)))
  (set-internal-dnsv (focused (state e)) t)
  (setf *text-input-buffer* (make-str-buf (val (text (state e)))))
  (sdl2:start-text-input))
(defun unfocus-elem (e)
  "Un-focus the given dom node"
  (when (and *curr-focused-elem*
             (or
               (eq (persist-id (state e)) *curr-focused-elem*)
               (not (find-with-persist-id *root-concrete* *curr-focused-elem*)))) 
    (setf *curr-focused-elem* nil)
    (sdl2:stop-text-input))
  (set-internal-dnsv (focused (state e)) nil))
(defun update-focused-elem-text ()
  (when (not *curr-focused-elem*) (return-from update-focused-elem-text))
  (let ((e (find-with-persist-id *root-concrete* *curr-focused-elem*))) 
    (when (not e) (setf *curr-focused-elem* nil) (return-from update-focused-elem-text))
    (when (and (typep e 'simple-concrete-dom-node) (eq 'text-input (tag e)))
      (set-internal-dnsv (text (state e)) *text-input-buffer*))))

(defun process-on-click (env dom)
  "Call on-click events on dom nodes

   dom - a concrete DOM"
  (let ((x *mouse-x*) (y *mouse-y*))
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
                      (progn (let ((on-click-fn (find-attr d "ON-CLICK")))
                               (when on-click-fn (funcall (val (val on-click-fn)) env nil)))
                             ;; Kinda messy... we really want an 'input supertype' here,
                             ;; but for now we only care about text input
                             (when (and (typep d 'simple-concrete-dom-node) (eq 'text-input (tag d)))
                               (sync-bindings *env* d)
                               (focus-elem d)))
                      ;; Unfocus text input if not clicked on
                      (when (and (typep d 'simple-concrete-dom-node) 
                                 (eq 'text-input (tag d))
                                 (val (focused (state d))))
                        (unfocus-elem d)))
                  (pos la))) (vec2 0.0 0.0))))

(defun process-dom-hover (dom)
  (let ((x *mouse-x*) (y *mouse-y*))
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
                       (set-internal-dnsv (hover (state d)) nil))
                   (pos la))) (vec2 0.0 0.0))))

(defun process-dom-scroll (dom x-scroll y-scroll)
  (when (not (= x-scroll 0)) (error "Unimplemented x scrolling"))
  (let ((x *mouse-x*) (y *mouse-y*))
    (walk-expr dom
               (lambda (d parent-pos)
                 (assert (layout-annot d))
                 
                 (let* ((la (layout-annot d))
                        (dx (+ (x parent-pos) (x (pos la))))
                        (dy (+ (y parent-pos) (y (pos la))))
                        (dw (x (size la)))
                        (dh (y (size la))))
                   (when (and 
                           (typep d 'simple-concrete-dom-node)
                           (eq 'overflow (tag d))
                           (>= x dx) (<= x (+ dw dx))
                           (>= y dy) (<= y (+ dh dy)))
                     ;; Get the height of the child, vs dh, is the max
                     ;; scrolling we want. Clamp between 0 and (- child-height dh)
                     (let* ((child-height (y (size (layout-annot (nth 0 (children d))))))
                            (max-y-scroll (max 0 (- child-height dh)))
                            (new-scroll-val (+ y-scroll (val (scroll-y (state d)))))
                            (clamped (max 0 (min max-y-scroll new-scroll-val)))) 
                      (set-internal-dnsv (scroll-y (state d)) clamped)))
                   (pos la))) (vec2 0.0 0.0))))

(defun oninput (e)
  (cond
    ((eq :mousebuttonup (sdl2:get-event-type e))
     (let* ((x (plus-c:c-ref e sdl2-ffi:sdl-event :button :x))
            (y (plus-c:c-ref e sdl2-ffi:sdl-event :button :y)))
       (setf *mouse-x* x)
       (setf *mouse-y* y)
       (process-on-click *env* *root-concrete*)
       (sync-bindings *env* *root-concrete*)
       ;; TODO check if we need to update, rather than doing it regardless
       ;; ALso, try subtree updates
       (update-root-concrete)))
    ((eq :mousemotion (sdl2:get-event-type e))
     (let* ((x (plus-c:c-ref e sdl2-ffi:sdl-event :button :x))
            (y (plus-c:c-ref e sdl2-ffi:sdl-event :button :y)))
       (setf *mouse-x* x)
       (setf *mouse-y* y)
       ;(process-dom-hover *root-concrete*)
       ;(sync-bindings *env* *root-concrete*)
       ;;; TODO check if we need to update, rather than doing it regardless
       ;;; ALso, try subtree updates
       ;(update-root-concrete)
       
       ))
    ((eq :mousewheel (sdl2:get-event-type e))
     (let* ((y* (plus-c:c-ref e sdl2-ffi:sdl-event :wheel :y))
            (direction (plus-c:c-ref e sdl2-ffi:sdl-event :wheel :direction))
            ;; Calculate ACTUAL scroll direction based on 'flipped' bool
            (y (if (eq sdl2-ffi:+sdl-mousewheel-normal+ direction) (- y*) y*)))
       ;; TODO Process scrolling
       (process-dom-scroll *root-concrete* 0 (* 10 y))))
    ((eq :textinput (sdl2:get-event-type e))
     (let ((text (plus-c:c-ref e sdl2-ffi:sdl-event :text :text)))
       (vector-push-extend (code-char text) *text-input-buffer*)
       (update-focused-elem-text)
       (sync-bindings *env* *root-concrete*)
       (update-root-concrete)))
    ((eq :keydown (sdl2:get-event-type e))
         (let ((keysym (plus-c:c-ref e sdl2-ffi:sdl-event :key :keysym :sym)))
           (cond 
             ((and (eq sdl2-ffi:+sdlk-backspace+ keysym) 
                   (> (length *text-input-buffer*) 0))
              (vector-pop *text-input-buffer*)
              (update-focused-elem-text)
              (sync-bindings *env* *root-concrete*)
              (update-root-concrete))
             ;; Reload on f5
             ((eq sdl2-ffi:+sdlk-f5+ keysym) (reload-page)))))
    ((eq :windowevent (sdl2:get-event-type e))
     (let ((event (plus-c:c-ref e sdl2-ffi:sdl-event :window :event))
           (d1 (plus-c:c-ref e sdl2-ffi:sdl-event :window :data1))
           (d2 (plus-c:c-ref e sdl2-ffi:sdl-event :window :data2)))
       (cond
         ((eq event sdl2-ffi:+sdl-windowevent-resized+ )
          (onresize d1 d2)))
       ))))

(register-event-listener (lambda (e) (oninput e)))

(defmethod flush-and-render ((p painter))
  "Render the current painter state & clear the painter"
  (flush p)
  (with-blending *blend-params* (render-painter p #'prog-1))
  (clear-painter p))

(defun render-main ()
  (flush-and-render *painter*))

(defun reload-page ()
  "Reload the page with *site-port* and *site-origin*."

  (let* ((url (format nil "http://~a:~a/" *site-origin* *site-port*))
         (input-stream (make-string-input-stream 
                         (drakma:http-request url)))) 
    (format t "Reloading ~a~%" url)
    (setf *atlas-manager* (make-atlas-manager))
    (setf *painter* (make-painter *atlas-manager*))
    (multiple-value-bind (tree env) 
      (compile-browser-program 
        (loop with res do (setf res (read input-stream nil 'eof))
              until (eq res 'eof)
              collect res))
      (setf *root* tree)
      (setf *env* env))
    (walk-expr *root* (lambda (x val) (declare (ignore val)) (init-dependent-env-vals x))) 
    (update-root-concrete :force t :with-redraw nil)
    (set-dirty-globals *env* t)
    (sync-bindings *env* *root-concrete*)
    (update-root-concrete)))

(defun init ()
  (setf (clear-color cepl.context::*primary-context*) 
        (vec4 0.99 0.99 0.99 1.0))
  (cepl.sdl2-ttf:init-cepl-sdl2-ttf)
  (gl:enable :stencil-test)
  (gl:stencil-mask #xff)
  (gl:stencil-op :keep :keep :keep)
  (gl:stencil-func :equal #xff #xff)
  (gl:depth-func :lequal)
  
  (setf *atlas-manager* (make-atlas-manager))
  (setf *painter* (make-painter *atlas-manager*))

  ;; Setup test DOM
  (multiple-value-bind (tree env) 
    (compile-browser-program
      '((var my-text string "Hello")
        (var focused bool f)
        (defcomp load-bar (value int max-value int w int h int)
                 (view (row :bg-col #xaaaaaaff :w w :h h
                            (empty :bg-col #xff0000ff :h h :w 
                                   (* (/ (num value) (num max-value)) w)))))
        (col
          (text-input :bg-col (if focused #xeeeeeeee #xaaaaaaaa) 
                      :bind-state-focused focused 
                      :bind-state-text my-text)
          (load-bar (length my-text) 40 80 20)
          (text my-text))))
    (setf *root* tree)
    (setf *env* env))
  (walk-expr *root* (lambda (x val) (declare (ignore val)) (init-dependent-env-vals x)))
  (update-root-concrete :force t)
  (sync-bindings *env* *root-concrete*)
  (update-root-concrete))

(defun update ()
  (step-host) ;; Poll events
  (update-repl-link)
  (clear)
  (clear-painter *painter*)
  (render-dom *painter* *root-concrete* 0.0 0.0 :is-debug nil)
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

(defun open-window ()
  ;; HACK: Set stencil size to 8 here, cepl.sdl2 is broken
  (print (sdl2::sdl-init sdl2-ffi::+sdl-init-everything+))
  (sdl2:gl-set-attr :stencil-size 8)
  (cepl:repl))
