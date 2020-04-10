(in-package #:browse-cl)

(defclass cpu-atlas-data ()
  ((data :initarg :data :accessor data :type c-array 
         :documentation "The CPU-side data backing the atlas")
   (tex-rect :initarg :tex-rect :accessor tex-rect :type tex-rect
             :initform nil
             :documentation "The tex-rect of this texture in the atlas, or nil
                             if not yet packed")))

(defclass atlas-manager ()
  ((dirty :initform nil :accessor dirty :type boolean
          :documentation "If true, then calling flush on this atlas-manager
                          will cause a repack")
   (curr-res-id :initform 0 :accessor curr-res-id :type fixnum)
   (atlas :initform (make-atlas 256 256) :accessor atlas :type atlas)
   (resource-map :initarg :resource-map :accessor resource-map
                :documentation 
                "A map of texture names to the cpu-atlas-data. These can be
                 used to re-pack the atlas, and aren't unloaded from the CPU
                 once packed (since many of these textures will be dynamically generated, 
                  e.g. text).
                 Make sure to free this when atlas-manager is finalized."))
  (:documentation "A wrapper around atlas which tracks loaded resources, and
                   can unload & re-pack resources into the atlas."))

(defun make-atlas-manager ()
  (let* ((resource-map (make-hash-table :test #'equal))
         (res (make-instance 'atlas-manager :resource-map resource-map)))
    (sb-ext:finalize 
      res (lambda () 
            (maphash (lambda (k v) 
                       (declare (ignore k)) 
                       ;; TODO unclear whether or not c-array is finalized,
                       ;; seem to get mem errors with this line, check source
                       ;;(free-c-array (data v))
                       ) 
                     resource-map)))
    res))

(define-condition am-not-found-error (error)
  ((name :initarg :name :reader name)))
(define-condition am-not-packed-error (error)
  ((name :initarg :name :reader name)))

(defmethod find-tex-rect ((a atlas-manager) name)
  "Errors am-not-found-error if tex with name is not found
   Errors am-not-packed-error if tex is found but not packed
   Returns the tex-rect associated with the given name otherwise"
  (let ((res (gethash name (resource-map a))))
    (when (not res) (error 'am-not-found-error :name name))
    (when (not (tex-rect res)) (error 'am-not-packed-error :name name))
    (tex-rect res)))

(defmethod unload-tex ((a atlas-manager) name)
  "Errors am-not-found-error if texture not found"
  (let ((res (gethash name (resource-map a))))
    (when (not res) (error 'am-not-found-error :name name))
    (setf (gethash name (resource-map a)) nil)))

(defmethod gen-new-am-res-name ((a atlas-manager))
  (format nil "RES-ANON-~a" (incf (curr-res-id a))))

(defmethod load-tex-from-c-array 
  ((a atlas-manager) img-data &optional (name (gen-new-am-res-name a)))
  "Takes ownership of img-data"
  (setf (dirty a) t)
  (setf (gethash name (resource-map a)) 
        (make-instance 'cpu-atlas-data :data img-data))
  name)

(defmethod flush ((a atlas-manager))
  "Repacks everything in the atlas-manager, as long as some texture has been added"
  (when (not (dirty a)) (return-from flush))
  ;; TODO don't re-make atlas here, just clear it
  (setf (atlas a) (make-atlas 256 256))
  (maphash (lambda (k v)
             (setf (tex-rect v) 
                   (load-tex-from-c-array (atlas a) (data v) k))) 
           (resource-map a))
  (flush (atlas a))
  (setf (dirty a) nil))
