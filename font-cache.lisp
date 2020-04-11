(in-package #:browse-cl)

(defclass font-size-cache ()
  ((path :initarg :path :accessor path :type 'string)
   (fonts :accessor fonts :initform (make-hash-table)
          :documentation "A map of font sizes to sdl2 fonts"))
  (:documentation "A cache for all the sizes of a given font"))

(defclass font-cache ()
  ((fonts :accessor fonts :initform (make-hash-table :test #'equal)
         :documentation "A map of font paths to font-size-cache instances"
         )))

(defmethod get-or-create-font-size-cache ((f font-cache) path)
  (let ((size-cache (gethash path (fonts f))))
    (if size-cache size-cache
        (progn
          (let ((new-size-cache (make-instance 'font-size-cache :path path)))
            (setf (gethash path (fonts f)) new-size-cache)
            new-size-cache)))))

(defmethod m-load-font ((f font-cache) path size)
  (let* ((fsc (get-or-create-font-size-cache f path))
        (font (gethash size (fonts fsc))))
    (if font font
        (let ((new-font (sdl2-ttf:open-font path size)))
          (setf (gethash size (fonts fsc)) new-font)
          new-font))))

(defmethod m-unload-font ((f font-cache) path size)
  "Unload a font if it exists"
  (let* ((fsc (get-or-create-font-size-cache f path))
         (f (gethash size (fonts fsc))))
    (when f 
      (sdl2-ttf:close-font f)
      (remhash size (fonts fsc)))))

(defparameter *global-font-cache* (make-instance 'font-cache))

(defun load-font (path size) 
  "Load the given font + size if not already loaded"
  (m-load-font *global-font-cache* path size))

(defun unload-font (path size) 
  "Unload a font if it exists"
  (m-unload-font *global-font-cache* path size))
