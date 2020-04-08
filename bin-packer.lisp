(in-package #:browse-cl)

(defclass bin-pack-node ()
  ((val :initarg :val :accessor val :type 't)
   (x-off :initform 0 :initarg :x-off :accessor x-off :type 'fixnum)
   (y-off :initform 0 :initarg :y-off :accessor y-off :type 'fixnum)
   (width :initarg :width :accessor width :type 'integer)
   (height :initarg :height :accessor height :type 'integer)
   (right :initform nil :accessor right :type 'bin-pack-node)
   (bottom :initform nil :accessor bottom :type 'bin-pack-node)))

(defun make-bin-packer (width height &optional (x-off 0) (y-off 0))
  "x-off and y-off are only for recursive calls for sub-nodes. Leave at
   default values."
  (make-instance 'bin-pack-node :val nil 
                 :width width :height height 
                 :x-off x-off :y-off y-off))

(define-condition bin-packer-full (error) ())

(defmethod pack ((bin-packer bin-pack-node) val width height)
  "Pack a value into the bin packer. Val must be non-nil. Signals bin-packer-full if full.
   Returns the bin-pack-node which successfully packed the value on success."
  (assert val)
  (if (not (val bin-packer)) 
      (progn
        (when (or (< (width bin-packer) width)
                  (< (height bin-packer) height))
          (error 'bin-packer-full))
        (setf (val bin-packer) val)
        (let ((right-w (- (width bin-packer) width))
              (right-h height)
              (bottom-w (width bin-packer))
              (bottom-h (- (height bin-packer) height)))
          (setf (right bin-packer) 
                (make-bin-packer right-w right-h 
                                 (+ width (x-off bin-packer))
                                 (y-off bin-packer)))
          (setf (bottom bin-packer) 
                (make-bin-packer bottom-w bottom-h
                                 (x-off bin-packer)
                                 (+ height (y-off bin-packer))))
          (setf (width bin-packer) width)
          (setf (height bin-packer) height)
          bin-packer))
      (handler-case
        (pack (right bin-packer) val width height) 
        (bin-packer-full () (pack (bottom bin-packer) val width height)))))
