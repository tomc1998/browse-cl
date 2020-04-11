(in-package #:browse-cl)

(deftype var-id () 'fixnum)

(defclass env () 
  ((globals :initform (make-array '(1024)
                                  :initial-element nil
                                  :adjustable t
                                  :fill-pointer 0
                                  :element-type '(or null constant))
            :accessor globals
            :documentation "A storage of global values. Runtime values are
                            represented by integers, and integers >= 0
                            reference a slot in this array. Integers < 0 use
                            the stack."
            )
   (stack :initform (make-array '(1024)
                                :initial-element nil
                                :adjustable t
                                :element-type '(or null constant))
          :accessor stack
          :documentation "A storage of stack values. Runtime values are
                          represented by integers, and integers < 0 reference a
                          slot in this array, with -1 being index 0, -2 being
                          index 1, etc."))
  (:documentation "A runtime environment, with stack & global map"))

(defmethod alloc-global ((e env))
  (vector-push-extend nil (globals e))
  (- (length (globals e)) 1))
