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
                            the stack.")
   (stack :initform (make-array '(1024)
                                :initial-element nil
                                :adjustable t
                                :fill-pointer 0
                                :element-type '(or null constant))
          :accessor stack
          :documentation "A storage of stack values. Runtime values are
                          represented by integers, and integers < 0 reference a
                          slot in this array offset by stack-base, with -1
                          being index 0, -2 being index 1, etc.  Top of the
                          stack is at fill-pointer, base of the stack is stack-base.")
  (stack-base :initform 0 :accessor stack-base 
              :documentation "This is a pointer to the base of the stack in
                              the current frame
                              
                              The value directly BELOW the base pointer is the
                              location of the previous stack base"))
  (:documentation "A runtime environment, with stack & global map"))

(defmethod env-lookup ((e env) id)
  (if (>= id 0)
      (aref (globals e) id)
      (aref (stack e) (+ (stack-base e) (- (- id) 1)))))

(defmethod alloc-stack ((e env))
  (vector-push-extend nil (stack e))
  (- (- (length (stack e)) (stack-base e))))

(defmethod alloc-global ((e env))
  (vector-push-extend nil (globals e))
  (- (length (globals e)) 1))
