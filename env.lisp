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
   (dirty-globals :initform (make-array '(1024) 
                                        :initial-element nil 
                                        :adjustable t 
                                        :fill-pointer 0 
                                        :element-type 'boolean)
                  :accessor dirty-globals
                  :documentation 
                  "Matches globals, if t then the corresponding
                   global has been changed since the last call
                   to clear-dirty-globals")
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

(defmethod clear-dirty-globals ((e env))
  (loop for ii below (length (dirty-globals e)) do
        (setf (aref (dirty-globals e) ii) nil)))

(defun (setf env-lookup) (val e id)
  "Set some id in the env to a given value. Updates the dirty array when
   updating a global."
  (if (>= id 0)
      (progn
        (setf (aref (globals e) id) val)
        (setf (aref (dirty-globals e) id) t))
      (setf (aref (stack e) (+ (stack-base e) (- (- id) 1))) val))
  )

(defmethod env-lookup ((e env) id)
  (if (>= id 0)
      (aref (globals e) id)
      (aref (stack e) (+ (stack-base e) (- (- id) 1)))))

(defmethod is-dirty ((e env) id)
  "Returns true if this is a global and the dirty bool is set"
  (and (>= id 0) (aref (dirty-globals e) id)))

(defmethod alloc-stack ((e env))
  (vector-push-extend nil (stack e))
  (- (- (length (stack e)) (stack-base e))))

(defmethod alloc-global ((e env))
  (vector-push-extend nil (globals e))
  (vector-push-extend nil (dirty-globals e))
  (- (length (globals e)) 1))
