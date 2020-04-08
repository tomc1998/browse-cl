(in-package #:browse-cl)

;; Eval this whole file to reset the global state of the app.

;; A map of component names to components
(defparameter *component-store* (make-hash-table :test 'equal))
;; A map of route names to component symbols
(defparameter *route-store* (make-hash-table :test 'equal))

;; Store of types, used to generate global scope
(defparameter *ty-store* (make-hash-table :test 'equal))

;; Similar to ty-store, but only for builtin types and not cleared with
;; clear-state
(defparameter *builtin-ty-store* (make-hash-table :test 'equal))

(defun add-type-to-global-store (ty)
  "Add a type to the global store. (name ty) will be used for the name of the
   type. After calling this function, scopes created with create-global-scope
   will have this type available under (name ty).
   Returns the ty."
  (setf (gethash (name ty) *ty-store*) ty)
  ty)

(defun add-type-to-builtin-store (ty)
  "Add a type to the global store. (name ty) will be used for the name of the
   type. After calling this function, scopes created with create-global-scope
   will have this type available under (name ty).
   Returns the ty."
  (setf (gethash (name ty) *builtin-ty-store*) ty)
  ty)

;; Clear all global state
(defun clear-state ()
  (setf *component-store* (make-hash-table :test 'equal))
  (setf *route-store* (make-hash-table :test 'equal))
  (setf *ty-store* (make-hash-table :test 'equal)))

(defmethod add-component-to-global-store (c)
  (setf (gethash (name c) *component-store*) c))

(defmethod add-route-to-global-store (path c)
  (setf (gethash path *route-store*) c))

(defun create-global-scope ()
  "Analyse global stores, generate a scope from it."
  (let ((s (make-instance 'scope))) 
    (maphash (lambda (k v) (set-in-scope s k v)) *component-store*)
    (maphash (lambda (k v) (set-in-scope s k v)) *ty-store*)
    (maphash (lambda (k v) (set-in-scope s k v)) *builtin-ty-store*)
    (create-builtin-scope s)))

