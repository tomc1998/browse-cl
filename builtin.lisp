(in-package #:browse-cl)

(defun check-attr-allowed (s attr validation-list)
  "Given an 'attr and a 'validation list', which is a list of pairs (<string>
   <fn>) where the first item is the name of the attribut and the last item
  is a lambda which takes a 'ty and checks if it's valid, check if the attr
  allowed. Throws a simple-error with an error message if the attr isn't
  allowed, returns normally otherwise."
  (let ((attr-spec (member (name attr) validation-list :key #'car :test #'string=)))
    (when (not attr-spec) (error "Invalid attribute name '~a'" (name attr)))
    (when (not (funcall (nth 1 (car attr-spec)) (get-type s (val attr))))
        (error "Invalid attribute type '~a', for attr '~a'" 
               (get-type s (val attr)) (name attr))))) 

(defun check-mouse-event-listener (ty)
  (and (kind ty 'fn) 
       (ty-eq (ret (metadata ty)) *ty-void*)
       (= 1 (length (params (metadata ty))))
       (ty-eq (car (params (metadata ty))) *ty-mouse-event*)))

(defun init-builtin-scope (s)
  "Given a scope, add values to it to initialise the builtin environment.
   Returns the scope."
  ;; DOM nodes
  (set-in-scope s "COL" (make-instance 'constant :val nil :ty *ty-concrete-dom*))
  (set-in-scope s "ROW" (make-instance 'constant :val nil :ty *ty-concrete-dom*))
  (set-in-scope s "TEXT-INPUT" (make-instance 'constant :val nil :ty *ty-concrete-dom*))
  (set-in-scope s "TEXT" (make-instance 'constant :val nil :ty *ty-concrete-dom*))
  (set-in-scope s "EMPTY" (make-instance 'constant :val nil :ty *ty-concrete-dom*))
  (set-in-scope s "OVERFLOW" (make-instance 'constant :val nil :ty *ty-concrete-dom*))
  s)

(defmethod create-builtin-scope ((parent scope))
  (init-builtin-scope parent))


