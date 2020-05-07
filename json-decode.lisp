
(in-package #:browse-cl)

(defun fit-to-constant (val ty) 
  (cond
    ((ty-eq ty *ty-void*) nil)
    ((ty-eq ty *ty-int*) 
     (assert (typep val 'number))
     (floor val))
    ((ty-eq ty *ty-num*) 
     (assert (typep val 'number))
     val)
    ((ty-eq ty *ty-string*)
     (assert (typep val 'string))
     val)
    ((eq (kind ty) 'arr)
     (assert (typep val 'list))
     (let ((ret (make-array (list (length val))
                         :adjustable t
                         :fill-pointer 0
                         :initial-element nil))
           (inner-t (metadata ty))) 
       (loop for v in val do 
             (vector-push-extend (fit-to-constant v inner-t) ret))
       ret))
    ((eq (kind ty) 'struct)
     (assert (listp val))
     (let ((ret (make-array (list (length (metadata ty)))
                            :fill-pointer 0
                            :initial-element nil))) 
       (loop for f in (metadata ty) do
             (let ((res (assoc (intern (name f) :keyword) val)))
               (assert res)
               (vector-push (fit-to-constant (cdr res) (ty f)) ret)
               ))
       ret))))

(defun decode-json-to-constant (strm ty)
  ;; Given a stream 'strm' containing some json, decode that into a value of
  ;; language type type 'ty', return that value.
  (fit-to-constant (json:decode-json strm) ty))
