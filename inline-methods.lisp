;; This file is responsible for setting up the inline methods on the global
;; types.
(in-package #:browse-cl)

(defmacro def-inline-methods (&rest methods)
  "Convenience macro for defining a list of inline methods.
   (def-inline-methods <def*>)

   <def> ::= (<name> (<param*>) <ty> <body>)
   <param> ::= <name> <ty>
   <body> ::= quasiquoted expression which interpolates the params, plus a
              special 'this' variable.

   # Example

   (def-inline-methods
     (add (x int) `(+ ,this ,b)))

   "
  `(list 
     ,@(loop for m in methods
             do (assert (= 4 (length m)))
             collect 
             (let* ((param-names (loop for (k v) on (nth 1 m) by #'cddr collect (string k)))
                    (params (loop for (k v) on (nth 1 m) by #'cddr collect 
                                  `(make-instance 'param :name ,(string k)
                                                  :ty (parse-ty-expr (create-global-scope) ',v))))
                    (body `(lambda (this ,@(loop for p in param-names 
                                                 collect (intern p)))
                             ,(nth 3 m))))
               `(make-instance 
                  'ty-inline-method 
                  :name ,(string (nth 0 m)) 
                  :params (list ,@params)
                  :ret-ty (parse-ty-expr (create-global-scope) ',(nth 2 m))
                  :eval-fn (eval ,body))))))

(setf (methods *ty-string*)
      (def-inline-methods 
        (length () int (length this))
        (+ (s string) string (concatenate 'string this s))
        (= (s string) bool (string= this s))))

(setf (methods *ty-num*)
      (def-inline-methods 
        (+ (x int) num (+ this x))
        (+ (x num) num (+ this x))
        (- (x int) num (- this x))
        (- (x num) num (- this x))
        (* (x int) num (* this x))
        (* (x num) num (* this x))
        (/ (x int) num (/ this x))
        (/ (x num) num (/ this x))

        (>  (x int) bool (>  this x))
        (>  (x num) bool (>  this x))
        (<  (x int) bool (<  this x))
        (<  (x num) bool (<  this x))
        (>= (x int) bool (>= this x))
        (>= (x num) bool (>= this x))
        (<= (x int) bool (<= this x))
        (<= (x num) bool (<= this x))
        (=  (x int) bool (=  this x))
        (=  (x num) bool (=  this x))

        (int () int (floor this))))

(setf (methods *ty-int*)
      (def-inline-methods 
        (+  (x int) int  (+  this x))
        (+  (x num) num  (+  this x))
        (-  (x int) int  (-  this x))
        (-  (x num) num  (-  this x))
        (*  (x int) int  (*  this x))
        (*  (x num) num  (*  this x))
        (/  (x int) int  (floor (/  this x)))
        (/  (x num) num  (/  this x))
        (>  (x int) bool (>  this x))
        (>  (x num) bool (>  this x))
        (<  (x int) bool (<  this x))
        (<  (x num) bool (<  this x))
        (>= (x int) bool (>= this x))
        (>= (x num) bool (>= this x))
        (<= (x int) bool (<= this x))
        (<= (x num) bool (<= this x))
        (=  (x int) bool (=  this x))
        (=  (x num) bool (=  this x))

        ;; Cast
        (num () num (float this))
        
        ))

(setf (methods *ty-bool*)
      (def-inline-methods 
        (and (x bool) bool (and this x))
        (or  (x bool) bool (or  this x))))

