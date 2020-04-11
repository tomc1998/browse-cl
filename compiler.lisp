(in-package #:browse-cl)

(defun compile-browser-program (program)
  "Return (values tree env), a template dom tree and an environment
   to run the given program"
  (let ((cst (parse-program program))
        (s (create-global-scope))
        (e (make-instance 'env)))

    ;; Setup env space for any var decls
    (loop for statement in cst do
          (cond
            ((typep statement 'cst-var-decl)
             (let ((id (alloc-global e))
                   (expr (to-expr s (val statement)))
                   )
               (set-in-scope 
                 s (name statement) 
                 (make-instance 
                   'runtime-value :id id 
                   :ty (if (ty statement) 
                           (to-ty s (ty statement))
                           (get-type expr)
                           )))
               (setf (aref (globals e) id) (eval-expr e expr))))))

    (values (to-expr s (car (last cst))) e)))
