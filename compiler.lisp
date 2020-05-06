(in-package #:browse-cl)

(defun compile-browser-program (program)
  "Return (values tree env), a template dom tree and an environment
   to run the given program"
  (let ((cst (parse-program program))
        (s (create-global-scope))
        (e (make-instance 'env)))

    ;; TODO make this order-independent

    ;; Setup typedefs
    (loop for statement in cst
          when (typep statement 'cst-typedef) do
          (set-in-scope s (name statement) (to-ty s (val statement))))


    ;; Setup env space for any var decls
    (loop for statement in cst
          when (typep statement 'cst-var-decl) do
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
            (setf (aref (globals e) id) (eval-expr e expr))))

    ;; Setup components
    (loop for statement in cst
          when (typep statement 'cst-component)
          do (set-in-scope s (name statement)
                           (make-instance 'constant :val nil
                                          :ty (make-instance 'ty :name (name statement) 
                                                             :metadata statement
                                                             :kind 'component))))

    ;; Setup iterative functions ('every' functions)
    (loop for statement in cst
          when (typep statement 'cst-every)
          do (push (make-instance 
                     'interval-fn
                     :time-per-exec 
                     (eval-expr e (to-expr s (millis statement)))
                     :fn (make-instance 
                           'expr-seq
                           :exprs (mapcar (curry #'to-expr s) 
                                          (exprs statement))))
                   (interval-fns e)))

    (values (to-expr s (car (last cst))) e)))
