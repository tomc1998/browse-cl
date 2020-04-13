(in-package #:browse-cl)

(defun parse-dom-node-key-attrs (form)
  "Given a list of attreters, where pairs of keyword attrs (e.g. a :key
   followed by an expr) are found throughout the list, find them & return a
  list of 'attr. "
  (let ((ii 0) (ret (list))) 
    (loop while (< ii (- (length form) 1))
          when (keywordp (nth ii form))
          do (let ((attr-name (string (nth ii form)))
                   (v (nth (+ ii 1) form))) 
               (push (make-instance 
                       'attr :name attr-name
                       :val (if (string= attr-name "STYLE")
                                (parse-style-expr v)
                                (parse-expr v)))
                     ret)
               ;; Skip over next item
               (setf ii (+ ii 1)))
          do (setf ii (+ ii 1)))
    ret))

(defun parse-dom-node-pos-attrs (form)
  "Given a list of attreters, where positional attrs are found throughout the
   list (e.g. attreters which AREN'T preceded by a keyword), find them &
   return a list of 'expr."
  (let ((ii 0) (ret (list))) 
    (loop while (< ii (length form))
          do (if (keywordp (nth ii form)) 
                ;; Skip keywords
                 (setf ii (+ ii 1))
                 (push (parse-expr (nth ii form)) ret))
          do (setf ii (+ ii 1)))
    (reverse ret)))

(defun parse-style-expr (form)
  (declare (ignore form))
  (error "Unimpl"))

(defun parse-set-expr (form)
  "Parses a cst-set-expr"
  (assert (and (listp form) (symbolp (car form)) 
               (string= "SET" (string (car form)))))
  (make-instance 'cst-set-expr 
                 :target (parse-expr (nth 1 form)) 
                 :val (parse-expr (nth 2 form))))

(defun parse-arr-lit (form)
  "Parses a cst-arr-lit"
  (assert (and (listp form) (symbolp (car form)) 
               (string= "ARR" (string (car form)))))
  (make-instance 'cst-arr-lit
                 :val (loop for n in (nthcdr 1 form) 
                            collect (parse-expr n))))

(defun parse-loop (form)
  "Parse a cst-loop"
  (assert (and (listp form) (symbolp (car form)) 
               (string= "FOR" (string (car form)))))
  (assert (and (symbolp (nth 2 form)) 
               (string= "IN" (string (nth 2 form)))))
  (assert (symbolp (nth 1 form)))
  (make-instance 'cst-loop
                 :item-name (string (nth 1 form))
                 :target (parse-expr (nth 3 form))
                 :body (mapcar #'parse-expr (nthcdr 4 form))))

(defun parse-expr (form)
  (cond
    ;; Anonymous function
    ((and (listp form) (symbolp (car form)) (string= (string (car form)) "FN"))
     (parse-anon-fn form))
    ((and (listp form) (symbolp (car form)) (string= (string (car form)) "SET"))
     (parse-set-expr form))
    ((and (listp form) (symbolp (car form)) (string= (string (car form)) "ARR"))
     (parse-arr-lit form))
    ((and (listp form) (symbolp (car form)) (string= (string (car form)) "FOR"))
     (parse-loop form))
    ((listp form) (parse-fn-call form))
    ((integerp form) (make-instance 'cst-int-lit :val form))
    ((numberp form) (make-instance 'cst-num-lit :val form))
    ((stringp form) (make-instance 'cst-string-lit :val form))
    ((symbolp form) (make-instance 'cst-var :name (string form)))
    (t (error "Unimpl"))))

(defun parse-fn-call (form)
  "Parses a cst-fn-call"
  (assert (and (listp form) (symbolp (car form))))
  (let ((name (string (car form)))
        (key-args (parse-dom-node-key-attrs (cdr form)))
        (pos-args (parse-dom-node-pos-attrs (cdr form))))
    (make-instance 'cst-fn-call 
                   :fn-name name 
                   :key-args key-args :pos-args pos-args)))

(defun parse-param-list (form)
  "Parses the form into a list of 'cst-param"
  ;; TODO proper error
  (assert (and (listp form) (evenp (length form))))
  (loop for (k v) on form by #'cddr 
        ;; TODO proper error
        do (assert (symbolp k)) 
        collect (make-instance 'cst-param 
                               :name (string k)
                               :ty (parse-expr v))))

(defun parse-anon-fn (form)
  "Parses a cst-fn"
  (assert (and (listp form) (symbolp (car form)) (string= (string (car form)) "FN")))
  (let ((params (parse-param-list (nth 1 form)))
        (ret-ty (parse-expr (nth 2 form)))
        (body (mapcar #'parse-expr (nthcdr 3 form))))
    (make-instance 'cst-fn :params params :ret-ty ret-ty :body body)))

(defun parse-var-decl (form)
  ;; (var <name> [<ty>] <val>)
  (assert (and (listp form) (symbolp (car form)) 
               (string= (string (car form)) "VAR")))
  (let*
    ((name (format nil "~a" (nth 1 form)))
     (ty (if (= (length form) 4) (parse-expr (nth 2 form))))
     (val-form (nth (if (= (length form) 4) 3 2) form))
     (val (parse-expr val-form)))
    (make-instance 'cst-var-decl :name name :ty ty :val val)))

(defun parse-top-level-form (form)
  "Given a scope & dom node expr, parse & return a template-dom-node."
  (when (not (listp form)) 
    (error 'browser-parse-error :tl-expr form :expr form 
           :msg "Expected list"))
  (when (not (symbolp (car form))) 
    (error 'browser-parse-error :tl-expr :expr (car form) 
           :msg "Expected symbol as first element in expression"))
  (let ((tagname (string (car form))))
    (cond
      ((string= "VAR" tagname)
       (parse-var-decl form))
      (t (parse-expr form)))))

(defun parse-program (forms)
  "Run parse-top-level-form on all the given forms. Return a list of those top
   level forms, as a list of cst-node."
  (assert (listp forms))
  (mapcar #'parse-top-level-form forms))
