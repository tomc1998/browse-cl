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

(defun parse-expr (form)
  (cond
    ((listp form)
      ;; Assume DOM node for now
      (parse-dom-node form))
    ((integerp form) (make-instance 'cst-int-lit :val form))
    ((numberp form) (make-instance 'cst-num-lit :val form))
    ((stringp form) (make-instance 'cst-string-lit :val form))
    ((symbolp form) (make-instance 'cst-var :name (string form)))
    (t (error "Unimpl"))))

(defun parse-concrete-tagname (tl-expr name)
  (if (member name '("COL" "EMPTY" "ROW" "TEXT") :test #'string=)
      (intern name)
      (error 'browse-parse-error :tl-expr tl-expr 
             :expr name :msg "Unrecognised tag name")))

(defun parse-dom-node (form)
  "Given a scope & dom node expr, parse & return a template-dom-node."
  (assert (and (listp form) (symbolp (car form))))
  (let ((tagname (string (car form)))
        (attrs (parse-dom-node-key-attrs (cdr form)))
        (children (parse-dom-node-pos-attrs (cdr form))))
    (make-instance 'cst-dom-node :tagname (parse-concrete-tagname form tagname) 
                   :attrs attrs :children children)))

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
      (t ;; Assume this is a dom node
       (parse-dom-node form)))))

(defun parse-program (forms)
  "Run parse-top-level-form on all the given forms. Return a list of those top
   level forms, as a list of cst-node."
  (assert (listp forms))
  (mapcar #'parse-top-level-form forms))
