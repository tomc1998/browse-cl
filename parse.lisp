(in-package #:browse-cl)

(defun parse-dom-node-key-attrs (s form)
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
                                (parse-style-expr s v)
                                (parse-expr s v)))
                     ret)
               ;; Skip over next item
               (setf ii (+ ii 1)))
          do (setf ii (+ ii 1)))
    ret))

(defun parse-dom-node-pos-attrs (s form)
  "Given a list of attreters, where positional attrs are found throughout the
   list (e.g. attreters which AREN'T preceded by a keyword), find them &
   return a list of 'expr."
  (let ((ii 0) (ret (list))) 
    (loop while (< ii (length form))
          do (if (keywordp (nth ii form)) 
                ;; Skip keywords
                 (setf ii (+ ii 1))
                 (push (parse-expr s (nth ii form)) ret))
          do (setf ii (+ ii 1)))
    (reverse ret)))

(defun parse-style-expr (s form)
  (declare (ignore s) (ignore form))
  (error "Unimpl"))

(defun parse-expr (s form)
  (cond
    ((listp form)
    ;; Assume DOM node for now
    (parse-dom-node s form))
    ((integerp form) (make-instance 'constant :ty *ty-int* :val form))
    ((numberp form) (make-instance 'constant :ty *ty-num* :val form))
    ((stringp form) (make-instance 'constant :ty *ty-string* :val form))
    (t (error "Unimpl"))))

(defun parse-concrete-tagname (tl-expr name)
  (if (member name '("COL" "EMPTY" "ROW") :test #'string=)
      (intern name)
      (error 'browse-parse-error :tl-expr tl-expr 
             :expr name :msg "Unrecognised tag name")))

(defun parse-dom-node (s form)
  "Given a scope & dom node expr, parse & return a template-dom-node."
  (when (not (listp form)) 
    (error 'browser-parse-error :tl-expr form :expr form 
           :msg "Expected list"))
  (when (not (symbolp (car form))) 
    (error 'browser-parse-error :tl-expr :expr (car form) 
           :msg "Expected symbol as first element in expression"))
  (let ((tagname (string (car form))))
    (cond
      ((string= "TEXT" tagname)
       (let ((attrs (parse-dom-node-key-attrs s (cdr form)))
             (exprs (parse-dom-node-pos-attrs s (cdr form)))) 
         (make-instance 'template-text-node :attrs attrs :exprs exprs)))
      (t 
       ;; Extract key / pos attrs
       (let ((attrs (parse-dom-node-key-attrs s (cdr form)))
             (children (parse-dom-node-pos-attrs s (cdr form))))
         (make-instance 'template-concrete-dom-node
                        :tag (parse-concrete-tagname form tagname)
                        :attrs attrs
                        :children children))))))

