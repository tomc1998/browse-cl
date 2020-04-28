(in-package #:browse-cl)

(defclass cst-component-param (param)
  ((pos :initarg :pos :accessor pos :type 'boolean)))

(defclass cst-component ()
  ((name :initarg :name :accessor name :type 'string)
   ;; List of 'cst-component-param
   (params :initarg :params :accessor params :type 'list)
   ;; List of 'single-data-def
   (data :initarg :data :initform (list) :accessor data :type 'list)
   ;; Constant stuff - e.g. functions. List of cst-single-const-def
   (const :initarg :const :initform (list) :accessor const :type 'list)
   ;; The view function
   (view :initarg :view :initform nil :accessor view :type 'view-def)))

(defclass cst-component-def () ())
(defclass cst-view-def (cst-component-def)
  ((root :initarg :root :accessor root :type 'expr)))
(defclass cst-single-data-def ()
  ((name :initarg :name :accessor name :type 'string)
   (ty :initarg :ty :accessor ty :type 'expr)
   (val :initarg :val :accessor val :type 'expr)))
(defclass cst-data-def (cst-component-def)
  ;; List of single-data-def
  ((items :initarg :items :accessor items :type 'list)))
(defclass cst-single-const-def ()
  ((name :initarg :name :accessor name :type 'string) 
   ;; First arg of fn (if this is a fn) should be the receiver
   (val :initarg :val :accessor val :type 'expr)))
(defclass cst-const-def (cst-component-def)
  ;; list of 'cst-single-const-def
  ((items :initarg :items :accessor items :type 'list)))


(defun parse-const-component-def (name val)
  "Given a name & a val, return a cst-single-const-def. This handles fn decls specially."
  (assert (symbolp name))
  (make-instance 
    'cst-single-const-def :name (string name) 
    :val (parse-expr val)))

(defun parse-component-def (form)
  (assert (symbolp (car form)))
  (cond
    ((string= (string (car form)) "VIEW") 
     (make-instance 'cst-view-def :root (parse-expr (nth 1 form))))
    ((string= (string (car form)) "DATA") 
      (make-instance 
        'cst-data-def :items 
        (loop for (k ty v) on (cdr form) by #'cdddr 
              collect (make-instance 
                        'cst-single-data-def :name (string k) 
                        :val (parse-expr v)
                        :ty (parse-expr ty)))))
    ((string= (string (car form)) "CONST")
     (make-instance 
       'cst-const-def 
       :items (loop for (k v) on (cdr form) by #'cddr do
                    (assert (symbolp k))
                    collect (parse-const-component-def k v))))
    (t (error "Unknown component def ~a" (car form)))))

(defun aggregate-component-def (c defs)
  "Given a component and a list of defs, add those defs to that
   component. Signal an error if another def is found for a type where
   we can't
   have multiple defs (e.g. view)
   Modifies `c` and also returns `c`"
  (loop for d in defs do 
        (cond
          ((typep d 'cst-view-def) 
           (if (view c) 
               (error "Duplicate 'view' defs detected on component ~a" (name c))
               (setf (view c) d)))
          ((typep d 'cst-data-def)
           (loop for item in (items d) 
                 do (push item (data c))))
          ((typep d 'cst-const-def)
           (loop for item in (items d) 
                 do (push item (const c))))
          (t (error "Unknown component def ~S" d))))
  c)

(defun parse-pos-params (form)
  "Given a list of parameters, extract a list 'cst-component-param.  Params are
   considered positional params when their key is NOT a :keyword symbol."
  (loop for (k v) on form by #'cddr
        when (not (keywordp k))
        collect (make-instance 'cst-component-param :name (string k) 
                               :ty (parse-expr v)
                               :pos t)))

(defun parse-key-params (form)
  "Given a list of parameters, extract a list of 'cst-component-param. Params are
   considered keyword params when their key is a :keyword symbol."
  (loop for (k v) on form by #'cddr
        when (keywordp k)
        collect (make-instance 'cst-component-param 
                               :name (string k) 
                               :ty (parse-expr v)
                               :pos nil)))

(defun parse-defcomp (form)
  "Parse a (defcomp <name> <params>) with the given scope `s`"
  (assert (and (symbolp (car form)) (string= (string (car form)) "DEFCOMP")))
  (let ((name (string (nth 1 form)))
        (key-params (parse-key-params (nth 2 form)))
        (pos-params (parse-pos-params (nth 2 form)))
        (defs (loop for def in (nthcdr 3 form) 
                    collect (parse-component-def def))))
    (aggregate-component-def 
      (make-instance 'cst-component :name name 
                     :params (concatenate 'list key-params pos-params)) defs)))

