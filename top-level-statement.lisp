(in-package #:browse-cl)

(defclass var-decl (expr)
  ((var :initarg :var :accessor var :type var-id)
   (ty :initarg :ty :accessor ty :type ty)
   (val :initarg :val :accessor val :type expr)))
