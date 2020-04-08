;;;; package.lisp

(uiop:define-package #:browse-cl
  (:shadowing-import-from #:alexandria #:switch)
  (:import-from #:alexandria curry)
    (:use #:cl #:cepl #:rtg-math #:vari
     :cepl.skitter :livesupport))
