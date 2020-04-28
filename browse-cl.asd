;;;; browse-cl.asd

(asdf:defsystem #:browse-cl
  :description "Describe browse-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl #:alexandria #:cl-ppcre #:rtg-math.vari #:cepl.sdl2 #:cepl.sdl2-ttf #:swank #:livesupport #:cepl.skitter.sdl2 #:dirt #:drakma)
  :components ((:file "package")

               ;; Util / defs
               (:file "error")

               ;; Low level rendering
               (:file "adj-c-array")
               (:file "font-cache")
               (:file "bin-packer")
               (:file "atlas")
               (:file "atlas-manager")
               (:file "font-renderer")
               (:file "painter")

               ;; Lang runtime / def
               (:file "env")
               (:file "scope")
               (:file "builtin")
               (:file "global-store")
               (:file "ty")
               (:file "expr")
               (:file "inline-methods")

               ;; DOM
               (:file "dom")
               (:file "layout")

               ;; Compiler
               (:file "top-level-statement")
               (:file "parse-tree")
               (:file "parse")
               (:file "cst-component")
               (:file "component")
               (:file "compiler")

               ;; Rendering
               (:file "renderer")

               ;; Main
               (:file "browse-cl")))
