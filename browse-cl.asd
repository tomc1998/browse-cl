;;;; browse-cl.asd

(asdf:defsystem #:browse-cl
  :description "Describe browse-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl #:cl-ppcre #:rtg-math.vari #:cepl.sdl2 #:cepl.sdl2-ttf #:swank #:livesupport #:cepl.skitter.sdl2 #:dirt)
  :components ((:file "package")
               (:file "adj-c-array")
               (:file "font-cache")
               (:file "bin-packer")
               (:file "atlas")
               (:file "atlas-manager")
               (:file "painter")
               (:file "browse-cl")))
