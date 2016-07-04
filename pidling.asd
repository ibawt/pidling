;;;; pidling.asd

(asdf:defsystem #:pidling
  :description "Describe pidling here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:sdl2
               #:sdl2-image
               #:cl-opengl
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "pidling")
               (:file "app")))
