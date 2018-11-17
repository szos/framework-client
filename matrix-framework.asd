;;;; matrix-framework.asd

(asdf:defsystem #:matrix-framework
  :description "Describe matrix-framework here"
  :author "Your Name <your.name@example.com>"
  :license "GPL v2"
  :serial t
  :depends-on (#:drakma
               #:flexi-streams
               #:yason)
  :components ((:file "package")
               (:file "matrix-framework")))

