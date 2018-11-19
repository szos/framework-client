;;;; matrix-framework.asd

(asdf:defsystem #:matrix-framework
  :description "This heres a bad description... Tested on SBCL linux."
  :author "Your Name <your.name@example.com>"
  :license "GPL v3"
  :serial t
  :depends-on (#:drakma
               #:flexi-streams
               #:yason)
  :components ((:file "package")
               (:file "matrix-framework")
	       (:file "parse-timeline,etc")
	       (:file "parse-chambers")))

