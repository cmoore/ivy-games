;;;; ivy-games.asd

(asdf:defsystem #:ivy-games
  :serial t
  :description "Describe ivy-games here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:parenscript
               #:parenscript-utils
               #:cl-who
               #:css-lite
               #:cl-ivy)
  :components ((:file "package")
               (:file "ivy-games")))
