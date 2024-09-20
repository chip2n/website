(asdf:defsystem #:website
  :description "My personal website"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("navi"
               "spinneret")
  :components ((:file "package")
               (:file "utils")
               (:file "site")
               (:file "page")
               (:file "project")
               (:file "pages/index")
               (:file "pages/about")
               (:file "pages/post-list")
               (:file "pages/project-list")
               (:file "pages/project-zball")
               (:file "pages/project-navi")))
