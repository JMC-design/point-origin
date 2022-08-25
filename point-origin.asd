(in-package :asdf-user)
(defsystem "point-origin"
  :description "Point protocol for mfiano's origin."
  :version "0.0.1"
  :licence "LGPL"
  :author "Johannes Martinez Calzada"
  :depends-on ("point" "origin")
  :components ((:file "package")
               (:file "point")
               (:file "protocol")
               (:file "documentation")))
