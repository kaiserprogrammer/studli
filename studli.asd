(defsystem :studli
  :version "0.0.1"
  :licence "GPL"
  :components
  ((:file "studli"))
  :depends-on (:drakma
               :alexandria
               :cl-ppcre
               :html-entities
               :cl-fad
               :lisp-unit))

