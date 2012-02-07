;;;; zaws-xml.asd

(asdf:defsystem #:zaws-xml
  :depends-on (#:cxml)
  :serial t
  :components ((:file "package")
               (:file "xml")))
