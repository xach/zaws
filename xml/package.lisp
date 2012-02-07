;;;; package.lisp

(defpackage #:zaws-xml
  (:use #:cl)
  (:export #:defbinder
           #:xml-bind
           #:try-to-xml-bind
           #:include
           #:alternate
           #:bind
           #:optional
           #:skip-rest
           #:sequence
           #:elements-alist
           #:bvalue
           #:bfun
           #:alist-bind))


