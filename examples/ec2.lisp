;;;; ec2.lisp

(defpackage #:ec2
  (:use #:cl))

(in-package #:ec2)

(defclass ec2-request (zaws:request zaws:query-auth-v2)
  ((action
    :initarg :action
    :accessor action))
  (:default-initargs
   :parameters (zaws:make-parameters "Version" "2011-12-15")
   :protocol "https"
   :host "ec2.amazonaws.com"))

(defmethod zaws:sign :before ((request ec2-request))
  (zaws:ensure-parameter "Action" (action request) request))
