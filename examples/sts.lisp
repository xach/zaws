;;;; sts.lisp

(defpackage #:sts
  (:use #:cl))

(in-package #:sts)

(defclass sts-request (zaws:request zaws:query-auth-v2)
  ((duration-seconds
    :initarg :duration-seconds
    :accessor duration-seconds))
  (:default-initargs
   :protocol "https"
   :duration-seconds 3600
   :parameters (zaws:make-parameters "Action" "GetSessionToken"
                                     "Version" "2011-06-15")
   :host "sts.amazonaws.com"))

(defmethod zaws:prepare-for-signing :after ((request sts-request))
  (zaws:ensure-parameter "DurationSeconds"
                         (princ-to-string (duration-seconds request))
                         request))

