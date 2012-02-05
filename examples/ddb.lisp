;;;; ddb.lisp

(defpackage #:ddb
  (:use #:cl))

(in-package #:ddb)

(defclass ddb-request (zaws:request zaws:json-auth-v3)
  ((action
    :initarg :action
    :accessor action))
  (:default-initargs
   :host "dynamodb.us-east-1.amazonaws.com"
   :content-type "application/x-amz-json-1.0"
   :method :post
   :uri-path "/"))

(defmethod zaws:prepare-for-signing :after ((request ddb-request))
  (zaws:ensure-header "x-amz-target"
                      (format nil "DynamoDB_20111205.~A"
                              (action request))
                      request))
