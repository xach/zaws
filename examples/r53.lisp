;;;; r53.lisp

(defpackage #:r53
  (:use #:cl))

(in-package #:r53)

(defclass r53-request (zaws:request zaws:query-auth-v3)
  ()
  (:default-initargs
   :uri-path "/2011-05-05/hostedzone"
   :host "route53.amazonaws.com"
   :protocol "https"))

