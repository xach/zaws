;;;;
;;;; Copyright (c) 2012 Zachary Beane, All Rights Reserved
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; request.lisp

(in-package #:zaws)

(defgeneric protocol (request)
  (:documentation
   "The protocol of the request as a string, e.g. \"http\" or \"https\"."))

(defgeneric host (request)
  (:documentation
   "The target host of the request."))

(defgeneric method (request)
  (:method ((request t))
    :get)
  (:documentation
   "The HTTP method as a keyword, e.g. :GET, :POST, :PUT, etc."))

(defgeneric content-type (request)
  (:method ((request t))
    "application/x-www-form-urlencoded"))

(defgeneric uri-path (request)
  (:method (request)
    "/"))

(defgeneric parameters (request)
  (:method ((request t))
    nil))

(defgeneric (setf parameters) (new-value request))

(defgeneric ensure-parameter (key value request)
  (:method (key value request)
    (let* ((parameters (parameters request))
           (existing (assoc key parameters :test 'equalp)))
      (if existing
          (setf (cdr existing) value)
          (setf (parameters request) (acons key value parameters)))
      value)))

(defgeneric delete-parameter (key request)
  (:method (key request)
    (setf (parameters request)
          (remove key (parameters request)
                  :test 'string-equal
                  :key 'car)))
  (:documentation
   "Remove any parameter named KEY from REQUEST's list of
   parameters."))

(defgeneric check-response-error (request response)
  (:method ((request t) (response t))
    nil)
  (:documentation
   "Check the low-level response object for errors and signal if
   found."))

(defgeneric process-response (request response)
  (:method ((request t) (response t))
    response)
  (:documentation
   "Process a valid response object into a suitable specialized object
   according to REQUEST. The default method returns the response
   unchanged."))

(defgeneric submit (request)
  (:method :around (request)
   (let ((response (call-next-method)))
    (check-response-error request response)
    (process-response request response)))
  (:documentation
   "Send REQUEST and return a response object, or signal an error if
   there's a problem with the response."))

(defclass request (http-message)
  ((protocol
    :initarg :protocol
    :reader protocol)
   (host
    :initarg :host
    :reader host)
   (method
    :initarg :method
    :reader method)
   (content-type
    :initarg :content-type
    :accessor content-type)
   (parameters
    :initarg :parameters
    :accessor parameters)
   (headers
    :initarg :headers
    :accessor headers)
   (uri-path
    :initarg :uri-path
    :reader uri-path))
  (:default-initargs
   :protocol "http"
   :method :get
   :content-type "application/x-www-form-urlencoded"
   :parameters nil
   :headers nil
   :uri-path "/"
   :content ""))

(defmethod uri ((request request))
  (format nil "~A://~A~A"
          (protocol request)
          (host request)
          (uri-path request)))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :type t)
    (format stream "~S ~S" (method request) (uri request))))


(defmethod submit (request)
  (multiple-value-bind (content status-code headers uri content-stream
                                must-close
                                reason-phrase)
      (drakma:http-request (uri request)
                           :method (method request)
                           :additional-headers (headers request)
                           :parameters (parameters request)
                           :content-type (content-type request)
                           :content (content request))
    (let ((response
           (make-instance 'response
                          :request request
                          :content content
                          :status-code status-code
                          :reason-phrase reason-phrase
                          :headers headers
                          :uri (with-output-to-string (stream)
                                 (puri:render-uri uri stream)))))
      (when must-close
        (close content-stream))
      (check-response-error request response)
      (process-response request response))))
