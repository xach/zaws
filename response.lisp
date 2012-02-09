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

;;;; response.lisp

(in-package #:zaws)

(defgeneric request (response))

(defgeneric status-code (response)
  (:documentation
   "The HTTP status code of RESPONSE as an integer."))

(defgeneric reason-phrase (response)
  (:documentation
   "The HTTP reason phrase (e.g. \"OK\") of RESPONSE."))

(define-condition response-error (error)
  ((request
    :initarg :request
    :reader response-error-request)
   (response
    :initarg :response
    :reader response-error-response))
  (:report
   (lambda (condition stream)
     (let ((response (response-error-response condition))
           (request (response-error-request condition)))
       (format stream "~A ~A resulted in ~A ~A"
               (method request)
               (uri request)
               (status-code response)
               (reason-phrase response))))))

(defclass response (http-message)
  ((request
    :initarg :request
    :reader request)
   (status-code
    :initarg :status-code
    :reader status-code)
   (reason-phrase
    :initarg :reason-phrase
    :accessor reason-phrase)))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (format stream "~A ~A"
            (status-code response)
            (reason-phrase response))))
