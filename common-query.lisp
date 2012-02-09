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

;;;; common-query.lisp

;;;
;;; Most AWS services have a similar style: One request parameter is
;;; named Action, and the rest are arguments to that Action. Make it
;;; easy to write support for services like that.
;;;

(in-package #:zaws)

(defgeneric action (request)
  (:documentation
   "The 'action' of a common query request."))

(defgeneric action-parameters (request)
  (:documentation
   "The parameters given to the 'action' of a common query request."))

(defgeneric api-version (request)
  (:documentation
   "The API version of a request."))

(defclass common-query-request (request query-auth-v2)
  ((action
    :initarg :action
    :reader action)
   (action-parameters
    :initarg :action-parameters
    :accessor action-parameters)
   (api-version
    :initarg :api-version
    :reader api-version))
  (:default-initargs
   :action-parameters nil))

(defmethod prepare-for-signing :after ((request common-query-request))
  (ensure-parameter "Action" (action request) request)
  (ensure-parameter "Version" (api-version request) request)
  (do-parameters (key value)
      (action-parameters request)
    (ensure-parameter key value request)))

(defun submit-common-query (class method action &rest action-parameters)
  (let ((request (make-instance class
                                :method method
                                :action action
                                :action-parameters
                                (apply 'make-parameters action-parameters))))
    (submit request)))

(defun submit-common-query* (host api-version method action
                             &rest action-parameters)
  (let ((request (make-instance 'common-query-request
                                :host host
                                :api-version api-version
                                :method method
                                :action action
                                :action-parameters
                                (apply 'make-parameters action-parameters))))
    (submit request)))
