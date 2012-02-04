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

;;;; http-message.lisp

(in-package #:zaws)

(defgeneric content (object)
  (:method (object)
    "")
  (:documentation
   "The content (aka body) of OBJECT."))

(defgeneric (setf content) (new-value object))

(defgeneric headers (object)
  (:documentation
   "The headers of OBJECT.")
  (:method (object)
    nil))

(defgeneric (setf headers) (new-value object))

(defgeneric uri (object)
  (:documentation
   "The URI of OBJECT."))

(defgeneric (setf uri) (new-value object))

(defgeneric ensure-header (key value http-message)
  (:documentation
   "Ensure that the header named by KEY in HTTP-MESSAGE has the value
   VALUE, updating or creating the header as needed.")
  (:method (key value http-message)
    (let* ((headers (headers http-message))
           (existing (assoc key headers :test 'equalp)))
      (if existing
          (setf (cdr existing) value)
          (setf (headers http-message) (acons key value headers)))
      value)))

(defgeneric delete-header (key http-message)
  (:documentation
   "Remove any header named by KEY from HTTP-MESSAGE's list of
   headers.")
  (:method (key http-message)
    (setf (headers http-message)
          (remove key (headers http-message)
                  :test 'string-equal
                  :key 'car))))

(defgeneric header-value (key http-message)
  (:documentation
   "Return the value of the header named by KEY.")
  (:method (key http-message)
    (cdr (assoc key (headers http-message)
                :test 'string-equal))))

(defclass http-message ()
  ((content
    :initarg :content
    :accessor content)
   (headers
    :initarg :headers
    :accessor headers)
   (uri
    :initarg :uri
    :accessor uri))
  (:documentation
   "HTTP-MESSAGE is the parent class of both REQUESTs and
   RESPONSEs."))
