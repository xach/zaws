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

;;;; credentials.lisp

(in-package #:zaws)

(defvar *credentials* nil
  "The credentials used for authenticating AWS requests. Built-in
  credentials can be a list, a pathname, or a string designating a
  pathname. For a list, the first two elements are used as the access
  key and secret key, and the third element, if present, is the
  session token. For a pathname, the first two lines of the file are
  the access key and secret key, and the third line, if present, is
  the session token.")

(defgeneric access-key-id (credentials)
  (:documentation
   "The access key id of CREDENTIALS."))

(defgeneric secret-access-key (credentials)
  (:documentation
   "The secret access key of CREDENTIALS."))

(defgeneric session-token (credentials)
  (:documentation
   "The session token of CREDENTIALS.")
  (:method (credentials)
    nil))

(defgeneric expiration (credentials)
  (:documentation
   "The expiration time, as a universal time, of CREDENTIALS.")
  (:method (credentials)
    (load-time-value
     (encode-universal-time 0 0 0 1 1 2100 0))))

(defgeneric expiredp (credentials)
  (:documentation
   "Returns T if CREDENTIALS have expired.")
  (:method (credentials)
    (< (expiration credentials) (get-universal-time))))


(defmethod access-key-id ((credentials cons))
  (first credentials))

(defmethod secret-access-key ((credentials cons))
  (second credentials))

(defmethod session-token ((credentials cons))
  (third credentials))


(defmethod access-key-id ((credentials pathname))
  (nth-line 0 credentials))

(defmethod secret-access-key ((credentials pathname))
  (nth-line 1 credentials))

(defmethod session-token ((credentials pathname))
  (nth-line 2 credentials nil))

(defmethod access-key-id ((credentials string))
  (access-key-id (pathname credentials)))

(defmethod secret-access-key ((credentials string))
  (secret-access-key (pathname credentials)))

(defmethod session-token ((credentials string))
  (session-token (pathname credentials)))

