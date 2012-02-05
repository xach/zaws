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

;;;; authentication.lisp

(in-package #:zaws)

(defclass aws-auth-mixin ()
  ()
  (:documentation
   "Any class that uses this mixin will be automatically signed before
   submission."))

(defgeneric prepare-for-signing (request)
  (:documentation
   "Prepare REQUEST for signing by adding or converting any
   information that needs to be available for the signing
   process. Returns the REQUST object.")
  (:method (request)
    request))

(defgeneric sign (request)
  (:documentation
   "Add authentication information to REQUEST based on the current
   value of *CREDENTIALS* and the contents of the request. Return the
   REQUEST object.")
  (:method (request)
    request))

(defgeneric string-to-sign (request)
  (:documentation
   "The string to sign for authentication.")
  (:method (request)
    ""))

(defgeneric vector-to-sign (request)
  (:documentation
   "The vector to sign for authentication; should be used in
   preference to creating a string to sign if the string includes
   ASCII control codes like LF; otherwise defaults to an UTF-8
   encoding of the string to sign.")
  (:method (request)
    (utf8 (string-to-sign request))))

(defmethod sign :before ((request aws-auth-mixin))
  (prepare-for-signing request))

(defmethod submit :before ((request aws-auth-mixin))
  (sign request))




;;; Query Auth V2

(defclass query-auth-v2 (aws-auth-mixin)
  ((signature-version
    :initarg :signature-version
    :reader signature-version))
  (:default-initargs
   :signature-version "2")
  (:documentation
   "Adding this class as a mixin to a request will automatically sign
   requests per the AWS Query Auth version 2 spec."))

(defun parameters-as-auth-string (parameters)
  "Convert the alist PARAMETERS to a string suitable for use in
authentication strings-to-sign."
  (flet ((one-pair (pair)
           (format nil "~A=~A"
                   (aws-url-encode (car pair))
                   (aws-url-encode (cdr pair)))))
    (format nil "~{~A~^&~}"
            (mapcar #'one-pair
                    (sort (copy-list parameters)
                          #'string<
                          :key 'car)))))

(defmethod prepare-for-signing ((request query-auth-v2))
  (ensure-parameter "AWSAccessKeyId" (access-key-id *credentials*) request)
  (ensure-parameter "SignatureVersion" (signature-version request) request)
  (ensure-parameter "SignatureMethod" "HmacSHA256" request)
  (ensure-parameter "Timestamp" (iso8601-timestamp) request)
  (delete-parameter "Signature" request)
  (let ((token (session-token *credentials*)))
    (when token
      (ensure-parameter "SecurityToken" token request))))

(defmethod vector-to-sign ((request query-auth-v2))
  (with-octet-sink (sink)
    (sink-write (list (string (method request)) :lf
                      (string-downcase (host request)) :lf
                      (uri-path request) :lf
                      (parameters-as-auth-string (parameters request)))
                sink)))

(defun hmac-sha256-signature-base64 (secret-key vector)
  "Return a base64-encoded string of the SHA256-HMAC of VECTOR with
the UTF-8 encoded octets of the string SECRET-KEY."
  (let ((hmac (ironclad:make-hmac (utf8 secret-key) :sha256)))
    (ironclad:update-hmac hmac vector)
    (base64 (ironclad:hmac-digest hmac))))

(defmethod sign ((request query-auth-v2))
  (ensure-parameter "Signature"
                    (hmac-sha256-signature-base64 (secret-access-key
                                                   *credentials*)
                                                  (vector-to-sign request))
                    request)
  request)


;;; Query Auth V3; just signs the Date header

(defclass query-auth-v3 (aws-auth-mixin) ())

(defmethod prepare-for-signing ((request query-auth-v3))
  (let ((token (session-token *credentials*)))
    (when token
      (ensure-header "x-amz-security-token" token request)))
  (ensure-header "x-amz-date" (rfc1123-timestamp) request))

(defmethod string-to-sign ((request query-auth-v3))
  (header-value "x-amz-date" request))

(defmethod sign ((request query-auth-v3))
  (ensure-header "x-amzn-authorization"
                 (format nil "AWS3-HTTPS AWSAccessKeyId=~A,~
                              Algorithm=HmacSHA256,~
                              Signature=~A"
                         (access-key-id *credentials*)
                         (hmac-sha256-signature-base64
                          (secret-access-key *credentials*)
                          (vector-to-sign request)))
                 request)
  request)

;;; JSON Auth V3


(defclass json-auth-v3 (aws-auth-mixin) ())

(defun sha256 (vector)
  (ironclad:digest-sequence :sha256 vector))

(defmethod prepare-for-signing ((request json-auth-v3))
  (let ((token (session-token *credentials*)))
    (unless token
      (error "No session token set in ~S"
             '*credentials*))
    (ensure-header "x-amz-date" (rfc1123-timestamp) request)
    (ensure-header "x-amz-security-token" token request)))

(defun json-auth-v3-headers (request)
  (let ((headers (sort (copy-list
                        (cons (cons "host" (host request))
                              (headers request)))
                       #'string-lessp
                       :key 'car)))
    (loop for (key . value) in headers
          for canon-key = (string-downcase key)
          when (or (string= canon-key "host")
                   (starts-with "x-amz-" canon-key))
          collect (format nil "~A:~A" canon-key value))))

(defmethod vector-to-sign ((request json-auth-v3))
  (with-octet-sink (sink)
    (sink-write (list (string (method request)) :lf
                      (uri-path request) :lf
                      (parameters-as-auth-string (parameters request)) :lf)
                sink)
    (dolist (header (json-auth-v3-headers request))
      (sink-write (list header :lf)
                  sink))
    (sink-write (list :lf (content request)) sink)))

(defmethod sign ((request json-auth-v3))
  (ensure-header
   "x-amzn-authorization"
   (format nil "AWS3 AWSAccessKeyId=~A,Algorithm=HMACSha256,Signature=~A"
           (access-key-id *credentials*)
           (hmac-sha256-signature-base64 (secret-access-key *credentials*)
                                         (sha256 (vector-to-sign request))))
   request)
  request)
