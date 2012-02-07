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

;;;; package.lisp

(defpackage #:zaws
  (:use #:cl)
  (:shadow #:method)
  ;; Misc utility
  (:export #:octet
           #:octet-vector
           #:base64
           #:utf8
           #:aws-url-encode
           #:iso8601-timestamp
           #:rfc1123-timestamp
           #:parse-iso8601-timestamp
           #:make-parameters
           #:do-parameters)
  ;; Credentials
  (:export #:*credentials*
           #:access-key-id
           #:secret-access-key
           #:session-token
           #:expiration
           #:expiredp)
  ;; Utility for producing things to sign
  (:export #:sink-write
           #:with-octet-sink)
  ;; Canned authentication tools
  (:export #:query-auth-v2
           #:query-auth-v3
           #:json-auth-v3
           #:aws-auth-mixin
           #:sha256
           #:hmac-sha256-signature-base64
           #:prepare-for-signing
           #:sign
           #:string-to-sign
           #:vector-to-sign)
  ;; HTTP messages
  (:export #:content
           #:headers
           #:ensure-header
           #:delete-header
           #:uri)
  ;; HTTP responses
  (:export #:response
           #:status-code
           #:reason-phrase
           #:response-error
           #:response-error-request
           #:response-error-response)
  ;; HTTP requests
  (:export #:request
           #:protocol
           #:host
           #:method
           #:content-type
           #:parameters
           #:uri-path
           #:ensure-parameter
           #:delete-parameter
           #:submit
           #:check-response-error
           #:process-response)
  ;; Common query APIs
  (:export #:common-query-request
           #:action
           #:action-parameters
           #:api-version))

