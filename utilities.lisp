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

;;;; utilities.lisp

(in-package #:zaws)

(deftype octet ()
  '(unsigned-byte 8))

(defun octet-vector (&rest octets)
  "Return a specialized array initialized with OCTETS."
  (make-array (length octets)
              :element-type 'octet
              :initial-contents octets))

(defun utf8 (string)
  "Return STRING encoded as UTF-8 octets."
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun base64 (vector)
  "Encode VECTOR as a base64 string."
  (base64:usb8-array-to-base64-string vector))

(defvar *unreserved-octets*
  ;; A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ),
  ;; and tilde ( ~ ).
  (let ((index 0)
        (unreserved
         (concatenate 'string
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      "abcdefghijklmnopqrstuvwxyz"
                      "0123456789"
                      "-_.~")))
    (map-into (make-array 256 :element-type 'bit :initial-element 0)
              (lambda ()
                (prog1
                    (if (position (code-char index) unreserved)
                        1
                        0)
                  (incf index))))))

(defun aws-url-encode (string)
  "URL-encode STRING according to AWS's requirements."
  (with-output-to-string (stream)
    (let ((octets (utf8 string))
          (*print-pretty* nil))
      (map nil
           (lambda (octet)
             (if (plusp (bit *unreserved-octets* octet))
                 (write-char (code-char octet) stream)
                 (format stream "%~2,'0X" octet)))
           octets))))

(defun iso8601-timestamp (&optional (time (get-universal-time)))
  "Return an ISO 8601-style UTC timestamp string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

(defun parse-iso8601-timestamp (timestamp)
  "Parse an ISO 8601-style UTC timestamp and return a universal time
and fraction of a second as multiple values."
  (flet ((number-at (start length)
           (parse-integer timestamp :start start :end (+ start length))))
    (let ((year (number-at 0 4))
          (month (number-at 5 2))
          (day (number-at 8 2))
          (hour (number-at 11 2))
          (minute (number-at 14 2))
          (second (number-at 17 2)))
      (let* ((fraction-start (position #\. timestamp :from-end t))
             (fraction (if fraction-start
                           (or (parse-integer timestamp
                                              :start (1+ fraction-start)
                                              :junk-allowed t)
                               0)
                           0)))
        (values
         (encode-universal-time second minute hour day month year 0)
         fraction)))))

(defun rfc1123-timestamp (&optional (time (get-universal-time)))
  "Return an RFC 1123-style GMT timestamp string."
  (let ((days (load-time-value #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
        (months (load-time-value #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))))
    (multiple-value-bind (sec min hour day month year day-of-week)
        (decode-universal-time time 0)
      (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
              (aref days day-of-week)
              day
              (aref months (1- month))
              year
              hour min sec))))

(defun make-parameters (&rest keys-and-values)
  "Convert KEYS-AND-VALUES to an alist suitable for using as
parameters or headers in an HTTP request. Keys and values are coerced
to strings, if necessary."
  (when (oddp (length keys-and-values))
    (error "Odd number of keys and values"))
  (flet ((stringize (object)
           (typecase object
             (string
              object)
             (symbol
              (string-downcase object))
             (character
              (string object))
             (t
              (write-to-string object)))))
    (loop for (key value) on keys-and-values by #'cddr
          collect (cons (stringize key) (stringize value)))))


;;; Unexported, internal-use stuff

(defun starts-with (prefix string)
  "Return T if STRING starts with PREFIX."
  (let ((end1 (length prefix))
        (end2 (length string)))
    (and (<= end1 end2)
         (string= prefix string :end2 end1))))

(defun nth-line (n file &optional (errorp t))
  (with-open-file (stream file)
    (let (result)
      (dotimes (i (1+ n) result)
        (setf result (read-line stream errorp))))))
