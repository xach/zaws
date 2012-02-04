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

;;;; octet-sink.lisp

(in-package #:zaws)

(defvar *ascii-control-codes*
  #(:nul :soh :stx :etx :eot :enq :ack :bel
    :bs :ht :lf :vt :ff :cr :so :si
    :dle :dc1 :dc2 :dc3 :dc4
    :nak :syn :etb :can :em
    :sub :esc))

(defvar *ascii-control-code-values*
  (let ((table (make-hash-table)))
    (dotimes (i (length *ascii-control-codes*) table)
      (setf (gethash (aref *ascii-control-codes* i) table) i))))


(defun call-with-output-sink (fun)
  "Call FUN with one argument, an octet sink."
  (subseq
   (flexi-streams:with-output-to-sequence (out :element-type 'octet)
     (let ((stream (flexi-streams:make-flexi-stream out
                                                    :external-format :utf-8)))
       (funcall fun stream)))
   0))

(defgeneric sink-write (object sink)
  (:documentation "Write OBJECT to SINK."))

(defmethod sink-write ((value integer) sink)
  (check-type value octet)
  (write-byte value sink))

(defmethod sink-write ((values cons) sink)
  (map nil (lambda (value)
             (sink-write value sink))
       values))

(defmethod sink-write ((value character) sink)
  (write-char value sink))

(defmethod sink-write ((value symbol) sink)
  (check-type value keyword)
  (write-byte (gethash value *ascii-control-code-values*) sink))

(defmethod sink-write ((value string) sink)
  (write-string value sink))

(defmacro with-octet-sink ((sink) &body body)
  "Evaluate BODY with SINK bound to a newly created octet
sink. Returns the octet vector accumulated in SINK when finished."
  `(call-with-output-sink (lambda (,sink) ,@body)))
