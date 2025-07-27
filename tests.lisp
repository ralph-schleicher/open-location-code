;;; tests.lisp --- test procedure

;; Copyright (C) 2019 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :open-location-code-tests
  (:use :common-lisp
        :lisp-unit
        :iterate)
  (:export
   #:main))

(in-package :open-location-code-tests)

(defparameter *source-directory* (asdf:system-source-directory "open-location-code"))

(defun source-file (file-name)
  (uiop:merge-pathnames* file-name *source-directory*))

(defun float-equal* (a b)
  (if (or (floatp a) (floatp b))
      (float-equal a b)
    (= a b)))

(defun fixme ()
  (error "Should not happen."))

(defun boolean-from-string (string)
  (cond ((or (string-equal string "true")
             (string-equal string "yes")
             (string-equal string "on"))
         t)
        ((or (string-equal string "false")
             (string-equal string "no")
             (string-equal string "off"))
         nil)
        (t
         (fixme))))

(defun number-from-string (string)
  (with-input-from-string (stream string)
    (read-number:read-float stream t nil nil :float-format 'double-float)))

(define-test length-from-precision
  (assert-eql  2 (olc:code-length  1))
  (assert-eql  4 (olc:code-length  2))
  (assert-eql  6 (olc:code-length  3))
  (assert-eql  8 (olc:code-length  4))
  (assert-eql 10 (olc:code-length  5))
  (assert-eql 11 (olc:code-length  6))
  (assert-eql 12 (olc:code-length  7))
  (assert-eql 13 (olc:code-length  8))
  (assert-eql 14 (olc:code-length  9))
  (assert-eql 15 (olc:code-length 10))
  (assert-eql 15 (olc:code-length 11)))

(define-test precision-from-length
  (assert-eql  1 (olc:precision  2))
  (assert-eql  2 (olc:precision  4))
  (assert-eql  3 (olc:precision  6))
  (assert-eql  4 (olc:precision  8))
  (assert-eql  5 (olc:precision 10))
  (assert-eql  6 (olc:precision 11))
  (assert-eql  7 (olc:precision 12))
  (assert-eql  8 (olc:precision 13))
  (assert-eql  9 (olc:precision 14))
  (assert-eql 10 (olc:precision 15))
  (assert-eql 10 (olc:precision 16)))

(defun validity-test (code valid short full)
  (declare (ignore valid))
  (assert-eq (cond (short
                    :short)
                   (full
                    :full))
             (olc:validp code)
             code))

(define-test validity-tests
  (with-open-file (stream (source-file "t/validityTests.csv"))
    (iter (for row :in (cl-csv:read-csv stream))
          (for columns = (length row))
          (cond ((zerop columns)
                 (next-iteration))
                ((char= (aref (first row) 0) #\#)
                 (next-iteration))
                ((= columns 4)
                 (validity-test
                  (first row)
                  (boolean-from-string (second row))
                  (boolean-from-string (third row))
                  (boolean-from-string (fourth row))))
                (t (fixme))))))

;; Test encoding and decoding of plus codes.
(defun encoding-test (code len lat lon lat-int lon-int)
  (let ((prec (olc:precision (or len (olc:decode code)))))
    (assert-equal code (olc:encode lat lon prec) lat lon prec)
    ;; Likewise for integer location.
    (let ((lat (- (/ lat-int 25000000) 90))
          (lon (- (/ lon-int 8192000) 180)))
      (assert-equal code (olc:encode lat lon prec) lat-int lon-int prec))))

(defun %decoding-test (code len lat-low lon-low lat-high lon-high)
  (let ((area (olc:decode code)))
    (and (if len (= (olc:code-length (olc:precision area)) len) t)
         (multiple-value-bind (south west)
             (olc:south-west-corner area)
           (and (float-equal* lat-low south)
                (float-equal* lon-low west)))
         (multiple-value-bind (north east)
             (olc:north-east-corner area)
           (and (float-equal* lat-high north)
                (float-equal* lon-high east))))))

(defun decoding-test (code len lat-low lon-low lat-high lon-high)
  (assert-true
   (%decoding-test code len lat-low lon-low lat-high lon-high)))

(define-test encoding-tests
  (with-open-file (stream (source-file "t/encoding.csv"))
    (iter (for row :in (cl-csv:read-csv stream))
          (for columns = (length row))
          (cond ((zerop columns)
                 (next-iteration))
                ((char= (aref (first row) 0) #\#)
                 (next-iteration))
                ((= columns 6)
                 (encoding-test
                  (sixth row)
                  (number-from-string (fifth row))
                  (number-from-string (first row))
                  (number-from-string (second row))
                  (number-from-string (third row))
                  (number-from-string (fourth row))))
                (t (fixme))))))

(define-test decoding-tests
  (with-open-file (stream (source-file "t/decoding.csv"))
    (iter (for row :in (cl-csv:read-csv stream))
          (for columns = (length row))
          (cond ((zerop columns)
                 (next-iteration))
                ((char= (aref (first row) 0) #\#)
                 (next-iteration))
                ((= columns 6)
                 (decoding-test
                  (first row)
                  (number-from-string (second row))
                  (number-from-string (third row))
                  (number-from-string (fourth row))
                  (number-from-string (fifth row))
                  (number-from-string (sixth row))))
                (t (fixme))))))

;; Test shortening and extending codes.
(defun %short-code-test (full-code lat lon short-code test-type)
  (and (if (member test-type '(:both :shorten))
           (string= (olc:shorten full-code lat lon) short-code)
         t)
       (if (member test-type '(:both :recover))
           (string= (olc:recover short-code lat lon) full-code)
         t)))

(defun short-code-test (full-code lat lon short-code test-type)
  (assert-true
   (%short-code-test full-code lat lon short-code test-type)))

(define-test short-code-tests
  (with-open-file (stream (source-file "t/shortCodeTests.csv"))
    (iter (for row :in (cl-csv:read-csv stream))
          (for columns = (length row))
          (cond ((zerop columns)
                 (next-iteration))
                ((char= (aref (first row) 0) #\#)
                 (next-iteration))
                ((= columns 5)
                 (short-code-test
                  (first row)
                  (number-from-string (second row))
                  (number-from-string (third row))
                  (fourth row)
                  (let ((test-type (fifth row)))
                    (cond ((string= test-type "B") :both)
                          ((string= test-type "S") :shorten)
                          ((string= test-type "R") :recover)
                          (t (fixme))))))
                (t (fixme))))))

;; Entry point.
(defun main (&optional (tests :all))
  (let ((lisp-unit:*print-errors* t)
        (lisp-unit:*print-failures* t)
        (lisp-unit:*print-summary* t)
        (lisp-unit:*epsilon* 1D-10))
    (run-tests tests :open-location-code-tests)))

;;; tests.lisp ends here
