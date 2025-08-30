;;; read-code.lisp --- reading Plus Codes

;; Copyright (C) 2025 Ralph Schleicher

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

;;; Commentary:

;;; Code:

(in-package :open-location-code)

(defun read-code (&optional
		    (stream *standard-input*)
		    (eof-error-p t)
		    eof-value
		    recursivep
		  &key
		    short-code)
  "Read a Plus Code from an input stream.

Optional first argument STREAM is an input stream designator.
 The default is standard input.
Optional second argument EOF-ERROR-P is a generalized boolean.
 If an end of file error occurs and EOF-ERROR-P is false, EOF-VALUE
 is returned.  Otherwise, signal an ‘end-of-file’ error.  The default
 is true.
Optional third argument EOF-VALUE is an object.  See above for more
 details.  The default is ‘nil’.
Optional fourth argument RECURSIVEP is a generalized boolean.  True
 means that this call is expected to be embedded in a higher-level
 call to ‘read’ or a similar function used by the Lisp reader.  The
 default is false.
Keyword argument SHORT-CODE specifies the Plus Code format.  A value
 of ‘nil’ means to read a full Plus Code, ‘t’ means to read a short
 Plus Code, and any other value means to read any valid Plus Code.
 The default is ‘nil’.

Return value is the Plus Code (a string), or ‘nil’ if no valid Plus
Code in the specified format can be read.  Secondary value is the
number of characters read from STREAM.

The ‘read-code’ function expects a Plus Code in the specified format.
Leading or trailing whitespace is not ignored.  Reading stops at the
end of the file or if an invalid character occurs.  That means, the
next character read from STREAM is the invalid character.

If the file ends in the middle of a Plus Code, ‘read-code’ signals
an ‘end-of-file’ error regardless of the value of the EOF-ERROR-P
argument."
  (let ((buffer (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
        char ;last character read, ‘nil’ means end of file reached
        plus ;position of the separator character
        (pad 0) ;number of pad characters
        (length 0) ;number of digits
        value ;decimal value of a digit
        code) ;return value
    (iter (for pos :from 0)
          (setf char (read-char stream nil nil recursivep))
          (cond ((null char)
                 (finish))
                ((setf value (digitp char))
                 ;; Valid characters can only occur before pad
                 ;; characters.
                 (when (plusp pad)
                   (leave))
                 ;; The separator character must occur latest after
                 ;; the sixth or eighth character.
                 (when (and (not plus) (>= pos (if (eq short-code t) 6 8)))
                   (leave))
                 ;; For a full code, the first two characters
                 ;; must be in the proper range.
                 (when (and (eq short-code nil)
                            (or (and (= pos 0) (>= value 9))
                                (and (= pos 1) (>= value 18))))
                   (leave))
                 ;; Update code length.
                 (incf length))
                ((char= char #\0)
                 ;; Pad characters can only occur in full codes before
                 ;; the separator character and after an even number
                 ;; of digits.
                 (when (or plus (oddp length) (< length 2) (>= pos 8) (eq short-code t))
                   (leave))
                 (incf pad))
                ((char= char #\+)
                 ;; There can only be one separator character and it
                 ;; must occur at the correct position.
                 (when (or plus (oddp pos) (cond ((eq short-code nil)
                                                  (/= pos 8))
                                                 ((eq short-code t)
                                                  (or (> pos 6) (plusp pad)))
                                                 (t ;full or short code
                                                  (if (plusp pad)
                                                      (/= pos 8)
                                                    (> pos 8)))))
                   (leave))
                 (setf plus pos))
                (t
                 ;; Invalid character.
                 (finish)))
          ;; Smells like a Plus Code.
          (vector-push-extend char buffer)
          (finally
           (setf code (and plus
                           ;; The separator character must occur at
                           ;; the correct position.
                           (and (evenp plus)
                                (cond ((eq short-code nil)
                                       (= plus 8))
                                      ((eq short-code t)
                                       (< plus 8))
                                      (t
                                       (<= plus 8))))
                           ;; There must be an even number of pad
                           ;; characters.  Pad characters can only
                           ;; occur in full codes.
                           (or (zerop pad)
                               (and (evenp pad)
                                    (= plus 8)))
                           ;; Code length must be at least two.
                           (>= length 2)
                           ;; Code length is either less than or equal
                           ;; to the position of the separator character,
                           ;; i.e. the separator character is the last
                           ;; character, or there are two or more valid
                           ;; characters after the separator character.
                           (or (<= length plus)
                               (>= (- length plus) 2))
                           ;; For a full code, the first two characters
                           ;; must be in the proper range.
                           (or (/= plus 8)
                               (and (< (digitp (aref buffer 0)) 9)
                                    (< (digitp (aref buffer 1)) 18)))
                           ;; Code is valid.
                           (coerce buffer 'simple-string))))
          ())
    ;; Done.
    (when char
      (unread-char char stream))
    ;; Return the resulting code, or signal an error.
    (values (if (or code char)
                ;; If CODE is ‘nil’ and CHAR is not ‘nil’, then it is
                ;; no end of file condition.
                code
              ;; Always signal an end of file error if the file ends
              ;; in the middle of an object.
              (if (or eof-error-p (plusp (length buffer)))
	          (error 'end-of-file :stream stream)
                eof-value))
            (length buffer))))

;;; read-code.lisp ends here
