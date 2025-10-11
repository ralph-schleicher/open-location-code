;;; open-location-code.lisp --- Open Location Code library

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

(in-package :open-location-code)

(define-condition code-error (type-error)
  ()
  (:documentation
   "Base class for all Open Location Code errors."))

(define-condition precision-error (code-error)
  ()
  (:documentation
   "Condition for an invalid Plus Code precision.")
  (:report
   (lambda (condition stream)
     (format stream
             "The value ‘~S’ is not a valid Plus Code precision."
             (type-error-datum condition)))))

(define-condition code-length-error (code-error)
  ()
  (:documentation
   "Condition for an invalid Plus Code length.")
  (:report
   (lambda (condition stream)
     (format stream
             "The value ‘~S’ is not a valid Plus Code length."
             (type-error-datum condition)))))

(define-condition invalid-code-error (code-error)
  ()
  (:documentation
   "Condition for an invalid Plus Code.")
  (:report
   (lambda (condition stream)
     (format stream
             "The value ‘~S’ is not a valid Plus Code."
             (type-error-datum condition)))))

(define-condition full-code-error (invalid-code-error)
  ()
  (:documentation
   "Condition for an invalid full Plus Code.")
  (:report
   (lambda (condition stream)
     (format stream
             "The value ‘~S’ is not a full Plus Code."
             (type-error-datum condition)))))

(define-condition short-code-error (invalid-code-error)
  ()
  (:documentation
   "Condition for an invalid short Plus Code.")
  (:report
   (lambda (condition stream)
     (format stream
             "The value ‘~S’ is not a short Plus Code."
             (type-error-datum condition)))))

(defconst +maximum-precision+ 10
  "Maximum number of discretization steps.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun length-from-precision (precision)
    "Return the code length as a function of the precision."
    (let ((prec (min precision +maximum-precision+)))
      (cond ((> prec 5)
             (+ 10 (- prec 5)))
            ((plusp prec)
             (* prec 2))
            (t
             (error 'precision-error :datum precision))))))

(defconst +maximum-length+ (length-from-precision +maximum-precision+)
  "Maximum code length excluding the plus sign character.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun precision-from-length (length)
    "Return the precision as a function of the code length."
    (let ((len (min length +maximum-length+)))
      (cond ((> len 10)
             (+ 5 (- len 10)))
            ((and (plusp len) (evenp len))
             (/ len 2))
            (t
             (error 'code-length-error :datum length))))))

(defconst +alphabet+ "23456789CFGHJMPQRVWX"
  "Set of valid digits, i.e. encoding characters.
The character position is equal to the decimal
value, i.e. weight, of the digit.")

(defsubst digitp (char)
  "True if CHAR is a valid digit.

Argument CHAR has to be a character.

Value is the decimal value, i.e. weight, of the digit,
or ‘nil’ if CHAR is an invalid digit."
  ;; Decoding must be case insensitive.
  (position char +alphabet+ :test #'char-equal))

(defsubst digit (value)
  "Return the digit for a decimal value.

Argument VALUE has to be a non-negative integer less than 20.

Value is the digit, i.e. encoding character."
  (aref +alphabet+ value))

(defconst +block-divisor+ 20
  "Divisor when splitting a block into sub-blocks.")

(defconst +grid-rows+ 5
  "Number of grid rows.")

(defconst +grid-columns+ 4
  "Number of grid columns.")

(defconst +area-height+
  (iter (with height = 400)
        (for prec :from 0 :to +maximum-precision+)
        (collect height :result-type 'vector)
        (setf height (/ height (if (< prec 5) +block-divisor+ +grid-rows+))))
  "Vector of code area heights in descending order.
Element position is the precision of the code area.")

(defconst +area-width+
  (iter (with width = 400)
        (for prec :from 0 :to +maximum-precision+)
        (collect width :result-type 'vector)
        (setf width (/ width (if (< prec 5) +block-divisor+ +grid-columns+))))
  "Vector of code area widths in descending order.
Element position is the precision of the code area.")

(defsubst area-size (precision)
  "Return the height and width of a code area as multiple values.

Argument PRECISION is the number of discretization steps."
  (values (svref +area-height+ precision)
          (svref +area-width+ precision)))

(defun integer-from-degree (latitude longitude)
  "Convert a location into non-negative integers."
  (let* ((lat-scale (/ (svref +area-height+ +maximum-precision+)))
         (lat-limit (* 180 lat-scale))
         (lat (+ (floor (* latitude lat-scale)) (/ lat-limit 2)))
         (lon-scale (/ (svref +area-width+ +maximum-precision+)))
         (lon-limit (* 360 lon-scale))
         (lon (+ (floor (* longitude lon-scale)) (/ lon-limit 2))))
    ;; Clip the latitude to the half-closed interval [0°, 180°).
    (setf lat (alexandria:clamp lat 0 (1- lat-limit)))
    ;; Normalise the longitude to the half-closed interval [0°, 360°).
    (setf lon (mod lon lon-limit))
    ;; Return values.
    (values lat lon)))

(defun normalize-location (latitude longitude precision)
  "Clip LATITUDE to the half-closed interval [-90°, 90°) and
normalize LONGITUDE to the half-closed interval [-180°, 180°).
Third argument PRECISION is used to determine the height of
 the code area if LATITUDE denotes the north pole.

Values are latitude, longitude, and the actual precision."
  (declare (type real latitude longitude)
           (type (integer 1) precision))
  (let ((lat (rationalize latitude))
        (lon (rationalize longitude))
        (prec (min precision +maximum-precision+)))
    ;; Clip the latitude to the closed interval [-90°, 90°].
    (setf lat (alexandria:clamp lat -90 90))
    ;; Ensure that the represented area does not exceed 90° latitude.
    (when (= lat 90)
      (decf lat (/ (svref +area-height+ prec) 2)))
    ;; Normalise the longitude to the half-closed interval [-180°, 180°).
    (setf lon (rem lon 360))
    (cond ((< lon -180)
           (incf lon 360))
          ((>= lon 180)
           (decf lon 360)))
    ;; Return values.
    (values lat lon prec)))

(defun normalize-location* (latitude longitude precision)
  "Like ‘normalize-location’ but return latitude and longitude
as non-negative numbers, i.e. add 90° to the latitude and 180°
to the longitude."
  (multiple-value-bind (lat lon prec)
      (normalize-location latitude longitude precision)
    ;; Add 90° to the latitude.
    (incf lat 90)
    ;; Add 180° to the longitude.
    (incf lon 180)
    ;; Return values.
    (values lat lon prec)))

(defun analyse (code start end &optional area)
  "Analyse a Plus Code.

First argument CODE is the Plus Code (an object).
Second argument START is the lower bounding index of CODE.
Third argument END is the upper bounding index of CODE.
If optional fourth argument AREA is not ‘nil’, secondary value
 is a code area object.

Primary value is ‘:full’ or ‘:short’ if CODE is a valid full or short
Plus Code respectively.  Otherwise, all values are ‘nil’."
  (let (valid object)
    (typecase code
      (string
       (when (null end)
         (setf end (length code)))
       (iter (with south = 0)
             (with west = 0)
             (with height = 0)
             (with width = 0)
             (with plus)
             (with pad = 0)
             (with length = 0)
             ;; Decimal value of a digit.
             (with value)
             ;; Discretization step of the next digit.
             ;; Also check for the correct position of
             ;; the separator character.
             (with prec = (case (alexandria:when-let ((pos (position #\+ code :start start :end end :test #'char=)))
                                  (- pos start))
                            (8 1) (6 2) (4 3) (2 4) (0 5) (t 0)))
             (initially
              (when (= prec 0)
                (return)))
             (for pos :from start :below end)
             (for char = (aref code pos))
             (cond ((setf value (digitp char))
                    ;; Valid characters can only occur before pad
                    ;; characters.
                    (when (plusp pad)
                      (leave))
                    ;; Update code area.
                    (when (not (null area))
                      (cond ((> prec +maximum-precision+)
                             ;; No further refinement but continue
                             ;; parsing CODE.
                             t)
                            ((> prec 5)
                             (multiple-value-setq (height width)
                               (area-size prec))
                             ;; Map VALUE to row and column.
                             (multiple-value-bind (row column)
                                 (truncate value +grid-columns+)
                               (incf south (* row height))
                               (incf west (* column width)))
                             (incf prec))
                            ((evenp length)
                             ;; Start a new pair.
                             (multiple-value-setq (height width)
                               (area-size prec))
                             ;; First digit encodes the latitude.
                             (incf south (* value height)))
                            (t
                             ;; Second digit encodes the longitude.
                             (incf west (* value width))
                             ;; Next discretization step.
                             (incf prec))))
                    ;; Update code length.
                    (incf length))
                   ((char= char #\0)
                    ;; Pad characters can only occur before the
                    ;; separator character.
                    (when plus
                      (leave))
                    (incf pad))
                   ((char= char #\+)
                    ;; There can only be one separator character
                    ;; and it must occur at the correct position.
                    ;; The later already has been checked in the
                    ;; prologue section.
                    (when plus
                      (leave))
                    (setf plus (- pos start)))
                   (t
                    ;; Invalid character.
                    (leave)))
             (finally
              (setf valid (and plus
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
                                   (and (< (digitp (aref code (+ start 0))) 9)
                                        (< (digitp (aref code (+ start 1))) 18)))
                               ;; Code is valid.
                               (if (= plus 8) :full :short)))
              (when (and valid area)
                (when (= plus 8)
                  (decf south 90)
                  (decf west 180))
                (setf object (make-code-area :south south
                                             :west west
                                             :height height
                                             :width width
                                             :precision (1- prec)
                                             :plus plus
                                             :pad pad))))
             ()))
      (code-area
       (when (plusp (code-area-precision code))
         (setf valid (if (= (code-area-plus code) 8) :full :short)))
       (when (and valid area)
         (setf object code))
       ()))
    (values valid object)))

;;; open-location-code.lisp ends here
