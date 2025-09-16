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

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

(define-condition code-error (type-error)
  ()
  (:documentation
   "Base class for all Open Location Code errors."))

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
      (+ (* (min prec 5) 2) (max 0 (- prec 5))))))

(defconst +maximum-length+ (length-from-precision +maximum-precision+)
  "Maximum code length excluding the plus sign character.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun precision-from-length (length)
    "Return the precision as a function of the code length."
    (let ((len (min length +maximum-length+)))
      (cond ((member len '(2 4 6 8 10))
             (/ len 2))
            ((> len 10)
             (+ 5 (- len 10)))
            (t
             (error 'code-length-error :datum len))))))

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

;; If slot value PLUS is equal to eight, the original code was a
;; full Plus Code and slot values SOUTH and WEST denote an absolute
;; location.  Otherwise, the original code was a short Plus Code and
;; slot values SOUTH and WEST denote a relative location, i.e. the
;; offset to the reference location.
(defstruct code-area
  "Area covered by a Plus Code."
  ;; Lower latitude of the code area in degree angle.
  (south 0 :type real :read-only t)
  ;; Lower longitude of the code area in degree angle.
  (west 0 :type real :read-only t)
  ;; Height of the code area in degree angle.
  (height 0 :type real :read-only t)
  ;; Width of the code area in degree angle.
  (width 0 :type real :read-only t)
  ;; Precision of the code area.
  (precision 0 :type fixnum :read-only t)
  ;; Position of the separator character in the Plus Code.
  (plus 0 :type fixnum :read-only t)
  ;; Number of pad characters in the Plus Code.
  (pad 0 :type fixnum :read-only t))

(defun south-west-corner (object)
  "Return the lower latitude and longitude as multiple values."
  (check-type object code-area)
  (values (code-area-south object)
          (code-area-west object)))

(defun north-east-corner (object)
  "Return the center latitude and longitude as multiple values."
  (check-type object code-area)
  (values (+ (code-area-south object) (code-area-height object))
          (+ (code-area-west object) (code-area-width object))))

(defun center (object)
  "Return the center latitude and longitude as multiple values."
  (check-type object code-area)
  (values (+ (code-area-south object) (/ (code-area-height object) 2))
          (+ (code-area-west object) (/ (code-area-width object) 2))))

(defgeneric precision (object)
  (:documentation
   "Return the Plus Code precision."))

(defmethod precision ((object code-area))
  "Return the precision of the code area."
  (code-area-precision object))

(defmethod precision ((length integer))
  "Return the precision as a function of the code length."
  (check-type length (integer 2))
  (precision-from-length length))

(defgeneric code-length (object)
  (:documentation
   "Return the Plus Code length."))

(defmethod code-length ((object code-area))
  "Return the code length for the code area."
  (code-length (precision object)))

(defmethod code-length ((precision integer))
  "Return the code length as a function of the precision."
  (check-type precision (integer 1))
  (length-from-precision precision))

(defgeneric separator-position (object)
  (:documentation
   "Return the position of the separator character."))

(defmethod separator-position ((object code-area))
  "Return the position of the separator character in the Plus Code."
  (code-area-plus object))

(defgeneric pad-characters (object)
  (:documentation
   "Return the number of pad characters."))

(defmethod pad-characters ((object code-area))
  "Return the number of pad characters in the Plus Code."
  (code-area-pad object))

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

(defun validp (code &key (start 0) end)
  "True if an object is a valid sequence of Plus Code characters.

Argument CODE is an object of any type.
Keyword arguments START and END are the bounding indices of CODE.
 Default is zero and the length of CODE respectively.

Value is ‘:full’ or ‘:short’ if CODE is a valid full or short Plus
Code respectively.  Otherwise, value is ‘nil’."
  (check-type start alexandria:array-index)
  (check-type end (or null alexandria:array-index))
  (nth-value 0 (analyse code start end)))

(defun fullp (code &key (start 0) end)
  "True if an object is a valid full Plus Code.

Argument CODE is an object of any type.
Keyword arguments START and END are the bounding indices of CODE.
 Default is zero and the length of CODE respectively.

Value is true if CODE is a valid full Plus Code.
Otherwise, value is ‘nil’."
  (check-type start alexandria:array-index)
  (check-type end (or null alexandria:array-index))
  (eq (analyse code start end) :full))

(defun shortp (code &key (start 0) end)
  "True if an object is a valid short Plus Code.

Argument CODE is an object of any type.
Keyword arguments START and END are the bounding indices of CODE.
 Default is zero and the length of CODE respectively.

Value is true if CODE is a valid full Plus Code.
Otherwise, value is ‘nil’."
  (check-type start alexandria:array-index)
  (check-type end (or null alexandria:array-index))
  (eq (analyse code start end) :short))

(defun encode (latitude longitude &optional (precision 5))
  "Encode a location into a Plus Code.

First argument LATITUDE and second argument LONGITUDE denote the
 location in degree angle.  The latitude is clipped to the closed
 interval [-90, 90] and the longitude is normalized to the
 half-closed interval [-180, 180).
Optional third argument PRECISION is the precision of the code.
 Default is five, i.e. a code length of ten digits.

Value is a full Plus Code (a string).

The relation between precision, code length, and code area size
is depicted in the following table.

   Precision  | Code Length |   Width / m   |  Height / m
 -------------+-------------+---------------+---------------
       1      |      2      | 2218929.9     | 2218929.9
       2      |      4      |  110946.5     |  110946.5
       3      |      6      |    5547.3     |    5547.3
       4      |      8      |     277.4     |     277.4
       5      |     10      |      13.9     |      13.9
       6      |     11      |       3.47    |       2.77
       7      |     12      |       0.867   |       0.555
       8      |     13      |       0.217   |       0.111
       9      |     14      |       0.0542  |       0.0222
      10      |     15      |       0.0135  |       0.00444

The code length is equal to the number of Plus Code digits.  Pad
characters and the separator character ‘+’ are not part of the code
length.  The code area dimensions are calculated with a mean earth
radius of 6356766 m for a code area at the equator."
  (check-type latitude real)
  (check-type longitude real)
  (check-type precision (integer 1))
  (multiple-value-bind (lat lon prec)
      (normalize-location* latitude longitude precision)
    ;; Do the encoding.
    (let ((code (make-string (+ 9 (if (> prec 4) 2 0) (max 0 (- prec 5))) :initial-element #\0)))
      (setf (aref code 8) #\+)
      (let ((height 0)
            (width 0)
            (index 1)
            (pos 0))
        (labels ((pair ()
                   ;; Query area size.
                   (multiple-value-setq (height width)
                     (area-size index))
                   (incf index)
                   (let (value)
                     (multiple-value-setq (value lat)
                       (truncate lat height))
                     (setf (aref code pos) (digit value))
                     (incf pos)
                     (multiple-value-setq (value lon)
                       (truncate lon width))
                     (setf (aref code pos) (digit value))
                     (incf pos))))
          (case prec
            (1
             (pair))
            (2
             (pair) (pair))
            (3
             (pair) (pair) (pair))
            (4
             (pair) (pair) (pair) (pair))
            (t
             (pair) (pair) (pair) (pair) (incf pos) (pair)
             ;; Refinement steps.
             (let (row column)
               (iter (repeat (- prec 5))
                     (multiple-value-setq (height width)
                       (area-size index))
                     (incf index)
                     (multiple-value-setq (row lat)
                       (truncate lat height))
                     (multiple-value-setq (column lon)
                       (truncate lon width))
                     (let ((value (+ (* row +grid-columns+) column)))
                       (setf (aref code pos) (digit value))
                       (incf pos))))))))
      code)))

(defun decode (code &key (start 0) end)
  "Decode a Plus Code.

Argument CODE is a Plus Code (a string).
Keyword arguments START and END are the bounding indices of CODE.
 Default is zero and the length of CODE respectively.

Primary value is a ‘code-area’ object.  Secondary value is ‘:full’
or ‘:short’ if CODE is a full or short Plus Code respectively.  If
CODE is a full Plus Code the code area denotes absolute coordinates.
Otherwise, the code area denotes relative coordinates, i.e. offset
values in the enclosing block.

Signal an ‘invalid-code-error’ if CODE is not a valid Plus Code."
  (check-type code string)
  (check-type start alexandria:array-index)
  (check-type end (or null alexandria:array-index))
  (multiple-value-bind (valid area)
      (analyse code start end t)
    (when (not valid)
      (error 'invalid-code-error :datum code))
    (values area valid)))

(defun shorten (code latitude longitude &key (start 0) end)
  "Remove four, six, or eight digits from the front of a full Plus
Code given a reference location.

First argument CODE is a full Plus Code (a string).
Second argument LATITUDE and third argument LONGITUDE denote the
 reference location in degree angle.  The latitude is clipped to
 the closed interval [-90, 90] and the longitude is normalized to
 the half-closed interval [-180, 180).
Keyword arguments START and END are the bounding indices of CODE.
 Default is zero and the length of CODE respectively.

Value is the short code, or the original full code if the reference
location is too far.

Signal a ‘full-code-error’ if CODE is not a full Plus Code."
  (check-type code string)
  (check-type latitude real)
  (check-type longitude real)
  (check-type start alexandria:array-index)
  (check-type end (or null alexandria:array-index))
  (multiple-value-bind (valid area)
      (analyse code start end t)
    (unless (eq valid :full)
      (error 'full-code-error :datum code))
    (let (lat lon ref-lat ref-lon distance)
      (multiple-value-setq (lat lon)
        (center area))
      (multiple-value-setq (ref-lat ref-lon)
        (normalize-location latitude longitude (precision area)))
      (setf distance (max (abs (- lat ref-lat))
                          (abs (- lon ref-lon))))
      (iter (for pos :from (min (code-length area) 8) :downto 4 :by 2)
            ;; Check for pad characters.
            (when (char= (aref code pos) #\0)
              (next-iteration))
            ;; Code area is square.
            (for block-size = (area-size (/ pos 2)))
            ;; Factor 0.3 adds just an extra margin of safety;
            ;; theoretically half the block size is sufficient.
            (when (< distance (* block-size 3/10))
              (return-from shorten (subseq code pos)))))
    ;; Return original full Plus Code.
    code))

(defun recover (code latitude longitude &key (start 0) end)
  "Recover a full Plus Code from a short code and a reference location.

First argument CODE is a Plus Code (a string).
Second argument LATITUDE and third argument LONGITUDE denote the
 reference location in degree angle.  The latitude is clipped to
 the closed interval [-90, 90] and the longitude is normalized to
 the half-closed interval [-180, 180).
Keyword arguments START and END are the bounding indices of CODE.
 Default is zero and the length of CODE respectively.

Value is the recovered full code.  If CODE is already a full code,
return CODE as is.

Signal an ‘invalid-code-error’ if CODE is not a Plus Code."
  (check-type code string)
  (check-type latitude real)
  (check-type longitude real)
  (check-type start alexandria:array-index)
  (check-type end (or null alexandria:array-index))
  (multiple-value-bind (valid area)
      (analyse code start end t)
    (unless valid
      (error 'invalid-code-error :datum code))
    (if (eq valid :full)
        ;; A recovered code contains only uppercase letters.
        (if (some #'lower-case-p code)
            (string-upcase code)
          code)
      (let (pad prec lat lon ref-lat ref-lon height width)
        ;; Number of digits to recover and the corresponding precision.
        (setf pad (- 8 (separator-position area))
              prec (/ pad 2))
        ;; Relative location of the code area.
        (multiple-value-setq (lat lon)
          (center area))
        ;; Generate the prefix from the reference location and combine
        ;; it with the short code.
        (multiple-value-setq (ref-lat ref-lon)
          (normalize-location* latitude longitude prec))
        (multiple-value-setq (height width)
          (area-size prec))
        (incf lat (* (floor ref-lat height) height))
        (incf lon (* (floor ref-lon width) width))
        ;; Check if the recovered code area is too far from the
        ;; reference location.  If so, move it.
        (let ((distance (- lat ref-lat))
              (half-height (/ height 2)))
          (cond ((and (> distance half-height)
                      (> (- lat height) 0))
                 (decf lat height))
                ((and (< distance (- half-height))
                      (< (+ lat height) 180))
                 (incf lat height))))
        (let ((distance (- lon ref-lon))
              (half-width (/ width 2)))
          (cond ((and (> distance half-width)
                      (> (- lon width) 0))
                 (decf lon width))
                ((and (< distance (- half-width))
                      (< (+ lon width) 360))
                 (incf lon width))))
        ;; Encode the recovered location.
        (encode (- lat 90) (- lon 180) (precision area))))))

;;; open-location-code.lisp ends here
