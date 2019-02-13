;;; open-location-code.lisp --- Open Location Code library.

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

;;; Commentary:

;; The methods requested by the Open Location Code specification
;; are implemented as follows:
;;
;; encode (LATITUDE LONGITUDE)  ==>  FULL-CODE
;;      Convert a latitude and longitude into a 10 digit Open
;;      Location Code.
;;
;; encode (LATITUDE LONGITUDE PRECISION)  ==>  FULL-CODE
;;      Convert a latitude and longitude into an arbitrary
;;      length Open Location Code.
;;
;; decode (CODE)  ==>  CODE-AREA
;;      Decode an Open Location Code into, at a minimum, the
;;      latitude and longitude of the south-west corner and
;;      the area's height and width.
;;
;; validp (CODE)  ==>  boolean
;;      Determine if a string is a valid sequence of Open
;;      Location Code characters.
;;
;; fullp (CODE)  ==>  boolean
;;      Determine if a string is a valid full Open Location
;;      Code.
;;
;; shortp (CODE)  ==>  boolean
;;      Determine if a string is a valid short Open Location
;;      Code.
;;
;; shorten (FULL-CODE LATITUDE LONGITUDE)  ==>  SHORT-CODE
;;      Remove four or six digits from the front of an Open
;;      Location Code given a reference location.
;;
;; recover (SHORT-CODE LATITUDE LONGITUDE)  ==>  FULL-CODE
;;      Recover a full Open Location Code from a short code
;;      and a reference location.
;;
;; See <https://github.com/google/open-location-code/> for more
;; documentation and sample implementations.

;;; Code:

(in-package :common-lisp-user)

(defpackage :open-location-code
  (:nicknames :olc)
  (:use :common-lisp
	:iterate)
  (:documentation
   "Open Location Code is a location encoding system for addresses,
independent of street names and building numbers.

See <https://plus.codes>."))

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

(defconst +float-type+ 'double-float
  "Data type for floating-point numbers.")

(deftype float-type (&rest spec)
  "Type specifier for floating-point numbers."
  `(,+float-type+ ,@spec))

;; Save values of special variables and bind them to file local
;; values.  Variable ‘file-local-variables’ is a property list.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter file-local-variables
    (funcall (lambda (bindings)
	       (iter (for binding :in bindings)
		     (for symbol = (first binding))
		     ;; Save symbol and it's value.
		     (collect symbol)
		     (collect (symbol-value symbol))
		     ;; Set new value.
		     (set symbol (second binding))))
	     ;; File local variables.
	     `((*read-default-float-format* ,+float-type+)
	       ))))

(export 'code-error)
(define-condition code-error (type-error)
  ()
  (:documentation
   "Base class for all Open Location Code errors."))

(export 'code-length-error)
(define-condition code-length-error (code-error)
  ()
  (:documentation
   "Condition for an invalid Open Location Code length.")
  (:report
   (lambda (condition stream)
     (format stream
	     "The value ‘~S’ is not a valid Open Location Code length."
	     (type-error-datum condition)))))

(export 'invalid-code-error)
(define-condition invalid-code-error (code-error)
  ()
  (:documentation
   "Condition for an invalid Open Location Code.")
  (:report
   (lambda (condition stream)
     (format stream
	     "The value ‘~S’ is not a valid Open Location Code."
	     (type-error-datum condition)))))

(export 'full-code-error)
(define-condition full-code-error (invalid-code-error)
  ()
  (:documentation
   "Condition for an invalid full Open Location Code.")
  (:report
   (lambda (condition stream)
     (format stream
	     "The value ‘~S’ is not a full Open Location Code."
	     (type-error-datum condition)))))

(export 'short-code-error)
(define-condition short-code-error (invalid-code-error)
  ()
  (:documentation
   "Condition for an invalid short Open Location Code.")
  (:report
   (lambda (condition stream)
     (format stream
	     "The value ‘~S’ is not a short Open Location Code."
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
value of the digit.")

(defsubst digitp (char)
  "True if CHAR is a valid digit.

Argument CHAR has to be a character.

Value is the decimal value, i.e. weight, of the digit,
or null if CHAR is an invalid digit."
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
	(collect (coerce height +float-type+) :result-type 'vector)
	(setf height (/ height (if (< prec 5) +block-divisor+ +grid-rows+))))
  "Vector of code area heights in descending order.")

(defconst +area-width+
  (iter (with width = 400)
	(for prec :from 0 :to +maximum-precision+)
	(collect (coerce width +float-type+) :result-type 'vector)
	(setf width (/ width (if (< prec 5) +block-divisor+ +grid-columns+))))
  "Vector of code area widths in descending order.")

(defsubst area-size (precision)
  "Return the height and width of a code area as multiple values.
Argument PRECISION is the number of discretization steps."
  (values (svref +area-height+ precision)
	  (svref +area-width+ precision)))

(defconst +smallest-height+ (svref +area-height+ (1- (length +area-height+)))
  "Smallest height of a code area.")

(defconst +smallest-width+ (svref +area-width+ (1- (length +area-width+)))
  "Smallest width of a code area.")

(defsubst fix (number divisor)
  "Round NUMBER to the nearest multiple of DIVISOR."
  (* (round number divisor) divisor))

(export 'code-area)
(defclass code-area ()
  ((south
    :initform 0.0
    :type float-type
    :documentation "Lower latitude of the code area in degree angle.")
   (west
    :initform 0.0
    :type float-type
    :documentation "Lower longitude of the code area in degree angle.")
   (height
    :initform 0.0
    :type float-type
    :documentation "Height of the code area in degree angle.")
   (width
    :initform 0.0
    :type float-type
    :documentation "Width of the code area in degree angle.")
   (precision
    :initform 0
    :type fixnum
    :documentation "Precision of the original code.")
   (length
    :initform 0
    :type fixnum
    :documentation "Code length of the original code.")
   (plus
    :initform 0
    :type fixnum
    :documentation "Position of the separator character in the original code.")
   ;; If slot value PLUS is equal to eight, the original code was a full
   ;; Open Location Code and slot values SOUTH and WEST denote an absolute
   ;; location.  Otherwise, the original code was a short Open Location Code
   ;; and slot values SOUTH and WEST denote a relative location, i.e. the
   ;; offset to the reference location.
   (pad
    :initform 0
    :type fixnum
    :documentation "Number of pad characters in the original code."))
  (:documentation "Area covered by an Open Location Code."))

(export 'south-west-corner)
(defgeneric south-west-corner (object)
  (:documentation
   "Return the lower latitude and longitude as multiple values."))

(defmethod south-west-corner ((object code-area))
  (with-slots (south west) object
    (values south west)))

(export 'north-east-corner)
(defgeneric north-east-corner (object)
  (:documentation
   "Return the upper latitude and longitude as multiple values.
The upper bounds are not included in the code area except for
the north pole."))

(defmethod north-east-corner ((object code-area))
  (with-slots (south west height width) object
    (values (+ south height) (+ west width))))

(export 'center)
(defgeneric center (object)
  (:documentation
   "Return the center latitude and longitude as multiple values."))

(defmethod center ((object code-area))
  (with-slots (south west height width) object
    (values (+ south (/ height 2)) (+ west (/ width 2)))))

(export 'precision)
(defgeneric precision (object)
  (:documentation
   "Return the Open Location Code precision."))

(defmethod precision ((object code-area))
  "Return the precision of the original code."
  (slot-value object 'precision))

(defmethod precision ((length integer))
  "Return the precision as a function of the code length."
  (check-type length (integer 2))
  (precision-from-length length))

(export 'code-length)
(defgeneric code-length (object)
  (:documentation
   "Return the Open Location Code length."))

(defmethod code-length ((object code-area))
  "Return the code length of the original code."
  (slot-value object 'length))

(defmethod code-length ((precision integer))
  "Return the code length as a function of the precision."
  (check-type precision (integer 1))
  (length-from-precision precision))

(export 'separator-position)
(defgeneric separator-position (object)
  (:documentation
   "Return the position of the separator character."))

(defmethod separator-position ((object code-area))
  "Return the position of the separator character in the original code."
  (slot-value object 'plus))

(export 'pad-characters)
(defgeneric pad-characters (object)
  (:documentation
   "Return the number of pad characters."))

(defmethod pad-characters ((object code-area))
  "Return the number of pad characters in the original code."
  (slot-value object 'pad))

(defun normalize-location (latitude longitude precision)
  "Clip LATITUDE to the half-closed interval [-90, 90) and
normalize LONGITUDE to the half-closed interval [-180, 180).
Third argument PRECISION is used to determine the height of
 the code area if LATITUDE denotes the north pole.

Values are latitude, longitude, and the actual precision."
  (declare (type real latitude longitude)
	   (type (integer 1) precision))
  (let ((lat (coerce latitude +float-type+))
	(lon (coerce longitude +float-type+))
	(prec (min precision +maximum-precision+)))
    ;; Clip the latitude to the closed interval [-90, 90].
    (setf lat (alexandria:clamp lat -90.0 90.0))
    ;; Ensure that the represented area does not exceed 90° latitude.
    (when (= lat 90.0)
      (multiple-value-bind (height width)
	  (area-size precision)
	(decf lat (/ height 2))))
    ;; Normalise the longitude to the half-closed interval [-180, 180).
    (setf lon (rem lon 360.0))
    (cond ((< lon -180.0)
	   (incf lon 360.0))
	  ((>= lon 180.0)
	   (decf lon 360.0)))
    ;; Return values.
    (values lat lon prec)))

(defun normalize-location* (latitude longitude precision)
  "Like ‘normalize-location’ but return latitude and longitude
as non-negative numbers."
  (multiple-value-bind (lat lon prec)
      (normalize-location latitude longitude precision)
    ;; Add 90° to the latitude.
    (incf lat 90.0)
    ;; Add 180° to the longitude.
    (incf lon 180.0)
    ;; Return values.
    (values lat lon prec)))

(defun analyse (code &optional area)
  "Analyse an Open Location Code.

First argument CODE is the Open Location Code (an object).
If optional second argument AREA is non-null, secondary value
 is a code area object.

Primary value is ‘:full’ or ‘:short’ if CODE is a valid full or short
Open Location Code respectively.  Otherwise, all values are null."
  (let (valid object)
    (when (stringp code)
      (iter (with south = 0.0)
	    (with west = 0.0)
	    (with height = 0.0)
	    (with width = 0.0)
	    (with length = 0)
	    (with plus)
	    (with pad = 0)
	    ;; Decimal value of a digit.
	    (with value)
	    ;; Discretization step of the next digit.
	    (with prec = (case (position #\+ code :test #'char=)
			   (8 1) (6 2) (4 3) (2 4) (0 5) (t 0)))
	    (initially
	     (when (= prec 0)
	       (return)))
	    (for pos :from 0 :below (length code))
	    (for char = (aref code pos))
	    (cond ((char= char #\0)
		   ;; Pad characters can only occur before the
		   ;; separator character.
		   (when plus
		     (leave))
		   (incf pad))
		  ((char= char #\+)
		   ;; There can only be one separator character
		   ;; and it has to be at the correct position.
		   ;; The later already has been checked in the
		   ;; prologue section.
		   (when plus ;(or plus (oddp pos) (> pos 8))
		     (leave))
		   (setf plus pos))
		  ((setf value (digitp char))
		   ;; Valid characters can only occur before pad
		   ;; characters.
		   (when (plusp pad)
		     (leave))
		   ;; Update code area.
		   (when (not (null area))
		     (cond ((> prec 5)
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
		  (t
		   ;; Invalid character.
		   (leave)))
	    (finally
	     (when (and plus
			;; There must be an even number of pad characters.
			(evenp pad)
			;; Code length has to be at least two.
			(>= length 2)
			;; Code length is either less than or equal
			;; to the position of the separator character,
			;; i.e. the separator character is the last
			;; character, or there are two or more valid
			;; characters after the separator character.
			(or (<= length plus)
			    (>= (- length plus) 2))
			;; For a full code, the first two characters
			;; have to be in the proper range.
			(or (/= plus 8)
			    (and (< (digitp (aref code 0)) 9)
				 (< (digitp (aref code 1)) 18)))
			;; Code is valid.
			(setf valid (if (= plus 8) :full :short)))
	       (when (not (null area))
		 (setf object (make-instance 'code-area))
		 (if (= plus 8)
		     (setf (slot-value object 'south) (fix (- south 90.0) height)
			   (slot-value object 'west) (fix (- west 180.0) width))
		   (setf (slot-value object 'south) (fix south height)
			 (slot-value object 'west) (fix west width)))
		 (setf (slot-value object 'height) height
		       (slot-value object 'width) width
		       (slot-value object 'precision) (1- prec)
		       (slot-value object 'length) length
		       (slot-value object 'plus) plus
		       (slot-value object 'pad) pad))
	       ))))
    (values valid object)))

(export 'validp)
(defun validp (code)
  "True if an object is a valid sequence of Open Location Code characters.

Argument CODE is an object of any type.

Value is ‘:full’ or ‘:short’ if CODE is a valid full or short Open
Location Code respectively.  Otherwise, value is null."
  (nth-value 0 (analyse code)))

(export 'fullp)
(defun fullp (code)
  "True if an object is a valid full Open Location Code.

Argument CODE is an object of any type.

Value is true if CODE is a valid full Open Location Code.
Otherwise, value is null."
  (eq (analyse code) :full))

(export 'shortp)
(defun shortp (code)
  "True if an object is a valid short Open Location Code.

Argument CODE is an object of any type.

Value is true if CODE is a valid full Open Location Code.
Otherwise, value is null."
  (eq (analyse code) :short))

(export 'encode)
(defun encode (latitude longitude &optional (precision 5))
  "Encode a location into an Open Location Code.

First argument LATITUDE and second argument LONGITUDE denote the
 location in degree angle.
Optional third argument PRECISION is the precision of the code.
 Default is five, i.e. a code length of ten digits.

Value is a string.

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

The code length is equal to the number of Open Location Code
digits.  The separator character ‘+’ is not part of the code
length.  The code area dimensions are calculated with a mean
earth radius of 6356766 m for a code area at the equator."
  (check-type latitude real)
  (check-type longitude real)
  (check-type precision (integer 1))
  (multiple-value-bind (lat lon prec)
      (normalize-location* latitude longitude precision)
    ;; Do the encoding.
    (let ((code (make-string (+ 9 (if (> prec 4) 2 0) (max 0 (- prec 5))) :initial-element #\0)))
      (setf (aref code 8) #\+)
      (let ((index 1)
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

(export 'decode)
(defun decode (code)
  "Decode an Open Location Code.

Argument CODE is an Open Location Code (a string).

Primary value is a ‘code-area’ object.  Secondary value is ‘:full’ or
‘:short’ if CODE is a full or short Open Location Code respectively.

Signal an ‘invalid-code-error’ if CODE is not a valid Open Location
Code."
  (check-type code string)
  (multiple-value-bind (valid area)
      (analyse code t)
    (when (not valid)
      (error 'invalid-code-error :datum code))
    (values area valid)))

(export 'shorten)
(defun shorten (code latitude longitude)
  "Remove four, six, or eight digits from the front of a full Open
Location Code given a reference location.

First argument CODE is a full Open Location Code (a string).
Second argument LATITUDE and third argument LONGITUDE denote the
 reference location in degree angle.

Value is the short code, or the original full code if the reference
location is too far.

Signal a ‘full-code-error’ if CODE is not a full Open Location
Code."
  (check-type code string)
  (check-type latitude real)
  (check-type longitude real)
  (multiple-value-bind (valid area)
      (analyse code t)
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
	    (when (< distance (* block-size 0.3))
	      (return-from shorten (subseq code pos)))))
    ;; Return original full Open Location Code.
    code))

(export 'recover)
(defun recover (code latitude longitude)
  "Recover a full Open Location Code from a short code
and a reference location.

First argument CODE is a short Open Location Code (a string).
Second argument LATITUDE and third argument LONGITUDE denote
 the reference location in degree angle.

Value is the recovered full code.

Signal a ‘short-code-error’ if CODE is not a short Open Location
Code."
  (check-type code string)
  (check-type latitude real)
  (check-type longitude real)
  (multiple-value-bind (valid area)
      (analyse code t)
    (unless (eq valid :short)
      (error 'short-code-error :datum code))
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
      (incf lat (* (ffloor ref-lat height) height))
      (incf lon (* (ffloor ref-lon width) width))
      ;; Check if the recovered code area is too far from the
      ;; reference location.  If so, move it.
      (let ((distance (- lat ref-lat))
	    (half-height (/ height 2)))
	(cond ((and (> distance half-height)
		    (> (- lat height) 0.0))
	       (decf lat height))
	      ((and (< distance (- half-height))
		    (< (+ lat height) 180.0))
	       (incf lat height))))
      (let ((distance (- lon ref-lon))
	    (half-width (/ width 2)))
	(cond ((and (> distance half-width)
		    (> (- lon width) 0.0))
	       (decf lon width))
	      ((and (< distance (- half-width))
		    (< (+ lon width) 360.0))
	       (incf lon width))))
      ;; Encode the recovered location.
      (encode (- lat 90.0) (- lon 180.0) (precision area)))))

;; Restore special variables saved in ‘file-local-variables’.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (boundp 'file-local-variables)
    (iter (while file-local-variables)
	  (for symbol = (pop file-local-variables))
	  (for value = (pop file-local-variables))
	  (set symbol value))))

;;; open-location-code.lisp ends here
