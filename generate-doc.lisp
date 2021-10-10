;;; generate-doc.lisp --- generate documentation.

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

(ql:quickload :open-location-code)
(ql:quickload :rs-doc) ;private

(let ((doc (rs-doc:gather-doc
	    :package :open-location-code
	    :symbols '(olc:validp
		       olc:fullp
		       olc:shortp
		       olc:encode
		       olc:decode
		       olc:shorten
		       olc:recover
		       olc:code-error
		       olc:code-length-error
		       olc:invalid-code-error
		       olc:full-code-error
		       olc:short-code-error
		       olc:code-area
		       olc:south-west-corner
		       olc:north-east-corner
		       olc:center
		       olc:precision
		       olc:code-length
		       olc:separator-position
		       olc:pad-characters)
	    :generic-functions nil)))
  (rs-doc:generate-doc :data doc
		       :output-format :html
		       :output (make-pathname :directory '(:relative "doc")
					      :name "open-location-code"
					      :type "html"))
  (rs-doc:generate-doc :data doc
		       :output-format :text
		       :output (make-pathname :directory '(:relative "doc")
					      :name "open-location-code"
					      :type "txt"))
  ())

;;; generate-doc.lisp ends here
