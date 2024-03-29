;;; open-location-code.asd --- Open Location Code library

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

(in-package :asdf-user)

(defsystem "open-location-code"
  :description "Open Location Code library."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version (:read-file-line "VERSION")
  :depends-on ("alexandria" "iterate")
  :components ((:file "open-location-code"))
  :in-order-to ((test-op (test-op "open-location-code/tests"))))

(defsystem "open-location-code/tests"
  :description "Open Location Code library test suite."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :depends-on ("open-location-code" "lisp-unit" "cl-csv" "read-number")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :open-location-code-tests :main)))

;;; open-location-code.asd ends here
