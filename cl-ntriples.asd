;;; -*- mode: lisp; -*-

;; Copyright (c) 2012, Victor Anyakin <anyakinvictor@yahoo.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the organization nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :cl-ntriples-asd
  (:use :cl :asdf))

(in-package :cl-ntriples-asd)

(defsystem :cl-ntriples
    :version "2012.12.16"      ; YYYY.MM.DD -- digits to suit the ASDF
    :licence "BSD"
    :description "CL-NTRIPLES is a simple basic parser for Ntriples data."
    :author "Victor Anyakin <anyakinvictor@yahoo.com>"
    :long-description
    "A basic parser and a set of simple utilities to parse N-Triples data.

N-Triples provides a simple format for representation of Semantic
Web/W3C RDF semantic data. CL-NTRIPLES provides a simple and easy to
use parser for Common Lisp applications."
    :components
    ((:module "src"
	      :components ((:file "package")
			   (:file "nt-parser" :depends-on ("package")))))
    :depends-on (:alexandria))



