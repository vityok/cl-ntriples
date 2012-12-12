;;; N-Triples parser

;;; Useful resources:

;; http://www.w3.org/2001/sw/RDFCore/ntriples/
;; http://www.w3.org/TR/rdf-testcases/#ntriples
;; http://www.w3.org/DesignIssues/Notation3


;;; N-Triples format definition in EBNF

;; ntripleDoc	::=	line*
;; line		::=	ws* ( comment | triple )? eoln
;; comment	::=	'#' ( character - ( cr | lf ) )*
;; triple	::=	subject ws+ predicate ws+ object ws* '.' ws*
;; subject	::=	uriref | nodeID
;; predicate	::=	uriref
;; object	::=	uriref | nodeID | literal
;; uriref	::=	'<' absoluteURI '>'
;; nodeID	::=	'_:' name
;; literal	::=	langString | datatypeString
;; langString	::=	'"' string '"' ( '@' language )?
;; datatypeString	::=	'"' string '"' '^^' uriref
;; language	::=	[a-z]+ ('-' [a-z0-9]+ )*
;; 			encoding a language tag.
;; ws		::=	space | tab
;; eoln		::=	cr | lf | cr lf
;; space	::=	#x20 /* US-ASCII space - decimal 32 */
;; cr		::=	#xD /* US-ASCII carriage return - decimal 13 */
;; lf		::=	#xA /* US-ASCII line feed - decimal 10 */
;; tab		::=	#x9 /* US-ASCII horizontal tab - decimal 9 */
;; string	::=	character* with escapes as defined in section Strings
;; name		::=	[A-Za-z][A-Za-z0-9]*
;; absoluteURI	::=	character+ with escapes as defined in section URI References
;; character	::=	[#x20-#x7E] /* US-ASCII space to decimal 126 */

;;; Main function is PARSE-NT

(ql:quickload "alexandria")

(defpackage :nt-parser
  (:nicknames :nt)
  (:use :cl :alexandria)
  (:export
   #:parse-nt))

(in-package :nt-parser)

;;---------------------------------------------------------

(define-constant +NT-CR+ #xD)
(define-constant +NT-LF+ #xA)
(define-constant +NT-SPACE+ #x20)
(define-constant +NT-TAB+ #x9)
(define-constant +NT-WS+ `(,+NT-SPACE+ ,+NT-TAB+))

;;---------------------------------------------------------

(defun ntriple-ws-p (c)
  (or (= (char-code c) +NT-SPACE+)
      (= (char-code c) +NT-TAB+)))

(defun ntriple-crlf-p (c)
  (or (= (char-code c) +NT-CR+)
      (= (char-code c) +NT-LF+)))

;;---------------------------------------------------------

(defun consume-whitespace (stream)
  (loop
     :for c = (peek-char t stream nil) :then (peek-char t stream nil)
     :while c
     :while (ntriple-ws-p c)
     :do (read-char stream)))

;;---------------------------------------------------------

(defun parse-uriref (stream)
  ;; uriref ::= '<' absoluteURI '>'
  (read-char stream)			; skip the <
  (with-output-to-string (str)
    (loop :for c = (read-char stream) :then (read-char stream)
       :until (char= c #\>)
       :do (princ c str))))

;;---------------------------------------------------------

(defun parse-node-id (stream)
  ;; nodeID ::=	'_:' name
  (read-char stream)			; skip the '_'
  (read-char stream)			; skip the ':'
  (with-output-to-string (str)
    (loop :for c = (peek-char t stream)
       :while (alphanumericp c)
       :do (princ (read-char stream) str))))

;;---------------------------------------------------------

(defun parse-literal (stream)
  "Parse the object literal from the stream.
"
  ;; literal ::= langString | datatypeString
  ;; langString ::= '"' string '"' ( '@' language )?
  ;; datatypeString ::=	'"' string '"' '^^' uriref

  (when (read-char stream nil)		; skip "
    (let ((literal-string		; remember the string
	   (with-output-to-string (str)
	     (loop :for c = (read-char stream)
		:when (char= c #\\)
		:do (switch ((read-char stream) :test #'char=)
		      (#\\ (princ #\\ str))
		      (#\n (princ #\Newline str))
		      (#\N (princ #\Newline str))
		      (#\" (princ #\" str)))
		:until (char= c #\")
		:do (princ c str))))
	  (lang-string ""))

      (switch ((peek-char t stream) :test #'char=)
	(#\@
	 ;; language ::= [a-z]+ ('-' [a-z0-9]+ )*
	 (read-char stream)		; skip the at sign
	 (setf lang-string
	       (with-output-to-string (str)
		 (loop :for c = (peek-char t stream)
		    :while (or (alphanumericp c)
			       (char= c #\-))
		    :do (princ (read-char stream) str))))
	
	 (list :literal-string literal-string
	       :lang lang-string))
	(#\^
	 (read-char stream)		; skip the hats
	 (read-char stream)
	 (list :literal-string literal-string
	       :uriref (parse-uriref stream)))
	(t			       ; simple literal: just a string
	 (list :literal-string literal-string))))))

;;---------------------------------------------------------

(defun parse-ntriple-triple (stream)
  "The triple consists of a: subject, predicate, object. All they are
separated by whitespace. There is also a terminating full stop point
in the end of a tripple.

Returns a list consisting of three elements corresponding to the
tripple.

The syntax is:
triple ::= subject ws+ predicate ws+ object ws* '.' ws*
"

  (flet ((nt-parse-subject ()
	   ;; subject ::= uriref | nodeID
	   (consume-whitespace stream)
	   (switch ((peek-char t stream) :test #'char=)
	     (#\<
	      (parse-uriref stream))
	     (#\_
	      (parse-node-id stream))
	     (t
	      (format t "wrong char `~a' in nt-parse-subject~%" (peek-char t stream)))))

	 (nt-parse-predicate ()
	   ;; predicate ::= uriref
	   (consume-whitespace stream)
	   (parse-uriref stream))

	 (nt-parse-object ()
	   ;; object ::= uriref | nodeID | literal
	   (consume-whitespace stream)
	   (switch ((peek-char t stream) :test #'char=)
	     (#\<
	      (parse-uriref stream))
	     (#\_
	      (parse-node-id stream))
	     (#\" 
	      (parse-literal stream))
	     (t
	      (format t "wrong character `~a' in nt-parse-subject~%" (peek-char t stream)))))

	 (nt-consume-period ()
	   (consume-whitespace stream)
	   (if (char= #\. (peek-char t stream))
	       (read-char stream)
	       (format t "wrong character `~a', expecting `.'" (peek-char t stream)))
	   (consume-whitespace stream)))

    (let ((triple (list (nt-parse-subject)
			(nt-parse-predicate)
			(nt-parse-object))))

      (nt-consume-period)
      triple)))

;;---------------------------------------------------------

(defun parse-ntriple-line (stream)
  ;; consume white space if there is any at the start of a line
  (loop
     :for c = (peek-char t stream nil) :then (peek-char t stream nil)
     :unless c
     :do (return-from parse-ntriple-line nil)
     :while (ntriple-ws-p c)
     :do (read-char stream))
  ;; check if this line is a comment or a triple
  (if (char= #\# (peek-char t stream))
      (progn				; consume comment line
	(loop :for c = (read-char stream nil) :then (read-char stream nil)
	   :unless c
	   :do (return-from parse-ntriple-line 'comment)
	   :until (ntriple-crlf-p c))
	(when (and (peek-char t stream nil)
		   (ntriple-crlf-p (peek-char t stream)))
	  (read-char stream nil))
	'comment)
      ;; this can be a valid ntriple line
      (parse-ntriple-triple stream)))

;;---------------------------------------------------------

(defun parse-ntriple-doc (stream)
  (loop
     :for line = (parse-ntriple-line stream) :then (parse-ntriple-line stream)
     :while line
     :unless (eq line 'comment)
     :collect line))

;;---------------------------------------------------------

(defgeneric parse-nt (src)
  (:documentation "Parses N-Triples from the given source. This method
accepts a file pathname, a stream, or a string.")
  (:method ((src pathname))
    (with-open-file (stream src :direction :input)
      (parse-ntriple-doc stream)))

  (:method ((src string))
    (with-input-from-string (stream src)
      (parse-ntriple-doc stream)))

  (:method ((src stream))
    (parse-ntriple-doc src)))

;; EOF