;;; mi-util.el --- Utility functions of mode-info

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: info

;; This file is a part of mode-info.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This file provides utility functions and an object system of
;; mode-info.


;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (cond
   ((boundp 'MULE)
    (require 'poe)
    (require 'pcustom))))

(defconst mode-info-titles-type
  '(repeat (choice (string :tag "Title")
		   (repeat :tag "Aliases" (string :tag "Title"))))
  "A widget type for editing `mode-info-*-titles'.")

(put 'mode-info-static-if 'edebug-form-spec '(form form body))
(put 'mode-info-static-if 'lisp-indent-function 2)
(defmacro mode-info-static-if (cond then &rest else)
  "Like `if', but evaluate COND at compile time."
  (if (eval cond) then `(progn ,@else)))

(eval-and-compile
  (unless (fboundp 'match-string-no-properties)
    (defun match-string-no-properties (num &optional string)
      "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
      (if (match-beginning num)
	  (if string
	      (let ((result
		     (substring string (match-beginning num) (match-end num))))
		(set-text-properties 0 (length result) nil result)
		result)
	    (buffer-substring-no-properties (match-beginning num)
					    (match-end num)))))))


;;; Object System:
(eval-and-compile
  (defvar mode-info-obarray
    (let ((obarray (make-vector 31 nil)))
      (intern "mode-info" obarray) ;; mode-info means the base object.
      obarray)
    "Table of mode-info-classes.")
  (defvar mode-info-method-obarray (make-vector 63 nil)
    "Method table of mode-info-classes."))

(defconst mode-info-prefix "mode-info-")

(defmacro mode-info-defclass (class &rest parents)
  "Define CLASS as a mode-info-class."
  `(eval-and-compile
     (mode-info-defclass-1 (quote ,class) (quote ,parents))))

(eval-and-compile
  (defmacro mode-info-parents (class)
    "Return parents of CLASS."
    `(get ,class '*parents*)))

(defun mode-info-defclass-1 (class parents)
  (setf (mode-info-parents
	 (setq class (intern (symbol-name class) mode-info-obarray)))
	(append
	 (mapcar (lambda (p)
		   (intern (symbol-name p) mode-info-obarray))
		 parents)
	 '(mode-info)))
  class)

(defmacro mode-info-method-symbol-name (method class)
  "Generate a unique name from METHOD and CLASS."
  `(concat (symbol-name ,class) "@" (symbol-name ,method)))

(defun mode-info-find-class (name)
  "Return class has NAME."
  (intern-soft (if (symbolp name) (symbol-name name) name)
	       mode-info-obarray))

(defmacro mode-info-class (entity)
  "Return class of ENTITY."
  `(intern-soft (symbol-name ,entity) mode-info-obarray))

(defun mode-info-class-name (class)
  "Return the CLASS name."
  (symbol-name class))

(defun mode-info-method-search (name class)
  "Search the method which is named NAME for CLASS."
  (or (intern-soft (mode-info-method-symbol-name name class)
		   mode-info-method-obarray)
      (let ((method)
	    (classes (mode-info-parents class)))
	(while (and (not method) classes)
	  (setq method
		(mode-info-method-search name (pop classes))))
	method)))

(put 'mode-info-void-method 'error-conditions
     '(error void-function mode-info-void-method))
(put 'mode-info-void-method 'error-message
     "Symbol's method definition is void")

(put 'mode-info-defgeneric 'lisp-indent-function 'defun)
(defmacro mode-info-defgeneric (name args &optional document)
  "Define a function NAME that provides a generic interface to the method NAME.
ARGS is the argument list for NAME.  The first element of ARGS is an
entity."
  `(defun ,(intern (concat mode-info-prefix (symbol-name name))) ,args
     ,@(if document (list document) nil)
     (let ((method (mode-info-method-search
		    (quote ,name) (mode-info-class ,(car args)))))
       (if method
	   (,(if (memq '&rest args)
		 'apply
	       'funcall)
	    method
	    ,@(delq '&rest (delq '&optional (copy-sequence args))))
	 (signal 'mode-info-void-method
		 (make-symbol
		  (mode-info-method-symbol-name (quote ,name)
						,(car args))))))))

(eval-and-compile
  (defun mode-info-method-search-next (name class)
    "Search the next method which is named NAME for CLASS."
    (let ((method)
	  (classes (mode-info-parents class)))
      (while (and (not method) classes)
	(setq method (mode-info-method-search name (pop classes))))
      method)))

(put 'mode-info-defmethod 'lisp-indent-function 'defun)
(put 'mode-info-defmethod 'edebug-form-spec
     '(&define name
	       ((arg symbolp)
		[&rest arg]
		[&optional ["&optional" arg &rest arg]]
		&optional ["&rest" arg])
	       [&optional stringp]
	       [&optional ("interactive" interactive)]
	       def-body))
(defmacro mode-info-defmethod (name spec &rest body)
  "Define NAME as a method of a mode-info-class."
  (let* ((class (nth 1 (car spec)))
	 (args (cons (caar spec) (cdr spec)))
	 (next (mode-info-method-search-next name (mode-info-class class))))
    `(fset (intern (eval-when-compile
		     (mode-info-method-symbol-name (quote ,name)
						   (quote ,class)))
		   mode-info-method-obarray)
	   ,(if next
		`(function
		  (lambda ,args
		    (labels ((mode-info-method-next
			      nil
			      (,(if (memq '&rest args)
				    'apply
				  'funcall)
			       (intern ,(symbol-name next)
				       mode-info-method-obarray)
			       ,@(delq '&rest (delq '&optional
						    (copy-sequence args))))))
		      ,@body)))
	      `(function (lambda ,args ,@body))))))

(defun mode-info-method-next (&rest args)
  "Call the next method in the current method function.
IMPORTANT NOTICE: It is disabled using this function out of methods."
  (signal 'mode-info-void-method nil))

(provide 'mi-util)

;;; mi-util.el ends here
