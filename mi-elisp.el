;;; mi-elisp.el --- Mode-info backend for emacs-lisp-mode -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1998-2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: emacs-lisp info

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

;; This file provides Info interface commands for major-modes related
;; to Emacs-Lisp.


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-elisp nil
  "Various sorts of improved help system for emacs-lisp-mode."
  :group 'mode-info)

(defcustom mode-info-elisp-index-file
  (expand-file-name "mi-elisp.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about emacs-lisp."
  :group 'mode-info-elisp
  :type 'file)

(defcustom mode-info-elisp-titles
  (let ((x (if (file-directory-p "/var/lib/dpkg")
	       "elisp-ja" "elisp-jp")))
    (if (boundp 'MULE)
	(list x "mule-jp")
      (list x)))
  "*Info titles about emacs-lisp."
  :group 'mode-info-elisp
  :type '(repeat string))

(defconst mode-info-elisp-entry-regexp "\
^[ \t]+-+[ \t]+\\(\\(Prefix[ \t]+\\)?Command\\|\\(プレフィックス\\)?コマンド\\|\
Function\\|Special[ \t]+Form\\|Macro\\|\\(\\(Glob\\|Loc\\)al[ \t]+\\)?Variable\\|\
\\(User[ \t]+\\)?Option\\):[ \t]+\\([^ \t\n]+\\)[ \t\n]")
(defconst mode-info-elisp-entry-pos 7)

(mode-info-defclass elisp)

(mode-info-defmethod function-at-point ((class elisp))
  (mode-info-static-if (featurep 'xemacs)
      (function-at-point)
    (function-called-at-point)))

(mode-info-defmethod read-function ((class mode-info) &optional prompt
				    default predicate require-match)
  (unless default
    (setq default (mode-info-function-at-point class)))
  (let* ((enable-recursive-minibuffers t)
	 (x (completing-read
	     (or prompt
		 (if default
		     (format "[%s] Describe function (default %s): "
			     (mode-info-class-name class) default)
		   (format "[%s] Describe function: "
			   (mode-info-class-name class))))
	     obarray
	     (if predicate
		 `(lambda (x) (and (fboundp x) (,predicate x)))
	       (function fboundp))
	     require-match)))
    (if (string= x "") default (intern x))))

(mode-info-defmethod function-document ((class elisp) function)
  (mode-info-load-index class)
  (let ((entry (assq function (mode-info-function-alist class))))
    (if entry
	(mode-info-goto-info-entry class entry)
      (describe-function function)
      (with-current-buffer "*Help*" (point-min-marker)))))

(mode-info-defmethod variable-at-point ((class elisp))
  (let ((v (variable-at-point)))
    (if (eq v 0) nil v)))

(mode-info-defmethod read-variable ((class mode-info) &optional prompt
				    default predicate require-match)
  (unless default
    (setq default (mode-info-variable-at-point class)))
  (let* ((enable-recursive-minibuffers t)
	 (x (completing-read
	     (or prompt
		 (if default
		     (format "[%s] Describe variable (default %s): "
			     (mode-info-class-name class) default)
		   (format "[%s] Describe variable: "
			   (mode-info-class-name class))))
	     obarray
	     (if predicate
		 `(lambda (x) (and (boundp x) (,predicate x)))
	       (function boundp))
	     require-match)))
    (if (string= x "") default (intern x))))

(mode-info-defmethod variable-document ((class elisp) variable)
  (mode-info-load-index class)
  (let ((entry (assq variable (mode-info-variable-alist class))))
    (if entry
	(mode-info-goto-info-entry class entry)
      (describe-variable variable)
      (with-current-buffer "*Help*" (point-min-marker)))))

(defun mode-info-elisp-make-index ()
  "Make index of Info files listed in `mode-info-elisp-titles'."
  (interactive)
  (mode-info-make-index 'elisp
			mode-info-elisp-titles
			mode-info-elisp-entry-regexp
			mode-info-elisp-entry-pos))

(mode-info-defmethod write-index-file ((class elisp) functions variables
				       &optional regexp)
  (with-temp-buffer
    (dolist (sym (list functions variables))
      (set sym (mapcar (lambda (x)
			 (cons (intern (car x)) (cdr x)))
		       (symbol-value sym))))
    (mode-info-dump-symbols functions variables)
    (write-region (point-min) (point-max)
		  (mode-info-index-file-name class))))

(mode-info-defmethod process-index-node ((class elisp) title nodename
					 functions variables)
  (when (string= "Index" nodename)
    (let (x)
      (while (re-search-forward mode-info-index-entry-regexp nil t)
	(and (setq x (intern-soft (match-string 1)))
	     (fboundp x)
	     (mode-info-process-index-node-1 title
					     functions
					     (match-string 1)
					     (match-string 2)
					     (match-string 3)))
	(and (setq x (intern-soft (match-string 1)))
	     (boundp x)
	     (mode-info-process-index-node-1 title
					     variables
					     (match-string 1)
					     (match-string 2)
					     (match-string 3)))))))

(provide 'mi-elisp)

;;; mi-elisp.el ends here
