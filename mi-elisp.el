;;; mi-elisp.el --- Mode-info backend for Emacs Lisp Reference Manual -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1998-2006 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

;; This file provides mode-info backend stuffs to see the GNU Emacs
;; Lisp Reference Manual, which can be downloaded from:
;;
;;     ftp://ftp.gnu.org/pub/gnu/emacs/elisp-manual-21-2.7a.tar.gz
;;
;; The Japanese version of its edition 2.5 can be downloaded from:
;;
;;     ftp://ftp.ascii.co.jp/pub/GNU/elisp-manual-20-2.5-jp.tgz
;;
;; The Japanese version of itsh edition 2.4 can be downloaded from:
;;
;;     ftp://etlport.etl.go.jp/pub/mule/elisp-manual-19-2.4-jp2.0.tar.gz


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (condition-case nil
      (require 'help-mode) ; For `help-xref' button of Emacs-21.3.50.
    (error nil))
  (require 'mi-config) ; For mode-info-with-help-buffer().
  (require 'mi-index))

(eval-and-compile
  (autoload 'find-function-search-for-symbol "find-func")
  (autoload 'find-variable-noselect "find-func"))

(defgroup mode-info-elisp nil
  "Various sorts of improved help system for emacs-lisp-mode."
  :group 'mode-info)

(defcustom mode-info-elisp-index-file
  (expand-file-name "mi-elisp.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about emacs-lisp."
  :group 'mode-info-elisp
  :type 'file)

(defcustom mode-info-elisp-titles
  (delq nil
	(list '("elisp-ja" "elisp-jp" "elisp")
	      (and (boundp 'MULE) "mule-jp")))
  "*Info titles about emacs-lisp."
  :group 'mode-info-elisp
  :type mode-info-titles-type)

(defconst mode-info-elisp-entry-regexp "\
^[ \t]+-+[ \t]+\\(\\(Prefix[ \t]+\\)?Command\\|\\(プレフィックス\\)?コマンド\\|\
Function\\|Special[ \t]+Form\\|Macro\\|\\(\\(Glob\\|Loc\\)al[ \t]+\\)?Variable\\|\
\\(User[ \t]+\\)?Option\\):[ \t]+\\([^ \t\n]+\\)[ \t\n]")
(defconst mode-info-elisp-entry-pos 7)
(defconst mode-info-elisp-packages
  '(calender diary-lib dired edebug facemenu font-lock view))

(mode-info-defclass elisp)

(mode-info-defmethod function-at-point ((class elisp))
  (mode-info-static-if (featurep 'xemacs)
      (function-at-point)
    (function-called-at-point)))

(mode-info-defmethod read-function ((class elisp) &optional prompt
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

(mode-info-defmethod function-described-p ((class elisp) function)
  (mode-info-load-index class)
  (and (assq function (mode-info-function-alist class)) t))

(defsubst mode-info-elisp-function-document-1 (function)
  (goto-char (point-max))
  (forward-line -1)
  (or (when (search-forward "not documented" nil t)
	(let ((x (ignore-errors
		   (unless (subrp (symbol-function function))
		     ;; Don't use find-function-noselect because it
		     ;; follows aliases (which fails for built-in
		     ;; functions).
		     (find-function-search-for-symbol
		      function nil (symbol-file function))))))
	  (when x
	    (message "%s is not documented" function)
	    x)))
      (cons (current-buffer) (point-min))))

(mode-info-defmethod function-document ((class elisp) function)
  (mode-info-load-index class)
  (let ((entry (assq function (mode-info-function-alist class))))
    (if entry
	(mode-info-goto-info-entry class entry)
      (describe-function function)
      (mode-info-with-help-buffer
	(mode-info-elisp-function-document-1 function)))))

(mode-info-defmethod variable-at-point ((class elisp))
  (let ((v (variable-at-point)))
    (if (eq v 0) nil v)))

(mode-info-defmethod read-variable ((class elisp) &optional prompt
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

(mode-info-defmethod variable-described-p ((class elisp) variable)
  (mode-info-load-index class)
  (and (assq variable (mode-info-variable-alist class)) t))

(mode-info-defmethod variable-document ((class elisp) variable)
  (mode-info-load-index class)
  (let ((entry (assq variable (mode-info-variable-alist class))))
    (if entry
	(mode-info-goto-info-entry class entry)
      (describe-variable variable)
      (mode-info-with-help-buffer
	(cons (current-buffer) (point-min))))))

(mode-info-defmethod describe-variable-internal ((class elisp) variable
						 &optional keep-window)
  (let ((value (symbol-value variable))
	(buffer (when (local-variable-p variable) (buffer-name))))
    (mode-info-method-next)
    (when (mode-info-variable-described-p class variable)
      (if buffer
	  (message "%s's local value is %s in buffer %s; %s"
		   variable (prin1-to-string value) buffer
		   (if (default-boundp variable)
		       (format "global value is %s"
			       (prin1-to-string (default-value variable)))
		     "globally void"))
	(message "%s's value is %s" variable (prin1-to-string value))))))

(mode-info-defmethod read-tag ((class elisp))
  (let* ((default (or (mode-info-variable-at-point class)
		      (mode-info-function-at-point class)))
	 (x (completing-read (if default
				 (format "[%s] Find tag (default %s): "
					 (mode-info-class-name class)
					 default)
			       (format "[%s] Find tag: "
				       (mode-info-class-name class)))
			     obarray
			     (lambda (s) (or (boundp s) (fboundp s)))
			     t)))
    (if (string= x "") default (intern x))))

(mode-info-defmethod find-tag-noselect ((class elisp) tag)
  (cond
   ((fboundp tag)
    (if (subrp (symbol-function tag))
	(prog1 (mode-info-function-document class tag)
	  (message "%s is a primitive function" tag))
      ;; Don't use find-function-noselect because it follows aliases
      ;; (which fails for built-in functions).
      (find-function-search-for-symbol tag nil (symbol-file tag))))
   ((boundp tag)
    (condition-case err
	(find-variable-noselect tag)
      (error
       (let ((msg (error-message-string err)))
	 (if (string= msg
		      (format "Don't know where `%s' is defined" tag))
	     (prog1 (mode-info-variable-document class tag)
	       (message msg))
	   (signal (car err) (cdr err)))))))))

(eval-and-compile
  (if (fboundp 'define-button-type)
      (defalias 'mode-info-define-button-type 'define-button-type)
    (defun mode-info-define-button-type (type &rest properties)
      "Define a `button type' called NAME (dummy function)."
      (put type 'mode-info-button properties))
    (put 'mode-info-define-button-type 'lisp-indent-function 1)))

(eval-and-compile
  (if (fboundp 'button-type-get)
      (defalias 'mode-info-button-type-get 'button-type-get)
    (defun mode-info-button-type-get (type prop)
      "Get the property of button-type TYPE named PROP (dummy function)."
      (plist-get (get type 'mode-info-button) prop))))

(mode-info-static-if (fboundp 'help-insert-xref-button)
    (eval-and-compile
      (if (fboundp 'define-button-type)
	  (defalias 'mode-info-insert-button 'help-insert-xref-button)
	(defun mode-info-insert-button (string type &rest args)
	  "Insert STRING and make a hyperlink."
	  (help-insert-xref-button string
				   (mode-info-button-type-get type
							      'help-function)
				   args
				   (mode-info-button-type-get type
							      'help-echo)))))
  (mode-info-static-if (fboundp 'help-xref-button)
      ;; The function is designed for Emacs-20.x, based on
      ;; `help-insert-xref-button' defined in help.el of Emacs-21.2.
      (defun mode-info-insert-button (string type &rest args)
	"Insert STRING and make a hyperlink."
	(let ((pos (point)))
	  (insert string)
	  (goto-char pos)
	  (search-forward string)
	  (help-xref-button 0
			    (mode-info-button-type-get type 'help-function)
			    args)))
    (defun mode-info-insert-button (string type &rest args)
      "Insert STRING (dummy function)."
      (insert string))))

(mode-info-define-button-type 'mode-info-describe-function
  ':supertype 'help-xref
  'help-function 'mode-info-describe-function
  'help-echo "mouse-2, RET: go to Info.")

(defun mode-info-elisp-add-function-button (function)
  (let ((buffer-read-only)
	(class (mode-info-find-class 'elisp)))
    (when (mode-info-function-described-p class function)
      (save-excursion
	(save-match-data
	  (goto-char (point-max))
	  (if (re-search-backward "\\[[-a-z]+\\]"
				  (line-beginning-position) t)
	      (progn
		(end-of-line)
		(insert " "))
	    (insert (if (bolp) "\n" "\n\n")))
	  (mode-info-insert-button "[info]"
				   'mode-info-describe-function
				   function class t))))))

(mode-info-define-button-type 'mode-info-describe-variable
  ':supertype 'help-xref
  'help-function 'mode-info-describe-variable
  'help-echo "mouse-2, RET: go to Info.")

(defun mode-info-elisp-add-variable-button (variable)
  (let ((buffer-read-only)
	(class (mode-info-find-class 'elisp)))
    (when (mode-info-variable-described-p class variable)
      (save-excursion
	(save-match-data
	  (goto-char (point-max))
	  (if (re-search-backward "\\[[-a-z]+\\]"
				  (line-beginning-position) t)
	      (progn
		(end-of-line)
		(insert " "))
	    (insert (if (bolp) "\n" "\n\n")))
	  (mode-info-insert-button "[info]"
				   'mode-info-describe-variable
				   variable class t))))))

(defun mode-info-elisp-info-ref (function)
  "Look up an Emacs Lisp function in the Elisp manual in the Info system."
  (let ((class (mode-info-find-class 'elisp)))
    (when (mode-info-function-described-p class function)
      (mode-info-describe-function-internal
       class function (memq major-mode '(Info-mode help-mode))))))

(defun mode-info-elisp-make-index ()
  "Make index of Info files listed in `mode-info-elisp-titles'."
  (interactive)
  (dolist (package mode-info-elisp-packages)
    (ignore-errors (require package)))
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
