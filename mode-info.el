;;; mode-info.el --- Describe functions and variables

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: info

;; This file is the main part of mode-info.

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


;;; Code:

(require 'custom)
(require 'mi-util)

(eval-when-compile
  (require 'cl)
  (defvar Info-index-alternatives))

(eval-and-compile
  (autoload 'Info-index-next "info")
  (autoload 'Info-goto-node "info")
  (autoload 'Info-mode "info")
  (autoload 'mode-info-make-index "mi-index"))

(defgroup mode-info nil
  "Various sorts of improved help system for major modes."
  :group 'help
  :group 'docs)

(defcustom mode-info-index-directory
  (if (fboundp 'locate-data-directory)
      (locate-data-directory "mode-info")
    (let ((dir (expand-file-name "mode-info/" data-directory)))
      (if (file-directory-p dir)
	  dir
	(file-name-directory (locate-library "mode-info.el")))))
  "*Directory in whuch all indices of `mode-info' are stored."
  :group 'mode-info
  :type 'directory)

(defcustom mode-info-index-coding-system
  (mode-info-static-if (boundp 'MULE) '*iso-2022-jp* 'iso-2022-7bit)
  "Coding system of index files."
  :group 'mode-info
  :type 'coding-system)

(defcustom mode-info-split-window t
  "*Non-nil means window will be split vertically to display descriptions."
  :group 'mode-info
  :type 'boolean)

(defcustom mode-info-select-window nil
  "*Non-nil means window will be selected when display descriptions."
  :group 'mode-info
  :type 'boolean)

(defvar mode-info-mode-alist
  '((emacs-lisp-mode . elisp)
    (lisp-interaction-mode . elisp)
    (perl-mode . perl)
    (cperl-mode . perl)
    (eperl-mode . perl)
    (c-mode . libc)
    (ruby-mode . ruby))
  "Alist of major modes and mode-info backends.")

(defun mode-info-read-mode (&optional prompt)
  (car (rassq (intern
	       (completing-read (or prompt "Language: ")
				(mapcar (lambda (x)
					  (list (symbol-name (cdr x))))
					mode-info-mode-alist)
				nil t))
	      mode-info-mode-alist)))

(defun mode-info-new (mode)
  (let ((name (or (cdr (assq mode mode-info-mode-alist))
		  'elisp)))
    (or (mode-info-find-class name)
	(progn
	  (require (intern (concat "mi-" (symbol-name name))))
	  (mode-info-find-class name)))))

;;; Declaration of basic class for mode-info backends.
(mode-info-defgeneric index-file-name (class)
  "Return the index file name of functions and variables.")

(mode-info-defmethod index-file-name ((class mode-info))
  (let ((v (intern-soft (concat mode-info-prefix
				(mode-info-class-name class)
				"-index-file"))))
    (and v (boundp v) (symbol-value v))))

(defmacro mode-info-function-alist (class)
  `(get ,class 'function-alist))
(defmacro mode-info-function-regexp (class)
  `(get ,class 'function-regexp))
(defmacro mode-info-variable-alist (class)
  `(get ,class 'variable-alist))
(defmacro mode-info-variable-regexp (class)
  `(get ,class 'variable-regexp))

(mode-info-defgeneric load-index (class)
  "Load index of functions and variables described in Info.")

(mode-info-defmethod load-index ((class mode-info))
  (unless (get class 'index-file-loaded)
    (let ((name (mode-info-index-file-name class))
	  function-alist function-regexp
	  variable-alist variable-regexp)
      (when (file-exists-p name)
	(load-file name)
	(setf (mode-info-function-alist class) function-alist)
	(setf (mode-info-function-regexp class) function-regexp)
	(setf (mode-info-variable-alist class) variable-alist)
	(setf (mode-info-variable-regexp class) variable-regexp)
	(put class 'index-file-loaded t)))))

(mode-info-defgeneric function-at-point (mode)
  "Return a function around point or else called by the list containing point.
If that doesn't give a function, return nil.")

(mode-info-defgeneric read-function (mode)
  "Read a function from the minibuffer.")

(mode-info-defgeneric function-document (mode function)
  "Return the marker which points the top of the FUNCTION's document.")

(mode-info-defmethod function-document ((class mode-info) function)
  (mode-info-load-index class)
  (let ((entry (assoc function (mode-info-function-alist class))))
    (when entry
      (mode-info-goto-info-entry class entry))))

(defun mode-info-describe-function (function &optional mode)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((mode (if current-prefix-arg (mode-info-read-mode) major-mode)))
     (list (mode-info-read-function (mode-info-new mode)) mode)))
  (mode-info-show-document
   (save-excursion
     (save-window-excursion
       (or (mode-info-function-document (mode-info-new (or mode major-mode))
					function)
	   (error "Undocumented function: %s" function))))))

(mode-info-defgeneric variable-at-point (mode)
  "Return the bound variable symbol found around point.
Return nil if there is no such symbol.")

(mode-info-defgeneric read-variable (mode)
  "Read a variable from the minibuffer.")

(mode-info-defgeneric variable-document (mode variable)
  "Return the marker which points the top of the VARIABLE's document.")

(mode-info-defmethod variable-document ((class mode-info) variable)
  (mode-info-load-index class)
  (let ((entry (assoc variable (mode-info-variable-alist class))))
    (when entry
      (mode-info-goto-info-entry class entry))))

(defun mode-info-describe-variable (variable &optional mode)
  "Display the full documentation of VARIABLE (a symbol)."
  (interactive
   (let ((mode (if current-prefix-arg (mode-info-read-mode) major-mode)))
     (list (mode-info-read-variable (mode-info-new mode)) mode)))
  (mode-info-show-document
   (save-excursion
     (save-window-excursion
       (or (mode-info-variable-document (mode-info-new (or mode major-mode))
					variable)
	   (error "Undocumented variable: %s" variable))))))

(defun mode-info-show-document (marker)
  "Display the document pointed by MARKER."
  (let ((org (selected-window))
	(new (or (get-buffer-window (marker-buffer marker))
		 (if mode-info-split-window
		     (if (one-window-p)
			 (split-window)
		       (next-window))
		   (selected-window)))))
    (unless (eq org new)
      (unless (pos-visible-in-window-p)
	(recenter (mode-info-static-if (>= emacs-major-version 20)
		      (if (> (point) (window-end nil t)) -3 2)
		    (/ (window-height) 2))))
      (select-window new))
    (with-current-buffer (marker-buffer marker)
      (set-window-buffer new (current-buffer))
      (goto-char marker)
      (push-mark marker t t)
      (unless (bobp)
	(recenter 2)))
    (unless mode-info-select-window
      (select-window org))
    (set-marker marker nil)))

(defsubst mode-info-goto-info-entry-1 (entry &optional interactive-select)
  (if (<= (length entry) 3)
      (progn
	(Info-goto-node (nth 1 entry))
	(goto-char (point-min))
	(forward-line (nth 2 entry)))
    (setq Info-index-alternatives
	  (let ((node (cdr entry)) (alt))
	    (while node
	      (setq alt (cons (list
			       (if (symbolp (car entry))
				   (symbol-name (car entry))
				 (car entry))
			       (car node)
			       (car node)
			       (car (cdr node)))
			      alt)
		    node (cdr (cdr node))))
	    (nreverse alt)))
    (when interactive-select
      (let* ((completion-ignore-case t)
	     (table (mapcar
		     (lambda (e)
		       (cons (if (string-match "(\\([^)]*\\))" (nth 1 e))
				 (substring (nth 1 e) (match-end 0))
			       (nth 1 e))
			     e))
		     Info-index-alternatives))
	     (top (cdr (assoc
			(completing-read "node: " table nil t nil nil)
			table))))
	(setq Info-index-alternatives
	      (cons top
		    (delq top Info-index-alternatives)))))
    (Info-index-next 0))
  (point-marker))

(defun mode-info-goto-info-entry (class entry &optional interactive-select)
  (mode-info-static-if (>= emacs-major-version 20)
      (progn
	(set-buffer (get-buffer-create
		     (concat "*info*<" (mode-info-class-name class) ">")))
	(unless (eq major-mode 'Info-mode)
	  (Info-mode))
	(mode-info-goto-info-entry-1 entry interactive-select))
     (info)
     (mode-info-goto-info-entry-1 entry interactive-select)))

(provide 'mode-info)

;;; mode-info.el ends here
