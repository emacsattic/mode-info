;;; mode-info.el --- Describe functions and variables

;; Copyright (C) 1998-2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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
  (autoload 'mode-info-make-index "mi-index")
  (autoload 'mode-info-make-all-indices "mi-index" nil t))

(defconst mode-info-version "0.0.1"
  "Version number of `mode-info'.")

(defgroup mode-info nil
  "Various sorts of improved help system for major modes."
  :group 'help
  :group 'docs)

(defcustom mode-info-index-directory
  (or (when (fboundp 'locate-data-directory)
	(locate-data-directory "mode-info"))
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

(defcustom mode-info-class-alist
  '((elisp emacs-lisp-mode lisp-interaction-mode)
    (perl perl-mode cperl-mode eperl-mode)
    (libc c-mode c++-mode)
    (ruby ruby-mode)
    (octave octave-mode))
  "*Alist of mode-info classes and major modes."
  :group 'mode-info
  :type '(repeat (cons (symbol :tag "Class")
		       (repeat (symbol :tag "Major mode")))))

(defun mode-info-default-class-name (&optional mode)
  "Decide the default class based on MODE."
  (unless mode
    (setq mode major-mode))
  (or (save-match-data
	(and (eq mode major-mode)
	     (eq mode 'Info-mode)
	     (string-match "\\`\\*info\\*<\\([^>]*\\)>" (buffer-name))
	     (mode-info-find-class (match-string 1 (buffer-name)))))
      (catch 'found-default-class
	(dolist (elem mode-info-class-alist)
	  (when (memq mode (cdr elem))
	    (throw 'found-default-class (car elem)))))))

(defun mode-info-read-class-name (&optional prompt default)
  (unless default
    (setq default (mode-info-default-class-name)))
  (let* ((table (mapcar (lambda (x)
			  (cons (symbol-name (car x)) (car x)))
			mode-info-class-alist))
	 (x (completing-read (if prompt
				 prompt
			       (if default
				   (format "Class (default %s): " default)
				 "Class: "))
			     table nil t)))
    (if (string= x "") default (cdr (assoc x table)))))

(defun mode-info-new (&optional name)
  (unless name
    (setq name (or (mode-info-default-class-name) 'elisp)))
  (or (mode-info-find-class name)
      (progn
	(require (intern (concat "mi-" (symbol-name name))))
	(mode-info-find-class name))))

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

(mode-info-defgeneric load-index (class &optional force)
  "Load index of functions and variables described in Info.")

(mode-info-defmethod load-index ((class mode-info) &optional force)
  (unless (and (not force) (get class 'index-file-loaded))
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

(mode-info-defgeneric function-at-point (class)
  "Return a function around point or else called by the list containing point.
If that doesn't give a function, return nil.")

(mode-info-defgeneric read-function (class &optional prompt default
					   predicate require-match)
  "Read a function from the minibuffer.")

(mode-info-defmethod read-function ((class mode-info) &optional prompt
				    default predicate require-match)
  (mode-info-load-index class)
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
	     (mode-info-function-alist class)
	     predicate require-match)))
    (if (string= x "") default x)))

(mode-info-defgeneric function-described-p (class function)
  "Return t if FUNCTION is described.")

(mode-info-defmethod function-described-p ((class mode-info) function)
  (mode-info-load-index class)
  (and (assoc function (mode-info-function-alist class)) t))

(mode-info-defgeneric function-document (class function)
  "Return the marker which points the top of the FUNCTION's document.")

(mode-info-defmethod function-document ((class mode-info) function)
  (mode-info-load-index class)
  (let ((entry (assoc function (mode-info-function-alist class))))
    (when entry
      (mode-info-goto-info-entry class entry))))

(defun mode-info-describe-function (function &optional class-name keep-window)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((name (if current-prefix-arg
		   (mode-info-read-class-name)
		 (mode-info-default-class-name))))
     (list (mode-info-read-function (mode-info-new name) nil nil nil t)
	   name)))
  (mode-info-show-document
    (save-excursion
     (save-window-excursion
       (or (mode-info-function-document (mode-info-new class-name) function)
	   (error "Undocumented function: %s" function))))
    keep-window))

(mode-info-defgeneric variable-at-point (mode)
  "Return the bound variable symbol found around point.
Return nil if there is no such symbol.")

(mode-info-defgeneric read-variable (class &optional prompt default
					   predicate require-match)
  "Read a variable from the minibuffer.")

(mode-info-defmethod read-variable ((class mode-info) &optional prompt
				    default predicate require-match)
  (mode-info-load-index class)
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
	     (mode-info-variable-alist class)
	     predicate require-match)))
    (if (string= x "") default x)))

(mode-info-defgeneric variable-described-p (class variable)
  "Return t if VARIABLE is described.")

(mode-info-defmethod variable-described-p ((class mode-info) variable)
  (mode-info-load-index class)
  (and (assoc variable (mode-info-variable-alist class)) t))

(mode-info-defgeneric variable-document (mode variable)
  "Return the marker which points the top of the VARIABLE's document.")

(mode-info-defmethod variable-document ((class mode-info) variable)
  (mode-info-load-index class)
  (let ((entry (assoc variable (mode-info-variable-alist class))))
    (when entry
      (mode-info-goto-info-entry class entry))))

(defun mode-info-describe-variable (variable &optional class-name keep-window)
  "Display the full documentation of VARIABLE (a symbol)."
  (interactive
   (let ((name (if current-prefix-arg
		   (mode-info-read-class-name)
		 (mode-info-default-class-name))))
     (list (mode-info-read-variable (mode-info-new name) nil nil nil t)
	   name)))
  (mode-info-show-document
   (save-excursion
     (save-window-excursion
       (or (mode-info-variable-document (mode-info-new class-name) variable)
	   (error "Undocumented variable: %s" variable))))
   keep-window))

(defun mode-info-show-document (marker &optional keep-window)
  "Display the document pointed by MARKER."
  (let ((org (selected-window))
	(new (or (get-buffer-window (marker-buffer marker))
		 (if (or keep-window
			 (not mode-info-split-window))
		     (selected-window)
		   (if (one-window-p)
		       (split-window)
		     (next-window))))))
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
    (unless (or keep-window mode-info-select-window)
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
  (mode-info-static-if
      (and (not (featurep 'xemacs)) (> emacs-major-version 20))
      ;; When Emacs21 is used, header line and font decoration cause
      ;; a gap of line number.
      (let ((end (point))
	    (start (progn (forward-line -4) (point))))
	(goto-char end)
	(if (search-backward (if (symbolp (car entry))
				 (symbol-name (car entry))
			       (car entry))
			     start t)
	    (forward-line 0)
	  (goto-char end))))
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
