;;; mi-emacs.el --- Mode-info backend for emacs

;; Copyright (C) 2002,2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: emacs info

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
;; to Emacs-Lisp.  These titles are available with this package.


;;; Code:

(require 'mode-info)
(require 'mi-elisp)
(eval-when-compile
  (require 'cl)
  (require 'mi-config) ; For mode-info-with-help-buffer
  (require 'mi-index))

(defgroup mode-info-emacs nil
  "Various sorts of improved help system for emacs-lisp-mode."
  :group 'mode-info)

(defcustom mode-info-emacs-index-file
  (expand-file-name "mi-emacs.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about emacs-lisp."
  :group 'mode-info-emacs
  :type 'file)

(defcustom mode-info-emacs-titles
  '(("emacs-ja" "emacs-jp" "emacs"))
  "*Info titles about emacs."
  :group 'mode-info-emacs
  :type mode-info-titles-type)

(mode-info-defclass emacs elisp)

(mode-info-defmethod function-document ((class emacs) function)
  (describe-function function)
  (mode-info-with-help-buffer
    (cons (current-buffer) (point-min))))

(mode-info-defmethod variable-document ((class emacs) variable)
  (describe-variable variable)
  (mode-info-with-help-buffer
    (cons (current-buffer) (point-min))))

(defun mode-info-emacs-goto-info (symbol &optional variable-p)
  (let ((class (mode-info-find-class 'emacs)))
    (mode-info-load-index class)
    (mode-info-show-document
     (save-excursion
       (save-window-excursion
	 (let ((entry (assq symbol
			    (if variable-p
				(mode-info-variable-alist class)
			      (mode-info-function-alist class)))))
	   (when entry
	     (mode-info-goto-info-entry class entry)))))
     t)))

(defun mode-info-emacs-add-function-button (function)
  (let ((buffer-read-only)
	(class (mode-info-find-class 'emacs)))
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
	  (mode-info-elisp-insert-button "[emacs-info]"
					 'mode-info-emacs-goto-info
					 (list function)
					 "mouse-2, Ret: go to Info."))))))

(defun mode-info-emacs-add-variable-button (variable)
  (let ((buffer-read-only)
	(class (mode-info-find-class 'emacs)))
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
	  (mode-info-elisp-insert-button "[emacs-info]"
					 'mode-info-emacs-goto-info
					 (list variable t)
					 "mouse-2, Ret: go to Info."))))))

(defun mode-info-emacs-make-index ()
  "Make index of Info files listed in `mode-info-emacs-titles'."
  (interactive)
  (mode-info-make-index 'emacs mode-info-emacs-titles nil nil))

(mode-info-defmethod process-index-node ((class emacs) title nodename
					 functions variables)
  (cond
   ((string= nodename "Command Index")
    (while (re-search-forward mode-info-index-entry-regexp nil t)
      (mode-info-process-index-node-1 title
				      functions
				      (match-string 1)
				      (match-string 2)
				      (match-string 3))))
   ((string= nodename "Variable Index")
    (while (re-search-forward mode-info-index-entry-regexp nil t)
      (mode-info-process-index-node-1 title
				      variables
				      (match-string 1)
				      (match-string 2)
				      (match-string 3))))))

(provide 'mi-emacs)

;;; mi-emacs.el ends here
