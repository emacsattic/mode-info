;;; mi-scheme.el --- Mode-info backend for scheme

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: scheme info

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

;; This file provides interface commands of Info which describes Scheme.


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-scheme nil
  "Various sorts of imporved help system for Scheme."
  :group 'mode-info)

(defcustom mode-info-scheme-index-file
  (expand-file-name "mi-scheme.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about Scheme."
  :group 'mode-info-scheme
  :type 'file)

(defcustom mode-info-scheme-titles
  '("slib")
  "*Info titles about Scheme."
  :group 'mode-info-scheme
  :type mode-info-titles-type)

(defconst mode-info-scheme-entry-regexp
  "^ --? \\(Syntax\\|Function\\|Macro\\|Procedure\\|Variable\\):[ \t]+\\([^ \t\n]+\\)[ \t\n]")
(defconst mode-info-scheme-entry-pos 2)

(mode-info-defclass scheme)

(defun mode-info-scheme-word-at-point (class &optional variable-p)
  (ignore-errors
    (let ((word
	   (save-excursion
	     (cond
	      ((looking-at "\\s(")
	       (skip-chars-forward "\\s("))
	      ((looking-at "\\s)")
	       (backward-sexp)
	       (skip-chars-forward "\\s("))
	      (t
	       (backward-sexp)))
	     (buffer-substring-no-properties
	      (point) (progn (forward-sexp) (point))))))
      (when (if variable-p
		(mode-info-variable-described-p class word)
	      (mode-info-function-described-p class word))
	word))))

(mode-info-defmethod function-at-point ((class scheme))
  (mode-info-scheme-word-at-point class))

(mode-info-defmethod variable-at-point ((class scheme))
  (mode-info-scheme-word-at-point class t))

(defun mode-info-scheme-make-index ()
  "Make index of Info files listed in `mode-info-scheme-titles'."
  (interactive)
  (mode-info-make-index 'scheme
			mode-info-scheme-titles
			mode-info-scheme-entry-regexp
			mode-info-scheme-entry-pos))

(mode-info-defmethod process-index-node ((class scheme) title nodename
					 functions variables)
  (if (string= title "slib")
      (when (and (string= nodename "Index")
		 (search-forward "\nVariable Index\n" nil t))
	(narrow-to-region (point-min) (match-beginning 0))
	(goto-char (point-min))
	(setq nodename "Function Index")
	(mode-info-method-next)
	(goto-char (point-max))
	(widen)
	(narrow-to-region (point)
			  (or (search-forward "\nConcept and Feature Index\n"
					      nil t)
			      (point-max)))
	(goto-char (point-min))
	(setq nodename "Variable Index")
	(mode-info-method-next)
	(widen))
    (mode-info-method-next)))

(provide 'mi-scheme)

;;; mi-scheme.el ends here
