;;; mi-libc.el --- Mode-info backend for Info of libc

;; Copyright (C) 2001,2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: libc info

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

;; This file provides mode-info backend stuffs to see GNU C Library
;; Reference Manual.  It is included in the source package of GNU C
;; Library, which can be downloaded from:
;;
;;    ftp://sourceware.cygnus.com/pub/glibc/releases/


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-libc nil
  "Various sorts of imporved help system for Libc."
  :group 'mode-info)

(defcustom mode-info-libc-index-file
  (expand-file-name "mi-libc.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about libc."
  :group 'mode-info-libc
  :type 'file)

(defcustom mode-info-libc-titles
  '("libc")
  "*Info titles about libc."
  :group 'mode-info-libc
  :type mode-info-titles-type)

(defconst mode-info-libc-entry-regexp
  (eval-when-compile
    (concat
     "^[ \t]+-+[ \t]+\\(\\("
     (mapconcat
      'identity
      '("\\(\\(Obsolete\\|Deprecated\\|POSIX\\.1\\|BSD\\|System[ \t]+V\\)[ \t]+\\)?Function"
	"Macro" "Variable")
      "\\|")
     "\\):\\([ \t]+\\(\\(const\\|static\\|extern\\)[ \t]+\\)?"
     "\\(\\(struct\\|union\\|enum\\)[ \t]+[^ \t]+\\|[^ \t]+_t\\|"
     "\\(\\(\\(unsigned\\|complex\\)[ \t]+\\)?"
     "\\(\\(\\(long[ \t]+\\)?long\\|short\\)[ \t]+\\)?\\("
     (mapconcat
      'identity
      '("void" "char" "short" "int" "long" "double" "float"
	"FILE" "TYPE" "DIR" "ENTRY" "nl_catd")
      "\\|")
     "\\)\\)\\)\\([ \t]+\\*+\\)?\\)?[ \t]+\\([^ \t\n(]+\\)[ \t]*(?\\)")))
(defconst mode-info-libc-entry-pos 18)

(mode-info-defclass libc)

(defun mode-info-libc-word-at-point (alist)
  (let ((orig-table (copy-syntax-table))
	(orig-point (point)))
    (unwind-protect
	(let (word)
	  (modify-syntax-entry ?_ "w")
	  (or (looking-at "\\<") (forward-word -1))
	  (if (assoc (setq word (buffer-substring-no-properties
				 (point) (progn (forward-word 1) (point))))
		     alist)
	      word))
      (goto-char orig-point)
      (set-syntax-table orig-table))))

(mode-info-defmethod function-at-point ((class libc))
  (mode-info-load-index class)
  (mode-info-libc-word-at-point (mode-info-function-alist class)))

(mode-info-defmethod variable-at-point ((class libc))
  (mode-info-load-index class)
  (mode-info-libc-word-at-point (mode-info-variable-alist class)))

(defun mode-info-libc-make-index ()
  "Make index of Info files listed in `mode-info-libc-titles'."
  (interactive)
  (mode-info-make-index 'libc
			mode-info-libc-titles
			mode-info-libc-entry-regexp
			mode-info-libc-entry-pos))

(provide 'mi-libc)

;;; mi-libc.el ends here
