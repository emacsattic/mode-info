;;; mi-ruby.el --- Mode-info backend for ruby-mode

;; Copyright (C) 1999 Rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2001,2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Rubikitch <rubikitch@ruby-lang.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: ruby info

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

;; Original code was ruby-info.el written by
;; Rubikitch <rubikitch@ruby-lang.org>.

;; This file provides interface commands of Info which describes Ruby.
;; It can be downloaded from:
;;
;;     ftp://ftp.netlab.co.jp/pub/lang/ruby/doc/ruby-texi-1.4.tar.gz


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-ruby nil
  "Various sorts of imporved help system for Ruby."
  :group 'mode-info)

(defcustom mode-info-ruby-index-file
  (expand-file-name "mi-ruby.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about Ruby."
  :group 'mode-info-ruby
  :type 'file)

(defcustom mode-info-ruby-titles
  '("ruby-jp.info")
  "*Info titles about Ruby."
  :group 'mode-info-ruby
  :type mode-info-titles-type)

(defconst mode-info-ruby-entry-regexp
  "^ +-.+\\(Method\\|Function\\|Variable\\)[^:]*: \\([^ \t\n]+\\)")
(defconst mode-info-ruby-entry-pos 2)
(defconst mode-info-ruby-suffix-regexp
  "\\([ \t]+on[ \t+][A-Za-z][A-Za-z0-9_]*\\|[ \t]*<[0-9]+>\\)$")

(mode-info-defclass ruby)

(defun mode-info-ruby-word-at-point (regexp)
  (let ((orig-table (copy-syntax-table))
	(orig-point (point)))
    (unwind-protect
	(let ((case-fold-search nil))
	  (modify-syntax-entry ?$ "w")
	  (modify-syntax-entry ?@ "w")
	  (and (forward-word 1)
	       (re-search-backward regexp nil t)
	       (match-string-no-properties 1)))
      (goto-char orig-point)
      (set-syntax-table orig-table))))

(mode-info-defmethod function-at-point ((class ruby))
  (mode-info-load-index class)
  (mode-info-ruby-word-at-point (mode-info-function-regexp class)))

(mode-info-defmethod variable-at-point ((class ruby))
  (mode-info-load-index class)
  (mode-info-ruby-word-at-point (mode-info-variable-regexp class)))

(defun mode-info-ruby-make-index ()
  "Make index of Info files listed in `mode-info-ruby-titles'."
  (interactive)
  (let ((mode-info-index-entry-suffix-regexp mode-info-ruby-suffix-regexp)
	(max-specpdl-size (* 10 max-specpdl-size)))
    (mode-info-make-index 'ruby
			  mode-info-ruby-titles
			  mode-info-ruby-entry-regexp
			  mode-info-ruby-entry-pos)))

(mode-info-defmethod write-index-file ((class ruby) functions variables
				       &optional regexp)
  (setq regexp t)
  (mode-info-method-next))

(provide 'mi-ruby)

;;; mi-ruby.el ends here
