;;; mi-perl.el --- Mode-info backend for perl-mode -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1998-2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: perl info

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

;; This file provides interface commands of Info which describes Perl.
;; It can be downloaded from:
;;
;;    http://namazu.org/~tsuchiya/doc/perl5.000texi-j.tar.gz


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-perl nil
  "Various sorts of imporved help system for perl-mode."
  :group 'mode-info)

(defcustom mode-info-perl-index-file
  (expand-file-name "mi-perl.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about Perl."
  :group 'mode-info-perl
  :type 'file)

(defcustom mode-info-perl-titles
  '("perl-ja")
  "*Info titles about Perl."
  :group 'mode-info-perl
  :type mode-info-titles-type)

(defconst mode-info-perl-entry-regexp "\
^[ \t]+-+[ \t]+\\(Function\\|Command\\|Prefix[ \t]+Command\\|コマンド\\|\
Special[ \t]+form\\|Macro\\|Method\\|Variable\\|Global[ \t]+Variable\\|\
Local[ \t]+Variable\\|User[ \t]+Option\\):[ \t]+\\([^ \t\n]+\\)[ \t\n]")
(defconst mode-info-perl-entry-pos 2)

(defun mode-info-perl-word-at-point (alist)
  (let ((orig-table (copy-syntax-table))
	(orig-point (point)))
    (unwind-protect
	(let (word)
	  (modify-syntax-entry ?$ "w")
	  (modify-syntax-entry ?@ "w")
	  (modify-syntax-entry ?% "w")
	  (modify-syntax-entry ?# "w")
	  (modify-syntax-entry ?_ "w")
	  (modify-syntax-entry ?^ "w")
	  (modify-syntax-entry ?: "w")
	  (modify-syntax-entry ?& "w")
	  (modify-syntax-entry ?/ "w")
	  (modify-syntax-entry ?! "w")
	  (or (looking-at "\\<") (forward-word -1))
	  (if (assoc (setq word (buffer-substring-no-properties
				 (point) (progn (forward-word 1) (point))))
		     alist)
	      word))
      (goto-char orig-point)
      (set-syntax-table orig-table))))

(mode-info-defclass perl)

(mode-info-defmethod function-at-point ((class perl))
  (mode-info-load-index class)
  (mode-info-perl-word-at-point (mode-info-function-alist class)))

(mode-info-defmethod variable-at-point ((class perl))
  (mode-info-load-index class)
  (mode-info-perl-word-at-point (mode-info-variable-alist class)))

(defun mode-info-perl-make-index ()
  "Make index of Info files listed in `mode-info-perl-titles'."
  (interactive)
  (mode-info-make-index 'perl
			mode-info-perl-titles
			mode-info-perl-entry-regexp
			mode-info-perl-entry-pos))

(provide 'mi-perl)

;;; mi-perl.el ends here
