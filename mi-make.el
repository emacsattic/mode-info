;;; mi-make.el --- Mode-info backend for make

;; Copyright (C) 2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: make info

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

;; This file provides mode-info backend stuffes to see the manual of
;; GNU make utility.  It is included in the source package of GNU make
;; utility, which can be downloaded from:
;;
;;     ftp://ftp.gnu.org/gnu/make/
;;
;; Its Japanese version can be downloaded from:
;;
;;     ftp://ftp.ascii.co.jp/pub/GNU/make/make-jp-3.79.1.tgz


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-make nil
  "Various sorts of imporved help system for Make."
  :group 'mode-info)

(defcustom mode-info-make-index-file
  (expand-file-name "mi-make.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about Make."
  :group 'mode-info-make
  :type 'file)

(defcustom mode-info-make-titles
  '(("make-ja" "make-jp" "make"))
  "*Info titles about Make."
  :group 'mode-info-make
  :type mode-info-titles-type)

(mode-info-defclass make)

(mode-info-defmethod function-at-point ((class make))
  (mode-info-load-index class)
  (save-match-data
    (when (re-search-backward (mode-info-function-regexp class) nil t)
      (match-string 1))))

(mode-info-defmethod variable-at-point ((class make))
  (mode-info-load-index class)
  (save-match-data
    (when (re-search-backward (mode-info-function-regexp class) nil t)
      (match-string 1))))

(defun mode-info-make-make-index ()
  "Make index of Info files listed in `mode-info-make-titles'."
  (interactive)
  (mode-info-make-index 'make mode-info-make-titles "" nil))

(mode-info-defmethod process-node ((class mode-info) title node
				   functions variables)
  (while (re-search-forward "\n`\\([^'\n]+\\)'\n" nil t)
    (let ((entry (match-string 1))
	  (line (- (count-lines (point-min) (point)) 2)))
      (when (or (string-match "\\`\\$(\\([-a-z]+\\) " entry)
		(string-match "\\`\\([^ ]+\\) " entry))
	(setq entry (match-string 1 entry)))
      (cond
       ((string-match "\\`\\(Automatic\\|Implicit Variables\\)\\'" node)
	(mode-info-process-index-node-1 title variables
					entry node line))
       ((string-match "\\(\\`Special Targets\\|Syntax\\| Functions\\)\\'" node)
	(mode-info-process-index-node-1 title functions
					entry node line))))))

(mode-info-defmethod process-index-node ((class make) title nodename
					 functions variables)
  (when (string= "Name Index" nodename)
    (while (re-search-forward mode-info-index-entry-regexp nil t)
      (let ((entry (match-string 1))
	    (node (match-string 2))
	    (line (match-string 3))
	    (case-fold-search))
	(cond
	 ((string= "Makefile Names" node))
	 ((string-match "\\`\\([-a-z]+\\|\\.[_A-Z]+\\)\\'" entry)
	  (mode-info-process-index-node-1 title functions
					  entry node line))
	 ((string-match "\\`\\(\\$[\000-\177]+\\|[_A-Z0-9]+\\)\\'" entry)
	  (mode-info-process-index-node-1 title variables
					  entry node line)))))))

(mode-info-defmethod write-index-file ((class make) functions variables
				       &optional regexp)
  (setq regexp t)
  (mode-info-method-next))

(provide 'mi-make)

;;; mi-make.el ends here
