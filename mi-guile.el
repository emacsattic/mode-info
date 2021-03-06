;;; mi-guile.el --- Mode-info backend for Guile -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: guile info

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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


;;; Commentary:

;; This file provides mode-info backend stuffs to see the reference
;; manual of GNU Guile.  It is an interpreter for the Scheme
;; programming language, developed by GNU.  Its manual can be
;; downloaded from Guile's anonymous CVS.
;;
;;     http://www.gnu.org/software/guile/anon-cvs.html
;;
;; Its Japanese version can be downloaded from:
;;
;;    http://www.kt.rim.or.jp/~kbk/index.html
;;    http://www.kt.rim.or.jp/~kbk/guile/guile1.zip
;;    http://www.kt.rim.or.jp/~kbk/guile/guile2.zip
;;
;; These are translated by KIMURA Koichi <kbk@kt.rim.or.jp>.  I'd like
;; to address my thanks to him for his nice work.


;;; Code:

(require 'mode-info)
(require 'mi-scheme)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-guile nil
  "Various sorts of imporved help system for Guile."
  :group 'mode-info)

(defcustom mode-info-guile-index-file
  (expand-file-name "mi-guile.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about Guile."
  :group 'mode-info-guile
  :type 'file)

(defcustom mode-info-guile-titles
  '(("guile-ja" "guile"))
  "*Info titles about Guile."
  :group 'mode-info-guile
  :type mode-info-titles-type)

(mode-info-defclass guile scheme)

;; Merge native indices and indices of Scheme.
(mode-info-defmethod load-index ((class guile) &optional force)
  (let ((ret (mode-info-method-next)))
    (when ret
      (let ((scheme (mode-info-new 'scheme)))
	(mode-info-load-index scheme)
	(dolist (elem (mode-info-function-alist scheme))
	  (let ((item (assoc (car elem) (mode-info-function-alist class))))
	    (if item
		(setcdr item (append (cdr item) (cdr elem)))
	      (push elem (mode-info-function-alist class)))))
	(dolist (elem (mode-info-variable-alist scheme))
	  (let ((item (assoc (car elem) (mode-info-variable-alist class))))
	    (if item
		(setcdr item (append (cdr item) (cdr elem)))
	      (push elem (mode-info-variable-alist class))))))
      ret)))

(defun mode-info-guile-make-index ()
  "Make index of Info files listed in `mode-info-guile-titles'."
  (interactive)
  (mode-info-make-index 'guile
			mode-info-guile-titles
			mode-info-scheme-entry-regexp
			mode-info-scheme-entry-pos))

(provide 'mi-guile)

;;; mi-guile.el ends here
