;;; mi-gauche.el --- Mode-info backend for Gauche -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2002,2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: gauche info

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

;; This file provides mode-info backend stuffs to see the reference
;; manual of Gauche, which is a R5RS Scheme implementation developed
;; to be a handy script interpreter.  It can be downloaded from:
;;
;;    http://www.shiro.dreamhost.com/scheme/gauche/


;;; Code:

(require 'mode-info)
(require 'mi-scheme)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-gauche nil
  "Various sorts of imporved help system for Gauche."
  :group 'mode-info)

(defcustom mode-info-gauche-index-file
  (expand-file-name "mi-gauche.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about Gauche."
  :group 'mode-info-gauche
  :type 'file)

(defcustom mode-info-gauche-titles
  '(("gauche-refj" "gauche-refe"))
  "*Info titles about Gauche."
  :group 'mode-info-gauche
  :type mode-info-titles-type)

(mode-info-defclass gauche scheme)

(defun mode-info-gauche-make-index ()
  "Make index of Info files listed in `mode-info-gauche-titles'."
  (interactive)
  (let ((mode-info-index-node-regexp "\\(索引\\|Index\\)\\'")
	(mode-info-index-entry-suffix-regexp "\\([ \t]+of[ \t]+<[^>]+>\\)\\'"))
    (mode-info-make-index 'gauche mode-info-gauche-titles nil nil)))

(mode-info-defmethod process-index-node ((class gauche) title nodename
					 functions variables)
  (cond
   ((string-match "\\`\\(\\(Function and Syntax\\|Module\\|Class\\) Index\\|\
\\(Index +- +\\)?\\(手続きと構文\\|モジュール\\|クラス\\)索引\\)\\'"
		  nodename)
    (while (re-search-forward
	    mode-info-index-entry-regexp nil t)
      (let ((key (match-string 1))
	    (node (match-string 2))
	    (line (match-string 3)))
	(while (string-match "`\\([-/<>+*=]+\\)'" key)
	  (setq key (concat (substring key 0 (match-beginning 0))
			    (match-string 1 key)
			    (substring key (match-end 0)))))
	(mode-info-process-index-node-1 title functions key node line))))
   ((string-match "\\`\\(Variable Index\\|\\(Index +- +\\)?変数索引\\)\\'"
		  nodename)
    (while (re-search-forward mode-info-index-entry-regexp nil t)
      (mode-info-process-index-node-1 title
				      variables
				      (match-string 1)
				      (match-string 2)
				      (match-string 3))))))

(provide 'mi-gauche)

;;; mi-gauche.el ends here
