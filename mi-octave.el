;;; mi-octave.el --- Mode-info backend for Octave

;; Copyright (C) 1999 Rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2001,2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Rubikitch <rubikitch@ruby-lang.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: octave info

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

;; Original code was octave-info.el written by
;; Rubikitch <rubikitch@ruby-lang.org>.

;; This file provides mode-info backend stuffs to see the manual of
;; GNU Octave.


;;; Code:

(require 'mode-info)
(eval-when-compile
  (require 'cl)
  (require 'mi-index))

(defgroup mode-info-octave nil
  "Various sorts of imporved help system for Octave."
  :group 'mode-info)

(defcustom mode-info-octave-index-file
  (expand-file-name "mi-octave.idx" mode-info-index-directory)
  "*Index file of functions and variables described in Info about Octave."
  :group 'mode-info-octave
  :type 'file)

(defcustom mode-info-octave-titles
  '("octave")
  "*Info titles about Octave."
  :group 'mode-info-octave
  :type mode-info-titles-type)

(defconst mode-info-octave-entry-regexp
  "^ +-.+\\(Command\\|Function\\|Keyword\\|Variable\\)[^:]*: +\\([^\n=]+= +\\)?\\([^ \t\n]+\\).*$")
(defconst mode-info-octave-entry-pos 3)

(mode-info-defclass octave)

(mode-info-defmethod function-at-point ((class octave))
  (mode-info-load-index class)
  (save-match-data
    (when (re-search-backward (mode-info-function-regexp class) nil t)
      (match-string 1))))

(mode-info-defmethod variable-at-point ((class octave))
  (mode-info-load-index class)
  (save-match-data
    (when (re-search-backward (mode-info-function-regexp class) nil t)
      (match-string 1))))

(defun mode-info-octave-make-index ()
  "Make index of Info files listed in `mode-info-octave-titles'."
  (interactive)
  (mode-info-make-index 'octave
			mode-info-octave-titles
			mode-info-octave-entry-regexp
			mode-info-octave-entry-pos))

(mode-info-defmethod write-index-file ((class octave) functions variables
				       &optional regexp)
  (setq regexp t)
  (mode-info-method-next))

(provide 'mi-octave)

;;; mi-octave.el ends here
