;;; mi-config.el --- a basic configuration of mode-info

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: info

;; This file a part of mode-info.

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

;; This file provides a basic configuration of mode-info.


;;; Code:

(eval-when-compile
  (require 'mi-util))

(eval-and-compile
  (autoload 'mode-info-describe-function "mode-info"
    "Display the full documentation of FUNCTION (a symbol)." t)
  (autoload 'mode-info-describe-variable "mode-info"
    "Display the full documentation of VARIABLE (a symbol)." t)
  (autoload 'mode-info-make-all-indices "mi-index" nil t)
  (autoload 'mode-info-elisp-add-function-button "mi-elisp")
  (autoload 'mode-info-elisp-add-variable-button "mi-elisp"))

(let (current-load-list)
  (mode-info-static-if (featurep 'xemacs)
      (defadvice describe-function
	(after mode-info-elisp-add-function-button activate compile)
	"Add a button which runs `mode-info-describe-function'."
	(mode-info-elisp-add-function-button (ad-get-arg 0)))
    (defadvice describe-function
      (after mode-info-elisp-add-function-button activate compile)
      "Add a button which runs `mode-info-describe-function'."
      (with-current-buffer "*Help*"
	(mode-info-elisp-add-function-button (ad-get-arg 0))))))

(let (current-load-list)
  (mode-info-static-if (featurep 'xemacs)
      (defadvice describe-variable
	(after mode-info-elisp-add-variable-button activate compile)
	"Add a button which runs `mode-info-describe-variable'."
	(mode-info-elisp-add-variable-button (ad-get-arg 0)))
    (defadvice describe-variable
      (after mode-info-elisp-add-variable-button activate compile)
      "Add a button which runs `mode-info-describe-variable'."
      (with-current-buffer "*Help*"
	(mode-info-elisp-add-variable-button (ad-get-arg 0))))))

(provide 'mi-config)

;;; mi-config.el ends here
