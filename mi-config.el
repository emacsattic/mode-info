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
  (autoload 'mode-info-find-tag "mode-info"
    "Find TAG and display it." t)
  (autoload 'mode-info-make-all-indices "mi-index" nil t)
  (autoload 'mode-info-elisp-add-function-button "mi-elisp")
  (autoload 'mode-info-elisp-add-variable-button "mi-elisp")
  (autoload 'mode-info-emacs-add-function-button "mi-emacs")
  (autoload 'mode-info-emacs-add-variable-button "mi-emacs")
  (autoload 'mode-info-emacs-goto-info "mi-emacs"))

(put 'mode-info-with-help-buffer 'lisp-indent-function 0)
(put 'mode-info-with-help-buffer 'edebug-form-spec t)
(defmacro mode-info-with-help-buffer (&rest body)
  (if (featurep 'xemacs)
      `(progn ,@body)
    `(with-current-buffer "*Help*" ,@body)))

(let (current-load-list)
  (defadvice describe-function
    (after mode-info-elisp-add-function-button activate compile)
    "Advised by `mode-info'.
Add a button which runs `mode-info-describe-function'."
    (mode-info-with-help-buffer
      (mode-info-elisp-add-function-button (ad-get-arg 0))
      (mode-info-emacs-add-function-button (ad-get-arg 0)))))

(let (current-load-list)
  (defadvice describe-variable
    (after mode-info-elisp-add-variable-button activate compile)
    "Advised by `mode-info'.
Add a button which runs `mode-info-describe-variable'."
    (mode-info-with-help-buffer
      (mode-info-elisp-add-variable-button (ad-get-arg 0))
      (mode-info-emacs-add-variable-button (ad-get-arg 0)))))

(let (current-load-list)
  (mode-info-static-if (not (featurep 'xemacs))
      (defadvice describe-key
	(after mode-info-elisp-add-command-button activate compile)
	"Advised by `mode-info'.
Add a button which runs `mode-info-describe-function'."
	(let ((f (or (string-key-binding (ad-get-arg 0))
		     (key-binding (ad-get-arg 0)))))
	  (when f
	    (mode-info-with-help-buffer
	      (mode-info-elisp-add-function-button f)
	      (mode-info-emacs-add-function-button f)))))))

(defcustom mode-info-emacs-command-node t
  "*Nil means that the advice, `mode-info-emacs-command-node', is deactivated."
  :group 'mode-info
  :type 'boolean)

(let (current-load-list)
  (defadvice Info-goto-emacs-command-node
    (around mode-info-emacs-command-node activate compile)
    "Advised by `mode-info'.
Search documents in Infos specified in `mode-info-emacs-titles',
instead of \"emacs\".  Check `mode-info-emacs-command-node' also."
    (if mode-info-emacs-command-node
	(or (mode-info-emacs-goto-info (ad-get-arg 0))
	    ad-do-it)
      ad-do-it)))

(provide 'mi-config)

;;; mi-config.el ends here
