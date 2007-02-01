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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


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
  (autoload 'mode-info-make-all-indices "mi-index"
    "Make indices of all available Info documents." t)
  (autoload 'mode-info-elisp-add-function-button "mi-elisp")
  (autoload 'mode-info-elisp-add-variable-button "mi-elisp")
  (autoload 'mode-info-elisp-info-ref "mi-elisp")
  (autoload 'mode-info-emacs-add-function-button "mi-emacs")
  (autoload 'mode-info-emacs-add-variable-button "mi-emacs")
  (autoload 'mode-info-emacs-goto-info "mi-emacs")
  (autoload 'mode-info-key-or-menu-binding "mi-util"))

(defcustom mode-info-advise-describe-commands
  (fboundp 'help-xref-button)
  "*Non-nil means that `mode-info' advises some describing commands,
such as `describe-function', `describe-variable' and `describe-key'."
  :group 'mode-info
  :type 'boolean)

(defcustom mode-info-advise-info-commands t
  "*Non-nil means that `mode-info' advises some Info commands,
such as `Info-goto-emacs-command-node' and `Info-elisp-ref'."
  :group 'mode-info
  :type 'boolean)

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
    (when mode-info-advise-describe-commands
      (mode-info-with-help-buffer
	(mode-info-elisp-add-function-button (ad-get-arg 0))
	(mode-info-emacs-add-function-button (ad-get-arg 0))))))

(let (current-load-list)
  (defadvice describe-variable
    (after mode-info-elisp-add-variable-button activate compile)
    "Advised by `mode-info'.
Add a button which runs `mode-info-describe-variable'."
    (when mode-info-advise-describe-commands
      (mode-info-with-help-buffer
	(mode-info-elisp-add-variable-button (ad-get-arg 0))
	(mode-info-emacs-add-variable-button (ad-get-arg 0))))))

(let (current-load-list)
  (defadvice describe-key
    (after mode-info-elisp-add-command-button activate compile)
    "Advised by `mode-info'.
Add a button which runs `mode-info-describe-function'."
    (when mode-info-advise-describe-commands
      (let ((func (mode-info-key-or-menu-binding (ad-get-arg 0))))
	(unless (or (null func) (integerp func))
	  (mode-info-with-help-buffer
	    (mode-info-elisp-add-function-button func)
	    (mode-info-emacs-add-function-button func)))))))

(let (current-load-list)
  (defadvice Info-goto-emacs-command-node
    (around mode-info-emacs-command-node activate compile)
    "Advised by `mode-info'.
Look up a documentation from `mode-info-emacs-titles' instead of \"emacs\"
when `mode-info-advise-info-commands' is set to a value other than nil."
    (or (when mode-info-advise-info-commands
	  (mode-info-emacs-goto-info (ad-get-arg 0)))
	ad-do-it)))

(let (current-load-list)
  ;; This advice will be useless for FSF Emacsen, because FSF Emacen
  ;; does not have Info-elisp-ref() but XEmacs have.
  (defadvice Info-elisp-ref
    (around mode-info-elisp-info-ref activate compile)
    "Advised by `mode-info'.
Look up a documentation from `mode-info-elisp-titles' instead of \"elisp\"
when `mode-info-advise-info-commands' is set to a value other than nil."
    (or (when mode-info-advise-info-commands
	  (mode-info-elisp-info-ref (ad-get-arg 0)))
	ad-do-it)))

(provide 'mi-config)

;;; mi-config.el ends here
