;;; mi-fontify.el --- Stuffs to fontify Info pages -*- coding: iso-2022-7bit; -*-

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

;; This file contains stuffs to fontify Info pages.


;;; Code:

(require 'font-lock)

(defface mode-info-fontify-entry-face
  '((((class color) (background dark)) (:foreground "cyan"))
    (((class color) (background light)) (:foreground "blue")))
  "Used to fontify entries."
  :group 'mode-info)

(defface mode-info-fontify-entry-body-face
  '((((class color) (background dark)) (:foreground "cyan" :bold t))
    (((class color) (background light)) (:foreground "blue" :bold t))
    (t (:bold t)))
  "Used to fontify entries."
  :group 'mode-info)

(defface mode-info-fontify-xref-face
  '((((class color) (background dark)) (:foreground "Plum1"))
    (((class color) (background light)) (:foreground "purple")))
  "Used to fontify cross references."
  :group 'mode-info)

(defface mode-info-fontify-xref-body-face
  '((((class color) (background dark)) (:foreground "Plum1" :bold t))
    (((class color) (background light)) (:foreground "purple" :bold t))
    (t (:italic t)))
  "Used to fontify cross references."
  :group 'mode-info)

(defface mode-info-fontify-keyword-face
  '((((class color) (background dark)) (:foreground "yellow"))
    (((class color) (background light)) (:foreground "firebrick" :underline t))
    (t (:underline t)))
  "Used to fontify keywords."
  :group 'mode-info)

(defvar mode-info-fontify-keywords
  (eval-when-compile
    (if (fboundp 'font-lock-compile-keywords)
	(font-lock-compile-keywords
	 '(("^\\(\\* \\)\\([^:]+:+\\)"
	    (1 'mode-info-fontify-entry-face t)
	    (2 'mode-info-fontify-entry-body-face t))
	   ("^\\* Menu:$" 0 'mode-info-fontify-entry-face t)
	   ("\\(\\*[Nn]ote\\b\\)\\([^:]+:+\\)"
	    (1 'mode-info-fontify-xref-face t)
	    (2 'mode-info-fontify-xref-body-face t))
	   ;; この正規表現は以下の単語に一致する
	   ;;     Prefix Command, Command, Data Type, Deprecated Function,
	   ;;     Obsolete Function, POSIX.1 Function, BSD Function,
	   ;;     System V Function, Function, Global Variable, Local Variable,
	   ;;     Variable, Macro, Method, Special Form, User Option,
	   ;;     Option, プレフィックスコマンド, コマンド.
	   ("^[ ^\t]-+[ \t]+\\(\\([Pp]refix[ \t]+\\)?[Cc]ommand\\|\
[Dd]ata[ \t]+[Tt]ype\\|\\(\\(Deprecated\\|Obsolete\\|POSIX\\.1\\|BSD\\|\
System[ \t]+V\\)[ \t]+\\)?[Ff]unction\\|\\(\\([Gg]lob\\|[Ll]oc\\)al[ \t]+\\)?\
[Vv]ariable\\|[Mm]\\(acro\\|ethod\\)\\|[Ss]pecial[ \t]+[Ff]orm\\|\
\\([Uu]ser[ \t]+\\)?[Oo]ption\\|\\(プレフィックス\\)?コマンド\\):.*$"
	    0 'mode-info-fontify-keyword-face t)))))
  "Rules for highlighting Info pages.")

(let (current-load-list)
  (defadvice Info-fontify-node
    (around mode-info-fontify-node activate compile)
    (let ((buffer-read-only)
	  (font-lock-keywords mode-info-fontify-keywords))
      (font-lock-default-unfontify-region (point-min) (point-max))
      ad-do-it
      (font-lock-fontify-keywords-region (point-min) (point-max) nil))))

(provide 'mi-fontify)

;;; mi-fontify.el ends here
