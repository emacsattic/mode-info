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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


;;; Commentary:

;; This file provides stuffs to fontify Info pages.  In order to use
;; this program, put this expression to your ~/.emacs.
;;
;;     (require 'mi-fontify)


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
  `(("^ --? [A-Z]\\([A-Z.0-9]*\\|[a-z]*\\)\
\\( [A-Za-z][a-z]*\\)*\\( `[A-Za-z]*'\\)?:.*$"
     0 'mode-info-fontify-keyword-face t)
    (,(concat
       "^[ \t]-+[ \t]+"
       (regexp-opt '("プレフィックスコマンド" "コマンド") t)
       ":.*$")
     0 'mode-info-fontify-keyword-face t))
  "Rules for highlighting keywords in Info pages.")

(defvar mode-info-fontify-references
  `(("^\\(\\* \\)\\([^:]+:+\\)"
     (1 'mode-info-fontify-entry-face t)
     (2 'mode-info-fontify-entry-body-face t))
    ("^\\* Menu:$" 0 'mode-info-fontify-entry-face t)
    ("\\(\\*[Nn]ote\\b\\)\\([^:]+:+\\)"
     (1 'mode-info-fontify-xref-face t)
     (2 'mode-info-fontify-xref-body-face t)))
  "Rules for highlighting references in Info pages.
Note: this variable is ignored when using Emacs22 or later.")

(let (current-load-list)
  (defadvice Info-fontify-node
    (around mode-info-fontify-node activate compile)
    "Advised by `mode-info'.
Highlight Info pages based on the value of `mode-info-fontify-keywords'."
    (let ((buffer-read-only))
      (unless font-lock-set-defaults
	(let ((font-lock-defaults
	       (list (if (facep 'info-xref)
			 mode-info-fontify-keywords
		       (append mode-info-fontify-references
			       mode-info-fontify-keywords))
		     t)))
	  (font-lock-set-defaults)))
      (font-lock-default-unfontify-region (point-min) (point-max))
      ad-do-it
      (font-lock-fontify-keywords-region (point-min) (point-max) nil)
      (set-buffer-modified-p nil))))

(let (current-load-list)
  (defadvice Info-goto-node
    (around retry-japanese-node-name activate compile)
    "Advised by `mode-info'.
When NODENAME includes spaces surrounded by Japanese characters and
such node is not found, retry for nodename removed spaces."
    (condition-case err
	ad-do-it
      (error
       (or (and (string-match
		 "\\`No such anchor in tag table or node in tag table or file:"
		 (error-message-string err))
		(string-match "\\cj\\([ \t\r\f\n]+\\)\\cj" (ad-get-arg 0))
		(Info-goto-node
		 (concat (substring (ad-get-arg 0) 0 (match-beginning 1))
			 (substring (ad-get-arg 0) (match-end 1)))
		 (ad-get-arg 1)))
	   (signal (car err) (cdr err)))))))

(provide 'mi-fontify)

;;; mi-fontify.el ends here
