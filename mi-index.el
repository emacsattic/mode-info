;;; mi-index.el --- Command to make indices of mode-info

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: info

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

;; This file provides `mode-info-make-index', the function to make
;; indices of mode-info.


;;; Code:

(require 'info)
(require 'mode-info)
(eval-when-compile
  (require 'cl))

(defcustom mode-info-coding-system
  (mode-info-static-if (boundp 'MULE) '*autoconv* 'undecided)
  "*Coding system of Info files."
  :group 'mode-info
  :type 'coding-system)

(defmacro mode-info-target-titles (class)
  `(get ,class 'target-files))
(defmacro mode-info-entry-regexp (class)
  `(get ,class 'entry-regexp))
(defmacro mode-info-entry-pos (class)
  `(get ,class 'entry-pos))
(defmacro mode-info-variable-regexp (class)
  `(get ,class 'variable-regexp))

(defconst mode-info-variable-regexp "\\(Variable\\|Option\\)"
  "Regular expression which match head of variable documents.")

(defun mode-info-make-index (class titles entry-regexp entry-pos
				   &optional variable-regexp)
  (setq class (mode-info-find-class class))
  (setf (mode-info-target-titles class) titles)
  (setf (mode-info-entry-regexp class) entry-regexp)
  (setf (mode-info-entry-pos class) entry-pos)
  (setf (mode-info-variable-regexp class)
	(or variable-regexp mode-info-variable-regexp))
  (mode-info-make-index-file class))

(eval-when-compile
  (defvar Info-directory-list)
  (defvar Info-additional-directory-list))

(defun mode-info-search-info-files (title)
  "Return files which have TITLE."
  (save-window-excursion
    (save-excursion
      (info)))
  (catch 'found-info-files
    (dolist (dir (append Info-directory-list Info-additional-directory-list))
      (when (or (file-readable-p (expand-file-name title dir))
		(file-readable-p (expand-file-name (concat title ".gz") dir)))
	(throw 'found-info-files
	       (directory-files dir t
				(format "\\`%s\\(-[0-9]+\\)?\\(\\.gz\\)?\\'"
					(regexp-quote title))))))))

(defconst mode-info-node-top-regexp
  "\^_\nFile: %s, +Node: +\\([^,\n\t]+\\)[,\n\t]"
  "Regular expression which match heads of Info nodes.")

(mode-info-defgeneric make-index-file (class)
  "Make index of functions and variables described in Info.")

(mode-info-defmethod make-index-file ((class mode-info))
  (let ((functions (make-symbol "function-alist"))
	(variables (make-symbol "variable-alist")))
    (set functions nil)
    (set variables nil)
    (dolist (title (mode-info-target-titles class))
      (with-temp-buffer
	(let ((case-fold-search t)
	      (regexp (format mode-info-node-top-regexp title))
	      (buffer (current-buffer)))
	  (dolist (file (mode-info-search-info-files title))
	    (with-temp-buffer
	      (message "Reading %s ..." file)
	      (let ((coding-system-for-read mode-info-coding-system)
		    (default-process-coding-system
		      (cons mode-info-coding-system mode-info-coding-system)))
		(if (string-match "\\.gz\\'" file)
		    (if (= 0 (call-process "gzip" nil t nil "-dc" file))
			(goto-char (point-min))
		      (error "Can't extract file: %s" file))
		  (insert-file-contents file)))
	      (while (re-search-forward regexp nil t)
		(let ((nodename (match-string 1)))
		  (save-restriction
		    (narrow-to-region (match-beginning 0)
				      (if (search-forward "\n\^_" nil t)
					  (1- (point))
					(point-max)))
		    (goto-char (point-min))
		    (if (string-match "\\(\\`\\| \\)Index\\'" nodename)
			;; Hold index nodes.
			(let ((temp-buffer (current-buffer)))
			  (with-current-buffer buffer
			    (goto-char (point-max))
			    (insert-buffer temp-buffer)))
		      (mode-info-process-node class title nodename
					      functions variables))
		    (goto-char (point-max)))))))
	  ;; Process index nodes.
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (while (re-search-forward regexp nil t)
	      (let ((nodename (match-string 1)))
		(save-restriction
		  (narrow-to-region (match-beginning 0)
				    (if (search-forward "\n\^_" nil t)
					(1- (point))
				      (point-max)))
		  (goto-char (point-min))
		  (mode-info-process-index-node class title nodename
						functions variables)
		  (goto-char (point-max)))))))))
    (dolist (sym (list functions variables))
      (set sym
	   (sort (symbol-value sym)
		 (lambda (a b)
		   (string< (downcase (car a)) (downcase (car b)))))))
    (mode-info-write-index-file class functions variables)))

(mode-info-defgeneric process-node (class title nodename functions variables)
  "Make index for functions and variables described in a node named NODENAME.")

(mode-info-defmethod process-node ((class mode-info) title nodename
				   functions variables)
  (while (re-search-forward (mode-info-entry-regexp class) nil t)
    (let ((entry (match-string (mode-info-entry-pos class))))
      (mode-info-process-node-1 title
				(if (string-match
				     (mode-info-variable-regexp class)
				     (match-string 1))
				    variables
				  functions)
				entry
				nodename
				(- (count-lines (point-min) (point)) 2)))))

(defun mode-info-process-node-1 (title symbol entry node line)
  (setq node (format "(%s)%s" title node))
  (let ((item (assoc entry (symbol-value symbol))))
    (if item
	(unless (member node (cdr item))
	  (set symbol
	       (cons (append item (list node line))
		     (delq item (symbol-value symbol)))))
      (set symbol
	   (cons (list entry node line)
		 (symbol-value symbol))))))

(defconst mode-info-index-entry-regexp
  "^\\* +\\([^\n:]*:*\\):[ \t]*\\([^.\n]*\\)\\.[ \t]*\\([0-9]*\\)"
  "Regular expression which match entries in index nodes.")

(mode-info-defgeneric process-index-node (class title nodename
						functions variables)
  "Make index for functions and variables described in an index node.")

(mode-info-defmethod process-index-node ((class mode-info) title nodename
					 functions variables)
  (cond
   ((string= nodename "Function Index")
    (while (re-search-forward mode-info-index-entry-regexp nil t)
      (mode-info-process-index-node-1 title
				      functions
				      (match-string 1)
				      (match-string 2)
				      (match-string 3))))
   ((string= nodename "Variable Index")
    (while (re-search-forward mode-info-index-entry-regexp nil t)
      (mode-info-process-index-node-1 title
				      variables
				      (match-string 1)
				      (match-string 2)
				      (match-string 3))))))

(defconst mode-info-index-entry-suffix-regexp
  "[ \t]*<[0-9]+>\\'"
  "Regular expression which match suffixes of entries in index nodes.")

(defun mode-info-process-index-node-1 (title symbol entry node line)
  (when (string-match mode-info-index-entry-suffix-regexp entry)
    (setq entry (substring entry (match-beginning 0))))
  (setq node (format "(%s)%s" title node)
	line (string-to-number line))
  (let ((item (assoc entry (symbol-value symbol))))
    (if item
	(unless (member node (cdr item))
	  (set symbol
	       (cons (append item (list node line))
		     (delq item (symbol-value symbol)))))
      (set symbol
	   (cons (list entry node line)
		 (symbol-value symbol))))))

(mode-info-defgeneric write-index-file (class functions variables
					      &optional regexp)
  "Write index file.")

(mode-info-defmethod write-index-file ((class mode-info) functions variables
				       &optional regexp)
  (with-temp-buffer
    (mode-info-dump-symbols functions variables)
    (when regexp
      (mode-info-dump-symbols-as-regexp functions variables))
    (write-region (point-min) (point-max) (mode-info-index-file-name class))))

(defun mode-info-dump-symbols (&rest symbols)
  (set (mode-info-static-if (>= emacs-major-version 20)
	   'buffer-file-coding-system
	 'file-coding-system)
       mode-info-index-coding-system)
  (insert ";; -*- mode: emacs-lisp; coding: "
	  (prin1-to-string mode-info-index-coding-system)
	  "; -*-\n;; This is the index file\n"
	  ";; generated by mode-info-make-index at "
	  (current-time-string)
	  ".\n\n")
  (dolist (sym symbols)
    (insert "(setq "
	    (symbol-name sym)
	    "\n      '("
	    (mapconcat (function prin1-to-string)
		       (symbol-value sym)
		       "\n        ")
	    "))\n")))

(defun mode-info-dump-symbols-as-regexp (&rest symbols)
  (dolist (sym symbols)
    (let ((name (if (string-match "-alist\\'" (symbol-name sym))
		    (concat (substring (symbol-name sym) 0 (match-beginning 0))
			    "-regexp")
		  (error "Invalie variable name: %s" sym)))
	  (value (format "\\b\\(%s\\)\\b"
			 (if (fboundp 'regexp-opt)
			     (funcall 'regexp-opt
				      (mapcar 'car (symbol-value sym)))
			   (mapconcat (lambda (item)
					(regexp-quote (car item)))
				      (symbol-value sym)
				      "\\|")))))
      (insert (format "\(setq %s %s\)\n" name (prin1-to-string value))))))

(provide 'mi-index)

;;; mi-index.el ends here