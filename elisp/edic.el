;;; edic.el --- Interface to the English-Japanese Dictionary

;; Copyright (C) 1996 Hirata Tadashi

;; Keywords: edic, dictionary

;; This file isn't part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; In your ".emacs" file, insert the follows
;;
;;   (autoload 'edic "edic" "Simple English-Japanese Dictionary" t)
;;   (global-set-key "\C-ce" 'edic)   ;; for example.
;;
;; and just type C-ce <word> <return>.
;; If you don't specify the <word> i.e. just typing C-ce <return>, 
;; the word under the cursor will be serched.

;;; Code:

(defvar edic-program "/Home/lib/ejdic.exe"
  "*Full path name of the \"grep\" program used to search the word.")
(defvar edic-dictionary "/Home/lib/ejdic"
  "*Dictionary file used in \"edic\" function")
(defvar edic-display-height 5
  "*Height of the window displaying the contents of the dictionary.")

(defun edic (P)
  "Search the WORD in English-Japanese Dictionary (specified as
\"edic-dictionary\") and display the contents.  If you don't
specify WORD, search the word under the cursor."
  (interactive "P")
  (save-excursion
    (let ((buffer (get-buffer-create "*Edic*"))
	  (word (if P
		    (read-string "word: ")
		  (let* ((start (progn (if (not (looking-at "\\<"))
					   (forward-word -1))
				       (point)))
			 (end (progn (forward-word 1)
				     (point))))
		    (buffer-substring start end)))))
      (if (string= word "")
	  (error "you have to specify some words"))
      (message (format "searching \"%s\"..." word))
      (if (not (get-buffer-window buffer))
	  (set-window-buffer
	   (split-window nil (- (window-height) edic-display-height))
	   buffer))
      (set-buffer buffer)
      (erase-buffer)
      (set-default-coding-systems 'euc-japan)
      ;;(call-process shell-file-name  edic-dictionary buffer t
      ;;	    shell-command-option
      ;;	    (format "%s %s %s" edic-program edic-dictionary word)
      ;;	    )
      (call-process edic-program edic-dictionary buffer t
      	    edic-dictionary (downcase word))
      (set-buffer-modified-p nil)
      (message ""))))

(defun mouse-edic ()
  "\"edic\" with mouse interface. See \"edic\"."
  (interactive)
  (let* ((pos (cdr (mouse-position)))
	 (x (car pos))
	 (y (cdr pos)))
    (move-to-window-line y)
    (forward-char x)
    (call-interactively 'edic)))

;;; edic.el ends here
