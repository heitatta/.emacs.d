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
;;   (autoload 'telgate "telgate" "NRI Telephone Book Search" t)
;;   (global-set-key "\C-ce" 'telgate)   ;; for example.

;;; Code:

(defvar telgate-program "/home/hirata/bin/telgate"
  "*Full path name of the \"telgate\" program.")
(defvar telgate-display-height 10
  "*Height of the window displaying the contents of the dictionary.")

(defun telgate (P)
  "Search NRI Yellow Page"
  (interactive "P")
  (save-excursion
	(let* ((cur-buf (mode-line-mode-name))
		   (buffer (get-buffer-create "*NRI Yellow Page*"))
		   (org-buffer (current-buffer))
		   (start (progn 
					(if (not (looking-at "\\<")) (forward-word -1))
					(point)))
		   (end (progn 
				  (forward-word 1) 
				  (point)))
		   (word (buffer-substring start end)))
	  (if P (progn
			  (setq word (read-string "search: " word))))
	  (if (string= word "")
		  (error "you have to specify some words"))
	  (message (format "searching \"%s\"..." word))
	  (if (not (get-buffer-window buffer))
		  (set-window-buffer
		   (split-window nil (- (window-height) telgate-display-height))
		   buffer))
	  (set-buffer buffer)
	  (erase-buffer)
	  (set-default-coding-systems 'euc-japan)
	  ;; 引数付きのときは全社から、そうでない場合は条件付きで検索
	  (if P
		  (call-process telgate-program nil buffer t "-a" word)
		(call-process telgate-program nil buffer t word))
	  (set-buffer-modified-p nil)
	  (message "")
	  ;; 候補が一つ、かつ draft モードだったら置き換えてあげる。
	  (if (and (not P) 
			   (string-match "^\\+draft/" (buffer-name org-buffer))
			   (= (count-lines 1 (point-max)) 1))
		  (let ((data (buffer-substring 1 (point-max)))
				(match))
			(string-match ": [^,]*, " data)
			(setq match (substring data 
								   (+ (match-beginning 0) 2) 
								   (- (match-end 0) 2)))
			(set-buffer org-buffer)
			(goto-char start)
			(delete-char (- end start))
			(insert-string match)))
	  )))

;;; telgate.el ends here
