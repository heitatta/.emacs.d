;;;
;;; COBOL (tiny) mode
;;;
;;;   It provides
;;;     - proper TAB stops
;;;     - some short-cut input
;;;     - start up function "cobol-start"
;;;
;;; Add to your ".emacs"
;;; (autoload 'cobol-mode "cobol" nil nil)
;;; (setq auto-mode-alist (cons (cons "\\.cbl$" 'cobol-mode) auto-mode-alist))
;;;
;;; BUG: The code is so bogus and the function is so trivial.
;;;
;;; (c)1989 HIRATA Tadashi
;;; 

(defvar cobol-mode-map (make-sparse-keymap) nil)

(defun cobol-mode ()
  (interactive)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list 
	'(7 11 15 19 23 27 31 35 39 43 47 51 55 59 63 67 71 75 79))
  (setq mode-name "COBOL")
  (force-mode-line-update)
  (use-local-map cobol-mode-map))

(defvar cobol-format-strings '(
"      ****************************************************
       IDENTIFICATION DIVISION.
       program-id. %s.

      ****************************************************
       ENVIRONMENT DIVISION.
       input-output section.
       file-control.
"
"
      ****************************************************
       DATA DIVISION.
       file section.
"
"       working-storage section.

      ****************************************************
       PROCEDURE DIVISION.

       XMain.
           perform XInit thru End-XInit

           perform XCleanup thru End-XCleanup
           stop run.

       XInit."
".
       End-XInit.
           exit.

       XCleanup.
           close"
".
       End-XCleanup.
           exit.
") nil)

(defun cobol-start ()
  (interactive)
  (let ((pname (read-string "Program ID: "))
        (num-of-input  (string-to-number 
                        (read-string "How many input files: " "1")))
        (num-of-output (string-to-number
                        (read-string "How many output files: " "1")))
        (input-list  nil)
        (output-list nil)
        (fd-list     nil)
        (fd-list2    nil)
        (i 1)
        fd fn)
    (goto-char 0)
    (insert-string (format (car cobol-format-strings) pname))

    ;; get input file information
    (while (<= i num-of-input)
      (setq fd (read-string (format "input handle (%d): " i)))
      (setq fn (read-string (format "input file name (%d): " i)))
      (insert-string (format "           select %s assign to \"%s\"\n" fd fn))
      (insert-string "               organization line sequential.\n")
      (setq fd-list (cons fd fd-list))
      (setq input-list (cons fd input-list))
      (setq i (1+ i)))

    ;; get output file information
    (setq i 1)
    (while (<= i num-of-output)
      (setq fd (read-string (format "output handle (%d): " i)))
      (setq fn (read-string (format "output file name (%d): " i)))
      (insert-string (format "           select %s assign to \"%s\"\n" fd fn))
      (insert-string "               organization line sequential.\n")
      (setq fd-list (cons fd fd-list))
      (setq output-list (cons fd output-list ))
      (setq i (1+ i)))

    ;; print followings
    (insert-string (car (cdr cobol-format-strings)))
    (setq fd-list     (reverse fd-list))
    (setq input-list  (reverse input-list))
    (setq output-list (reverse output-list))
    (setq fd-list2 fd-list)
    (while fd-list
      (insert-string (format "       fd %s.\n       01  .\n\n" (car fd-list)))
      (setq fd-list (cdr fd-list)))
    (insert-string (car (nthcdr 2 cobol-format-strings)))
    (while input-list
      (insert-string (format "\n           open input  %s" (car input-list)))
      (setq input-list (cdr input-list)))
    (while output-list
      (insert-string (format "\n           open output %s" (car output-list)))
      (setq output-list (cdr output-list)))
    (insert-string (car (nthcdr 3 cobol-format-strings)))
    (while fd-list2
      (insert-string (format " %s" (car fd-list2)))
      (setq fd-list2 (cdr fd-list2)))
    (insert-string (car (nthcdr 4 cobol-format-strings)))
    (search-backward "perform XInit thru End-XInit")
    (next-line 1)
    (indent-to-column 11)))

(defun cobol-insert-call-function ()
  (interactive)
  (let ((fname (read-string "Function: "))
	(col (current-column)))
    (insert-string (format "perform %s thru End-%s\n" fname fname))
    (indent-to-column col)))

(defun cobol-insert-function ()
  (interactive)
  (let ((f (read-string "Function name: ")))
    (beginning-of-line)
    (indent-to-column 7)
    (insert-string (format "%s.\n\n" f))
    (indent-to-column 7)
    (insert-string (format "End-%s.\n" f))
    (indent-to-column 11)
    (insert-string "exit.\n")
    (previous-line 3)
    (indent-to-column 11)))

(defun cobol-insert-read ()
  (interactive)
  (let ((fd (read-string "FD: "))
	(col (current-column)))
    (insert-string (format "read %s\n" fd))
    (indent-to-column (+ col 4))
    (insert-string "at end \n")
    (indent-to-column col)
    (insert-string "end-read")
    (previous-line 1)
    (end-of-line)))

(defun cobol-insert-while ()
  (interactive)
  (let ((col (current-column))
	(condition (read-string "Condition: ")))
    (insert-string (format "perform until %s\n\n" condition))
    (indent-to-column col)
    (insert-string "end-perform")
    (previous-line 1) 
    (indent-to-column (+ 4 col))))


(define-key cobol-mode-map "\C-c\C-s" 'cobol-start)
(define-key cobol-mode-map "\C-c\C-c" 'cobol-insert-call-function)
(define-key cobol-mode-map "\C-c\C-f" 'cobol-insert-function)
(define-key cobol-mode-map "\C-c\C-r" 'cobol-insert-read)
(define-key cobol-mode-map "\C-c\C-w" 'cobol-insert-while)
(define-key cobol-mode-map "\C-i" 'tab-to-tab-stop)

(provide 'cobol-mode)
