;;; -*- Emacs-Lisp -*-
;;;
;;; allen: a typetraining program with "My Friends the Allen" 
;;;        on GNU Emacs.
;;;
;;; allen.el  (Version: 1.0 for GNU Emacs Version 19.23)
;;;

(require 'picture)

(defvar allen-text-directory "c:/home/doc/porno/original"
  "Path of the directory containing text file which will be used.
Must not end with \"/\".")

(defvar allen-fixed-text-file nil
  "Text file for the type training.")

(defvar allen-startup-hook nil
  "*List of hook functions called when start up \"allen\". You can use
this hook to set up your original training text.")
(defvar allen-text-buffer "* Allen Type Trainer *"
  "*Name of text buffer")

(defvar allen-result-buffer "* Allen Result *"
  "*Name of result buffer")

(defun allen ()
  "Tyepe training system with \"My Friends the Allens\"."
  (interactive)
  (run-hooks 'allen-startup-hook)
  (allen-init)
  (delete-other-windows)
  (allen-prepare-buffers)
  (unwind-protect
      (let ((next-step 3)
        (started nil))
    (forward-line 0)
    (recenter 0)
    (while (and (not allen-quit-flag)
            (if started
            (or (y-or-n-p "Continue? ")
                (not (y-or-n-p "Really want to exit? ")))
              (or (y-or-n-p "Are you ready? ")
              (not (y-or-n-p "Really want to exit? ")))))
      (if started
          (forward-line (1- next-step))
        (setq started t))
      (save-excursion
        (narrow-to-region
         (point)
         (progn
           (forward-line (* next-step allen-text-lines))
           (point))))
      (forward-line 1)
      (setq allen-correct-char-count 0)
      (setq allen-whole-char-count 0)
      (setq allen-space-char-count 0)
      (allen-one-session)
      (widen)))
    (allen-finish)))

(defun allen-init ()
    (set-buffer (get-buffer-create allen-text-buffer))
    ;; define local variables
    (set (make-local-variable 'window-min-height) 2)
    (set (make-local-variable 'allen-text-file) nil)
    (set (make-local-variable 'allen-quit-flag) nil)
    (make-local-variable 'allen-number-of-lines)
    (make-local-variable 'allen-correct-char-count)
    (make-local-variable 'allen-whole-char-count)
    (make-local-variable 'allen-space-char-count)
    (set (make-local-variable 'allen-buffer-width) nil)
    (set (make-local-variable 'allen-text-buffer-height) nil)
    (set (make-local-variable 'allen-result-buffer-height) 5)
    (set (make-local-variable 'allen-text-lines) nil)
    (set (make-local-variable 'allen-prev-win-conf) 
     (current-window-configuration))
    ;; set the text file
    (if allen-fixed-text-file
    (setq allen-text-file allen-fixed-text-file)
      (setq allen-text-file
        (let* ((dir (directory-files allen-text-directory))
           (num (length dir))
           (element (% (abs (random t)) (- num 2))))
          (concat allen-text-directory "/"
              (car (nthcdr (+ element 2) dir)))))))

(defun allen-prepare-buffers ()
  (let ((height (- (window-height) 4)))
    (setq allen-text-buffer-height
          (1+ (- height (% height 3)))))
  (setq allen-text-lines (/ (- allen-text-buffer-height 2) 3))
  (setq allen-buffer-width (window-width))
  (erase-buffer)
  (allen-read-file)
  (switch-to-buffer allen-text-buffer)
  (split-window (get-buffer-window (current-buffer))
        ;(1- allen-text-buffer-height))
        allen-text-buffer-height)
  (set-buffer (get-buffer-create allen-result-buffer))
  (erase-buffer)
  (insert "Mistype Ratio       |  \n"
      "Evaluation          |  ")
  (other-window 1)
  (switch-to-buffer allen-result-buffer)
  (other-window 1))

(defun allen-finish ()
  (interactive)
  (let ((allen-win-conf allen-prev-win-conf))
    (kill-buffer allen-text-buffer)   ;; vanish "allen-prev-win-conf" var.
    (kill-buffer allen-result-buffer)
    (message "")
    (set-window-configuration allen-win-conf)))

(defun allen-one-session ()
  (if (eobp) 
      t
    (let ((inhibit-quit 't)
      (echo-keystrokes 0)
      (lines (/ (count-lines (point-min) (point-max))
            3))
      (text-pos (save-excursion
              (forward-line -1)
              (point)))
      (started nil)
      ch start end)
      (setq ch (read-char-exclusive))
      (setq start (nth 1 (current-time)))
      (while (and (> lines 0)
          (/= ch 28));; if ch = ^\ then quit
    (setq allen-whole-char-count (1+ allen-whole-char-count))
    (if (= ch (char-after text-pos))
        (progn
          (setq allen-correct-char-count (1+ allen-correct-char-count))
          (setq text-pos (1+ text-pos))
          (if (= ch 32)
          (setq allen-space-char-count (1+ allen-space-char-count)))
          (if (/= ch 13)
          (progn
            (insert-char ch 1)
            (setq ch (read-char-exclusive)))
        (insert-char 13 1)  ; cr mark
        (setq allen-space-char-count (1+ allen-space-char-count))
        (setq lines (1- lines))
        (if (/= lines 0)
            (progn (forward-line 3)
               (setq text-pos (save-excursion
                        (forward-line -1) (point)))
               (setq ch (read-char-exclusive))))))
      (if (= ch 10)
          (insert " ")
        (if (= ch 12)
        (redraw-display)
          (if (= ch 7)
          (setq quit-flag 'nil)))
        (insert-char ?X 1))
      (backward-char 1)
      (while (and (/= ch (char-after text-pos))
              (/= ch  28))
        (setq ch (read-char-exclusive))
        (delete-char 1)
        (if (= ch 10)
        (insert " ")
          (if (= ch 12)
          (redraw-display)
        (if (= ch 7)
            (setq quit-flag 'nil)))
          (insert-char ?X 1))
        (backward-char 1))
      (picture-move-down 1)
      (insert "^")
      (forward-line -1)
      (end-of-line)
      (backward-delete-char 1)
      (setq text-pos (1+ text-pos))
      (if (/= ch 13)
          (if (= ch 28)
          (setq lines 0)
        (insert-char ch 1)  ; blink or reverse mode, if possible
        (setq ch (read-char-exclusive)))
        (setq lines (1- lines))
        (insert-char 13 1)      ; cr mark
        (if (/= lines 0)
        (progn (forward-line 3)
               (setq text-pos (save-excursion
                    (forward-line -1) (point)))
               (setq ch (read-char-exclusive)))))))
      (setq end (nth 1 (current-time)))
      (recenter -2)
      (if (= ch 28)             ; if ch = '^\' then quit immediately
      (setq allen-quit-flag t)
    (setq allen-quit-flag nil)
    (allen-check-elapsed-time start end
                  allen-whole-char-count
                  allen-correct-char-count
                  allen-space-char-count)))))

(defun allen-check-elapsed-time (s e w crct spc)
  (let ((elapsed (- e s))
    (diff    (- w crct)))
    (if (< elapsed 0)
    (setq elapsed (+ elapsed 65536)))
    (other-window 1)
    (erase-buffer)
    (let ((ratio (* 100 (/ (float diff) w))))
      (insert (format "Mistype Ratio       |  %5.2f%% (%d / %d)\n"
              ratio diff w)
          (format "Evaluation          |  %5.2f (%d word)"
              (1+ (- (* 60 (/ (float w) elapsed)) 
                 (exp (* 2 ratio))))
              (* 60 (/ (float spc) elapsed)))))
    (goto-char (point-min))
    (other-window 1)))

(defun allen-read-file ()
  (message "Reading Allen text file...")
  (let ((fill-column (- allen-buffer-width 3)))
    (call-process shell-file-name allen-text-file t nil "-c"
          "sed -e \"/^$/d\" -e \"s/     / /g\" -e \"s/   */  /g\"")
    (fill-region (point-min) (point-max)))
  ;;(call-process "sed" allen-text-file t nil
  ;;        "\"/^[   ]*$/d\"")
  (untabify (point-min) (point-max))
  (setq allen-number-of-lines (count-lines (point-min) (point-max)))
  (goto-char (point-min))
  (while (not (eobp))
    (end-of-line)
    (insert-char 13 1)
    (insert "\n\n")
    (forward-line 1))
  (goto-char (point-min))
  (message "Reading Allen text file... done."))
