; -*- mode: lisp-interaction; syntax: elisp -*-
;;;
;;; ~/.utils
;;;
;;; 2003.1/19
;;;
;(setq debug-on-error t)

;;
;; パラグラフ内の改行を削除
;;
(defun remove-return ()
  (interactive)
  ; (backward-paragraph)
  ; (forward-char)
  (while (not (string-equal (buffer-substring (point) (+ (point) 1)) "\n"))
    (end-of-line)
    (delete-char 1))
  (forward-char))

;;
;; NRI SPAM 選択メールにチェックをつける
;;
(defun nrispam ()
  "Mark all spam mails in the nri spam filter mail"
  (interactive)
  (if (string-equal "+draft/" (substring (buffer-name (current-buffer)) 0 7))
      (progn
       (end-of-buffer)
       (let ((end (point)))
         (replace-string "[ ] r" "[x] r" nil 0 end))
       (mew-draft-send-message))))

(provide 'utils)
;;;
;;; end of ".elutils"
;;;
