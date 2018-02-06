;;;
;;; Set Jisyos
;;;
(setq skk-initial-search-jisyo "~/.emacs.d/dict/skk-initial-jisyo")
(setq skk-jisyo "~/.skk/skk-jisyo")
(setq skk-large-jisyo "~/.emacs.d/dict/SKK-JISYO.L")
;(setq skk-server-host "cosmos")
;(setq skk-aux-large-jisyo "~/lib/elisp/skk-jisyo.L")
;(setq skk-portnum 1178)
;(setq skk-server-prog "/usr/local/bin/pskkserv")
;(setq skk-server-jisyo "/usr/local/share/skk/SKK-JISYO.L.db")

;;;
;;; Favorite Settings
;;;
(setq skk-show-annotation t)
(setq skk-allow-spaces-newlines-and-tabs nil)
(setq skk-auto-start-henkan nil)
(setq skk-auto-insert-paren nil)
(setq skk-isearch-start-mode 'ascii)
(setq skk-isearch-use-previous-mode nil)
;(skk-dabberv-like-completion t)
;(setq skk-use-vip t)
(setq skk-echo t)
(setq skk-kakutei-early t)

;; skk-henkan-okuri-strictly は功罪ある機能だが、
;; skk-henkan-strict-okuri-precedence は折衷案で便利
;(setq skk-henkan-okuri-strictly t)
(setq skk-henkan-okuri-strictly nil)
(setq skk-henkan-strict-okuri-precedence t)
(setq skk-auto-okuri-process nil)
(setq skk-convert-okurigana-into-katakana nil)
(setq skk-delete-okuri-when-quit nil)
(setq skk-process-okuri-early nil)

;; instantly を指定するので、もはや save-count に意味はないが、
;; private-jisyo 設定には save-count の設定が必須なので。
(setq skk-jisyo-save-count 100)
(setq skk-save-jisyo-instantly t)
(setq skk-share-private-jisyo t)
(setq skk-compare-jisyo-size-when-saving t)
(setq skk-backup-jisyo "~/.skk/skk-jisyo.BAK")

(setq skk-keep-record t)
(setq skk-record-file "~/.skk/skk-record")

(setq skk-date-ad t)
(setq skk-number-style nil)

(setq skk-use-color-cursor nil)
(setq skk-use-face nil)

;;;
;;; Roma-Kana Conversion
;;;
(setq skk-rom-kana-rule-list
  '(("n'" nil ("ン" . "ん"))
    ("nn" nil ("ン" . "ん"))
    ("xw" "*" ("" . ""))
    ("*i" nil ("ヰ" . "ゐ"))
    ("*e" nil ("ヱ" . "ゑ"))
    ("@" nil "＠")
    ("*" nil "*")
    ("#" nil "＃")
    ;; ("~" nil "〜")
    ("\\" nil "\\")
    ("z\\" nil skk-list-chars)
    ("$" nil "$")
    (":" nil ":")
    (";" nil ";")
    ("?" nil "?")
    ("z3" nil "③")
    ("z4" nil "④")
    ("z5" nil "⑤")
    ("z6" nil "⑥")
    ("z7" nil "⑦")
    ("z8" nil "⑧")
    ("z9" nil "⑨")
    ("z10" nil "⑩")
    ("z11" nil "⑪")
    ("z12" nil "⑫")
    ("z13" nil "⑬")
    ("z14" nil "⑭")
    ("z15" nil "⑮")
    ("z16" nil "⑯")
    ("z17" nil "⑰")
    ("z18" nil "⑱")
    ("z19" nil "⑲")
    ("z20" nil "⑳")
    ("z1" "" "①")
    ("z2" "" "②")
    ("jf" nil ("平田です。" . "平田です。"))
    ("js" nil 
     ("平田＠IT基盤技術部です。" . "平田＠IT基盤技術部です。"))
    ("jn" nil ("平田＠NRI です。" . "平田＠NRI です。"))))

;;;
;;; Add maru-suji to skk-num-type-alist
;;;
(defun skk-num-maru-suji (num)
  (let ((s "①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳")
	(n (string-to-number num)))
    (when (and (>= n 1) (<= n 20))
      (let ((m (1- n)))
	(substring s m (1+ m))))))
(add-to-list 'skk-num-type-alist '(6 . skk-num-maru-suji))

;;;
;;; Hack for skk-henkan-okuri-strictly
;;;
(add-hook 'minibuffer-setup-hook
          (function
           (lambda ()
             (if (and (boundp 'skk-henkan-okuri-strictly)
                      skk-henkan-okuri-strictly
                      (not (eq last-command 'skk-purge-jisyo)) )
                 (progn
                   (setq skk-henkan-okuri-strictly nil)
                   (put 'skk-henkan-okuri-strictly 'temporary-nil t) )))))

(add-hook 'minibuffer-exit-hook
          (function
           (lambda ()
             (if (get 'skk-henkan-okuri-strictly 'temporary-nil)
                 (progn
                   (put 'skk-henkan-okuri-strictly 'temporary-nil nil)
                   (setq skk-henkan-okuri-strictly t) )))))

(add-hook 'isearch-mode-hook
          (function (lambda ()
                      (and (boundp 'skk-mode) skk-mode
                           (skk-isearch-mode-setup) ))))
(add-hook 'isearch-mode-end-hook
          (function (lambda ()
                      (and (boundp 'skk-mode) skk-mode
                           (skk-isearch-mode-cleanup)
                           (skk-set-cursor-color-properly) ))))
