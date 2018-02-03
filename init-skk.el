;;;
;;; Set Jisyos
;;;
(setq skk-initial-search-jisyo "~/.skk/skk-initial-jisyo")
(setq skk-jisyo "~/.skk/skk-jisyo")
(setq skk-large-jisyo "~/.skk/SKK-JISYO.L")
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
(setq skk-backup-jisyo "~/.skk/skk-jisyo.BAK")
(setq skk-compare-jisyo-size-when-saving nil)
(setq skk-date-ad t)
(setq skk-henkan-okuri-strictly t)
(setq skk-isearch-start-mode 'ascii)
(setq skk-isearch-use-previous-mode nil)
(setq skk-jisyo-save-count 100)
(setq skk-number-style nil)
(setq skk-record-file "~/.skk/skk-record")
(setq skk-use-color-cursor nil)
   (defun skk-set-cursor-color-properly ()) ;; bug...
(setq skk-use-face nil)
;(skk-dabberv-like-completion t)
;(setq skk-use-vip t)


;;;
;;; Favorite Settings (by Default)
;;;
;(setq skk-auto-okuri-process nil)
;(setq skk-convert-okurigana-into-katakana nil)
;(setq skk-delete-okuri-when-quit nil)
;(setq skk-echo t)
;(setq skk-kakutei-early t)
;(setq skk-keep-record t)
;(setq skk-process-okuri-early nil)

;;;
;;; Modify Input Vector
;;;
;(add-hook 'skk-mode-hook
;	  '(lambda ()
;	     (define-key skk-j-mode-map "@" 'skk-insert)
;	     (define-key skk-j-mode-map "\\" 'skk-insert)
;	     (define-key skk-j-mode-map "$" 'skk-insert)))
;(aset skk-input-vector ?! nil)
;(aset skk-input-vector ?# "＃")
;(aset skk-input-vector ?% "％")
;(aset skk-input-vector ?: nil)
;(aset skk-input-vector ?; nil)
;(aset skk-input-vector ?? nil)
;(aset skk-input-vector ?@ "＠")
;(aset skk-input-vector ?~ "〜")

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
	("$" nil "$")	
	(":" nil ":")	
	(";" nil ";")	
	("?" nil "?")	
	("jf" nil ("平田です。" . "平田です。"))
	("js" nil 
		("平田＠システム基盤一部です。" . "平田＠システム基盤一部です。"))
	("jn" nil ("平田＠NRI です。" . "平田＠NRI です。"))))

;;;
;;; Date command
;;;
(defun j-date (ad style)
  (let ((skk-date-ad ad)
	(skk-number-style style))
    (skk-today)))

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

