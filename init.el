 ; -*- mode: lisp-interaction; syntax: elisp -*-
;;;
;;; ~/.emacs
;;;
;;; Since 1996.10/8
;;; Update 2018.1/31 (Emacs24-with-package)
;;;
(message "loading .emacs.d/init.el....")
(setq debug-on-error t)

;;;
;;; package.el
;;;
(setq url-proxy
      '(("http" . "camellia.nri.co.jp:8080")
        ("https" . "camellia.nri.co.jp:8080")))
(setq url-proxy-services url-proxy)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(defun toggle-proxy ()
  (interactive)
  (if url-proxy-services
      (progn
        (message "proxy is off")
        (setq url-proxy-services nil))
    (message (format "use proxy: %s" (cdr (assoc "http" url-proxy))))
    (setq url-proxy-services url-proxy)))

(defun require-or-install (pkg)
  (unless (locate-library (symbol-name pkg))
    (package-refresh-contents)
    (package-install pkg))
  (require pkg))

;;;
;;; Load Pathes
;;;
(let ((dir (expand-file-name "~/lib/elisp")))
  (setq load-path (cons dir load-path))
  (let ((default-directory dir))
    (load (expand-file-name "subdirs.el"))))

(if (featurep 'w32-win)
    (setq exec-path
          (append '("c:/cygwin/bin" "c:/cygwin/home/hirata/bin") exec-path))
  (setq exec-path
        (append '("/usr/bin/" "/usr/local/bin" "/opt/local/bin"
                  "/Users/hirata/bin" "/home/hirata/bin")
                exec-path)))

;;;
;;; Require basic libraries
;;;
(require 'utils "~/.emacs.d/utils.el")

;;;
;;; Coding System (Use default)
;;;
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;; 「丸付き数字」が入った JISメールを読むための設定
(require-or-install 'cp5022x)
(coding-system-put 'iso-2022-jp :decode-translation-table
       '(cp51932-decode japanese-ucs-cp932-to-jis-map))

;; charset の判定する際に cp932 を sjis より優先順位を上げておくことで
;; 機種依存文字を表示できるようにする (charset と coding-system の優先度設定)。
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                     'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;;;
;;; Default Mode
;;;
(setq major-mode 'paragraph-indent-text-mode)
(setq initial-major-mode 'paragraph-indent-text-mode)
(add-hook 'text-mode-hook '(lambda ()
                             (turn-on-auto-fill)
                             (paragraph-indent-minor-mode)
                             (setq adaptive-fill-mode nil)))
(define-key text-mode-map "\t" 'tab-to-tab-stop)
(define-key text-mode-map "\M-i" 'indent-relative)

;;;
;;; emacs server
;;;
(when window-system
  (require 'server)
  (server-start))
(setq gnuserv-frame (selected-frame))

;;;
;;; Shell command convention (arguments isn't like Win32).
;;;
(if (featurep 'w32-win)
        (progn
          (setq explicit-shell-file-name "c:/cygwin/bin/bash.exe")
          (setq shell-file-name "c:/cygwin/bin/bash.exe"
                shell-command-option "-c")
          (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8-unix))
  (setq shell-file-name "/usr/bin/zsh")
  (modify-coding-system-alist 'process ".*sh" 'utf-8))

(setq shell-edit-mode-map (make-sparse-keymap))
(defun toggle-shell-edit-mode ()
  "Toggle the key-map in shell-mode. We'd like to use \"C-a\"
for \"comit-bol\", then do so. And when you'd like to edit the
buffer, change the key-map by this function."
  (interactive)
  (cond ((string= mode-name "Shell")
     (setq mode-name "Shell Edit")
     (use-local-map shell-edit-mode-map)
     (force-mode-line-update))
    ((string= mode-name "Shell Edit")
     (setq mode-name "Shell")
     (use-local-map shell-mode-map)
     (goto-char (point-max))
     (force-mode-line-update))
    (t
     nil)))
(define-key shell-edit-mode-map "\C-\\" 'toggle-shell-edit-mode)
(add-hook 'shell-mode-hook
  '(lambda ()
     (define-key shell-mode-map "\C-a"     'comint-bol)
     (define-key shell-mode-map "\C-n"     'comint-next-input)
     (define-key shell-mode-map "\C-p"     'comint-previous-input)
     (define-key shell-mode-map "\C-r"     'comint-previous-matching-input)
     (define-key shell-mode-map "\C-s"     'comint-next-matching-input)
     (define-key shell-mode-map "\C-u"     'comint-kill-input)
     (define-key shell-mode-map "\C-w"     'backward-kill-word)
     (define-key shell-mode-map "\C-z"     'comint-stop-subjob)
     (define-key shell-mode-map "\C-c\C-c" 'comint-interrupt-subjob)
     (define-key shell-mode-map "\C-\\"    'toggle-shell-edit-mode)))
(setq comint-input-ignoredups t)

;;;
;;; Info mode (C-ci)
;;;
(if (featurep 'w32-win)
    (setq emacs-install-dir
          (let ((version (emacs-version)))
            (string-match "Emacs \\([0-9]+\\.[0-9]+\\)" version)
            (format "/Program Files/emacs-%s" (match-string 1 version)))))
(if (featurep 'w32-win)
    (setq Info-directory-list
          (cons (format "%s/info" emacs-install-dir) '("/usr/info")))
  (setq Info-directory-list '("/usr/share/info")
        Info-additional-directory-list '("usr/local/info" "/usr/info"
                                         "/usr/local/share/info"
                                         "/usr/X11R6/info")))

;;;
;;; View mode
;;;
(add-hook 'view-mode-hook
          '(lambda ()
             (define-key view-mode-map "j" 'View-scroll-line-forward)
             (define-key view-mode-map "k" 'View-scroll-line-backward)))

;;;
;;; auto-complete
;;;
(require-or-install 'auto-complete)
(require-or-install 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start t)
(ac-set-trigger-key "C-i")

;;;
;;; Ange FTP
;;;
(setq ange-ftp-default-user "hirata"
      ange-ftp-generate-anonymous-password user-mail-address)
(setq ange-ftp-binary-file-name-regexp
      (concat "\\.[zZ]$\\|\\.lzh$\\|\\.arc$\\|\\.zip$\\|\\.zoo$\\|\\.tar$\\|"
          "\\.dvi$\\|\\.ps$\\|\\.elc$\\|TAGS$\\|"
          "\\.gif$\\|\\.jpe?g$\\|\\.GIF$\\|\\.JPE?DG$\\|"
          "\\.EXE\\(;[0-9]+\\)?$\\|\\.[zZ]-part-..$\\|\\.gz$\\|"
          "\\.taz$\\|\\.tgz$"))

;;
;; archive mode
;;
(setq auto-mode-alist
      (cons '("\\.\\(jar\\|ear\\|war\\)$" . archive-mode) auto-mode-alist))

;;;
;;; font-lock
;;;
(when window-system
  (require 'font-lock)
  (setq font-lock-support-mode 'jit-lock-mode)
  ;(setq font-lock-support-mode 'fast-lock-mode)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode 1))

;;;
;;; whitespace
;;;
(when window-system
  (require 'whitespace)
  (setq whitespace-style
        '(face tabs spaces trailing lines newline empty
               space-mark tab-mark newline-mark))
  (setcdr (assoc 'newline-mark whitespace-display-mappings)
          '(10 [171 10] [36 10]))

  ;; 全角スペースを□で
  (setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
  (setq whitespace-display-mappings
        (cons '(space-mark ?\u3000 [?\u25a1]) whitespace-display-mappings))

  (let ((bc "lemonchiffon2")
        (fc "light gray"))
    (set-face-background 'whitespace-line nil)
    (set-face-foreground 'whitespace-line nil)
    (set-face-background 'whitespace-empty bc)
    (set-face-background 'whitespace-indentation bc)
    (set-face-background 'whitespace-space-after-tab bc)
    (set-face-background 'whitespace-space-before-tab bc)
    (set-face-foreground 'whitespace-indentation fc)
    (set-face-foreground 'whitespace-space-after-tab fc)
    (set-face-foreground 'whitespace-space-before-tab fc)
    (set-face-background 'whitespace-space nil)
    (set-face-foreground 'whitespace-space fc)
    (set-face-background 'whitespace-newline nil)
    (set-face-foreground 'whitespace-newline fc)
    (set-face-background 'whitespace-trailing bc)
    (set-face-foreground 'whitespace-trailing fc))
  (global-whitespace-mode 1))

;;;
;;; Dired
;;;
(require-or-install 'dired-single)
(add-hook 'dired-load-hook
          '(lambda ()
             (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
             (define-key dired-mode-map "f" 'dired-single-buffer)
             (define-key dired-mode-map "^"
               '(lambda nil (interactive) (dired-single-buffer "..")))))

(if (locate-library "sorter")
    (add-hook 'dired-load-hook
              '(lambda () (load "sorter" nil t))))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Junk")

;;;
;;; Calendar (C-cc)
;;;
;; "今日"をマーク
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;; 日本の休日
(add-hook 'calendar-load-hook
          (lambda ()
            (require-or-install 'japanese-holidays)
            (setq calendar-holidays
                  (append japanese-holidays holiday-other-holidays))))

;; 日曜日、土曜日、休日に色をつける
(make-face 'holiday-face)
(set-face-background 'holiday-face "tomato")

(make-face 'sunday-face)
(set-face-foreground 'sunday-face "red")

(make-face 'saturday-face)
(set-face-foreground 'saturday-face "blue")

(setq calendar-mark-holidays-flag t)
(setq calendar-mark-diary-entries-flag nil)
(setq calendar-holiday-marker 'holiday-face)
(setq japanese-holiday-weekend-marker
      '(sunday-face nil nil nil nil nil saturday-face))
(add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
(add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)

;;;
;;; session
;;;
(require-or-install 'session)
(setq session-use-package t)
(add-hook 'after-init-hook 'session-initialize)
(add-hook 'session-after-jump-to-last-change-hook
          '(lambda () (when (and (eq major-mode 'org-mode)
                                 (outline-invisible-p))
                        (org-reveal))))

;;;
;;; C-Mode
;;;
(setq c-indent-level                4
      c-argdecl-indent              0
      c-brace-offset                0
      c-continued-statement-offset  2   ;; 4
      c-label-offset               -2
      c-auto-newline                nil
      c-tab-always-indent           t)
(setq c-default-style "bsd")
;(define-key c-mode-map "{" 'electric-c-semi)
;(define-key c-mode-map "\C-m" 'newline-and-indent)

;;;
;;; JDE
;;;
(setq semanticdb-default-save-directory "~/.semantic")
(when (locate-library "lisp/jde")
  (autoload 'jde-mode "lisp/jde" "JDE mode." t))
(when (locate-library "jde")
  (autoload 'jde-mode "jde" "JDE mode." t))
(eval-after-load "jde" '(load-library "cedet-1.0/common/cedet.el"))
(eval-after-load "jde" '(require 'jde-ant))
(eval-after-load "jde" '(require 'jde-javadoc))
(setq auto-mode-alist
      (cons (cons "\\.java$" 'jde-mode) auto-mode-alist))
(set-variable 'jde-ant-enable-find t)
(set-variable 'jde-ant-invocation-method (quote ("Script")))
(set-variable 'jde-ant-program "/home/hirata/lib/java/ant/bin/ant")
(set-variable 'jde-build-function (quote (jde-ant-build)))
(set-variable 'jde-help-docsets (quote (("JDK API" "/usr/local/java/docs/ja/api" nil) ("User (javadoc)" "http://java.sun.com/products/servlet/2.3/javadoc/index.html" nil))))
(set-variable 'jde-help-use-frames nil)
(set-variable 'jde-javadoc-author-tag-template "\"* @author <a href=\\\"mailto:heita@nifty.com\\\">Tadashi HIRATA</a>\"")
(setq jde-build-function '(jde-ant-build)
          jde-ant-program (if (featurep 'w32-win)
            "/cygwin/home/hirata/lib/ant/bin/ant"
            "/home/hirata/lib/java/ant/bin/ant")
          jde-ant-enable-find t
          jde-ant-invocation-method '("Script")
          jde-help-docsets '(("JDK API" "/usr/local/java/docs/ja/api" nil)
                 ("User (javadoc)"
                  "http://java.sun.com/products/servlet/2.3/javadoc/index.html" nil))
          jde-help-use-frames nil)
;(setq tags-table-list "/home/hirata/cbi2/TAGS")
(setq c-basic-offset 4)

;;;
;;; ECB (use ecb-activete, ecb-deactivete to use ECB)
;;;
(when (locate-library "ecb-autoloads")
  (require 'ecb-autoloads))

;;;
;;; SmartDoc
;;;
(add-to-list 'auto-mode-alist '("\\.sdoc$" . sdoc-mode))
(setq sgml-quick-keys t)
(autoload 'sdoc-mode "sdoc-mode" nil t)

;;;
;;; mgp
;;;
(autoload 'mgp-mode "mgp" "mgp mode." t)
(setq auto-mode-alist
      (cons (cons "\\.mgp$" 'mgp-mode) auto-mode-alist))

;;;
;;; gpg
;;;
(require 'epg)
(setq epa-file-inhibit-auto-save t)
(setq epa-file-select-keys 1)
(setq epa-pinentry-mode 'loopback)

;;;
;;; python
;;;
(require-or-install 'elpy)
(add-to-list 'exec-path
             '("c:/Users/hirata/Anaconda3/Scripts"))
(elpy-enable)
(setq elpy-test-runner 'elpy-test-pytest-runner)
; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "\C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)))
(set-face-attribute 'highlight-indentation-face nil :background "gray100")

;;;
;;; Ruby
;;;
(setq ruby-program-name (executable-find "irb"))
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;;;
;;; Perl mode
;;;
(setq auto-mode-alist
      (cons (cons "\\.pl$" 'perl-mode) auto-mode-alist))

;;;
;;; SML
;;;
(setq sml-program-name "smlsharp")
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(setq auto-mode-alist
      (append '(("\\.sml$" . sml-mode)
                ("\\.sig$" . sml-mode)
                ("\\.ML$"  . sml-mode)) auto-mode-alist))
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

;;;
;;; Haskell
;;;
(when (locate-library "haskell-site-file")
  (load "haskell-site-file"))
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

;;;
;;; java script
;;;
(require-or-install 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;
;;; HTML
;;;
(add-hook 'html-mode-hook
          (lambda ()
            (define-key sgml-mode-map "\"" nil)))

;;;
;;; yaml mode
;;;
(if (locate-library "yaml-mode")
    (progn
      (autoload 'yaml-mode "yaml-mode" nil t)
      (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))))

;;;
;;; erlang
;;;
(autoload 'erlang-mode "erlang" "Major mode for editing erlang." t)
(setq auto-mode-alist
      (cons '("\\.erl$" . erlang-mode) auto-mode-alist))

;;;
;;; Scheme
;;;
(when (locate-library "quack")
  (require 'quack)
  (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t))
(setq quack-programs '("gauche" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))
(setq scheme-program-name "gosh -i")
(setq quack-default-program "gosh")
(setq quack-pretty-lambda-p nil)
(setq quack-fontify-style 'plt)

;;;
;;; slime (Common Lisp)
;;;
(setq inferior-lisp-program "/usr/bin/cmucl")
(when (locate-library "slime")
  (require 'slime)
  (slime-setup))

;;;
;;; Emacs Speaks Statistics
;;;
;(add-to-list 'load-path "~/lib/elisp/ess-13.09-1/")
;(when (locate-library "ess-site")
;  (require 'ess-site))

;;;
;;; grep
;;;
(setq grep-program "lgrep")
(setq grep-command (format "%s -i -n -Ke -Oe " grep-program))
(setq grep-find-command (format "find . -type f -print0 | xargs -0 %s" grep-command))

;;;
;;; Mew
;;;
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(autoload 'mew-user-agent-compose "mew" nil t)
(setq mew-mail-domain-list '("nri.co.jp" "mb.infoweb.ne.jp" ))
(setq mew-prog-text/html-ext nil) ;; for emacs24
;(setq mew-demo nil)
(setq mew-icon-directory "/usr/share/emacs/site-lisp/mew/etc")
(if (boundp 'mail-user-agent)
      (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

;;;
;;; SKK
;;;
(autoload 'skk-mode "skk" nil t)
(autoload 'skk-auto-fill-mode "skk" nil t)
(autoload 'skk-input-by-code-or-menu "skk" nil t)  ;; C-c\\
(autoload 'skk-display-code-for-char-at-point "skk" nil t) ;; C-c$
(autoload 'skk-latin-region "skk" nil t) ;; C-cl
(autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)
(setq skk-byte-compile-init-file nil)
(setq skk-init-file "~/.skk/skkrc")
;;(setq skk-jisyo     "~/.skk/skk-jisyo")
;;(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xj" 'auto-fill-mode)
(global-set-key "\C-x\C-j" 'skk-mode)

;;;
;;; YaTeX
;;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX Mode" t)
(put 'yatex-mode 'font-lock-defaults 'tex-mode)
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq tex-command "platex")
(setq dvi2-command "xdvi")
(setq YaTeX-prefix "\C-\\")
(setq yatex-mode-hook '())
(defun YaTeX-insert-underscore () (interactive) (insert "\\_"))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map "_" 'YaTeX-insert-underscore)))

;;;
;;; VC (disable)
;;;
(setq vc-handled-backends nil)

;;;
;;; Edic
;;;
(autoload 'edic "edic" "English-Japanese Dictionary" t)
(autoload 'eedic "eedic" "English-English Dictionary (online)" t)
(autoload 'telgate "telgate" "NRI Telephone Book Search" t)
(autoload 'mouse-edic "edic" "English-Japanese Dictionary" t)
(setq edic-program "~/bin/ejdic")
(setq edic-dictionary "/home/hirata/lib/dict/ejdic")
(modify-coding-system-alist 'process "telgate" 'utf-8)

;;;
;;; Japanese/English dictionary (C-ce)
;;;
(require 'google-translate)
(require 'google-translate-default-ui)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ja")

;;;
;;; TRR & Allen (trr program specialized for Allen series.)
;;;
(autoload 'trr "trr" nil t)
(when (featurep 'w32-win)
  (setenv "TRRDIR" "c:/Meadow2/site-lisp/trr19")
  (setenv "TRRBINDIR" "c:/Meadow2/site-lisp/trr19"))
(autoload 'allen "allen" "Typing traning program" t)
(setq allen-text-directory "~/doc/allen")

;;;
;;; dmacro
;;;
(defconst *dmacro-key* "\C-cd" "繰返し指定キー")
(autoload 'dmacro-exec "dmacro" nil t)

;;;
;;; switch buffer
;;;
; C-M-j/k でバッファを切り替え
(require-or-install 'swbuff)
(setq swbuff-exclude-buffer-regexps '("^ " "^\*.*\*"))
(setq swbuff-window-min-text-height 3)

;;;
;;; filecache (find-file 中に \C-c\C-i で 指定ディレクトリの下全部を検索、マッ
;;; チしたものを表示する。
;;;
(when (not (featurep 'w32-win))
  (require 'filecache)
  (file-cache-add-directory-using-locate "hirata/Dropbox")
  (define-key minibuffer-local-completion-map
    "\C-c\C-i" 'file-cache-minibuffer-complete))

;;;
;;; Recent used file
;;;
(require 'recentf)
(setq recentf-max-saved-items 2000)
(recentf-mode 1)

;;;
;;; uniquify
;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;
;;; redo
;;;
(when (locate-library "redo")
  (require 'redo))

;;;
;;; org
;;;
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-hide-leading-stars t)
(setq org-display-custom-times t)
(setq org-cycle-separator-lines 0)
(setq org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M>"))
(setq org-capture-templates
      '(("m" "memo" entry (file "~/Dropbox/PlainText/memo.org")
             "* %u %i %?" :prepend t)))
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
(defun gtd ()
  (interactive)
  (find-file "~/Dropbox/PlainText/gtd.org"))
(defun memo (p)
  (interactive "P")
  (if p
      (find-file (expand-file-name "~/Dropbox/PlainText/memo.org"))
    (org-capture nil "m")))

;;;
;;; migemo
;;;
(require-or-install 'migemo)
(setq migemo-dictionary
      (cond ((featurep 'w32-win) 
             "c:/Users/hirata/.emacs.d/migemo/utf-8/migemo-dict")
            (t "/usr/share/cmigemo/utf-8/migemo-dict")))
(setq migemo-command (executable-find "cmigemo"))
(setq migemo-options '("-q" "--emacs"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)

;;;
;;; web browser
;;;
(setq eww-search-prefix "http://www.google.co.jp/search?q=")
(setq eww-disable-colorize t)
(when (featurep 'w32-win)
  (setq browse-url-generic-program 
        "c:/Program Files/Mozilla Firefox/firefox.exe"))
;(add-hook 'eww-mode-hook '(lambda () (rename-buffer "eww" t)))

;;;
;;; psgml
;;;
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(setq auto-mode-alist
      (cons (cons "\\.xml$" 'xml-mode) auto-mode-alist))
(setq sgml-auto-activate-dtd t)
(setq sgml-omittag-transparent t)
(setq sgml-balanced-tag-edit t)
(setq sgml-auto-insert-required-elements t)
(setq sgml-live-element-indicator t)
(setq sgml-indent-step nil)

;;;
;;; auto save buffer
;;;
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1)
(setq auto-save-buffers-enhanced-include-regexps '(".+")) ;全ファイル
(setq auto-save-buffers-enhanced-exclude-regexps '("\\.gpg$"))
(setq auto-save-buffers-enhanced-quiet-save-p t)
(setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
(setq auto-save-buffers-enhanced-file-related-with-scratch-buffer
      (locate-user-emacs-file "scratch"))
(auto-save-buffers-enhanced t)

;;;
;;; ispell
;;;
; 日本語のスキップ
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;(when (featurep 'w32-win)
;  (setenv "DICTIONARY" "c:/cygwin/usr/local/share/ispell/english.hash")
;  (setq ispell-alternate-dictionary
;       "c:/cygwin/usr/local/share/ispell/american.hash"))
(setq-default ispell-program-name "aspell")

;;;
;;; tramp
;;;
(when (locate-library "tramp")
  (require 'tramp)
  (setq tramp-default-method "ssh"))

;;;
;;; smv
;;;
(autoload 'smv-mode "smv-mode" "SMV specifications editing mode." t)
(setq auto-mode-alist
      (append  (list '("\\.smv$" . smv-mode) '("\\.ord$" . smv-ord-mode))
           auto-mode-alist))
(setq completion-ignored-extensions
      (cons ".ord" (cons ".opt" completion-ignored-extensions)))

;;;
;;; Alternative visuall bell
;;;
(when (locate-library "altbell")
  (require 'altbell))

;;;
;;; Favorite Features
;;;
(setq system-time-locale "C")  ;; for dired
(setq default-tab-width 4)
(setq fill-column 70)
(setq next-line-add-newlines nil)
(setq scroll-step 4)
(setq line-number-mode t)
(set-scroll-bar-mode nil)
(setq minibuffer-auto-raise nil)
(setq visible-bell t)
;(setq inhibit-default-init t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "hirata")
(set-default 'line-spacing 3)
(set-default 'indent-tabs-mode nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq read-file-name-completion-ignore-case t)
(tool-bar-mode -1)
(blink-cursor-mode nil)
(auto-compression-mode t)
;(show-paren-mode 1)
(transient-mark-mode 1)
;(mouse-avoidance-mode 'banish)
(mouse-avoidance-mode 'jump)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;;
;;; faces
;;;
(setq list-faces-sample-text "abcdefg ABCDEFG 私の名前は中野です")
(when window-system
  (set-face-attribute 'default nil :background "LemonChiffon")
  (set-face-attribute 'default nil :foreground "gray15")
  (set-face-attribute 'region nil :background "lightgoldenrod2")
  (set-face-attribute 'region nil :foreground nil)
  (make-face 'modeline) ;; compatibility with emacs23
  (make-face 'red)
  (set-face-attribute 'red nil :foreground "red")
  (make-face 'blue)
  (set-face-attribute 'blue nil :foreground "blue")
  (make-face 'RED)
  (set-face-attribute 'RED nil :foreground "red" :weight 'bold)
  (make-face 'BLUE)
  (set-face-attribute 'BLUE nil :foreground "blue" :weight 'bold)
  (make-face 'large)
  (set-face-attribute 'large nil :height 2.0 :weight 'bold)
  (make-face 'strong)
  (set-face-attribute 'strong nil
                      :background "red" :foreground "black"
                      :weight 'extra-bold)
  (set-face-attribute 'menu nil 
                      :background "gray20"
                      :foreground "white"
                      :box nil)
  (set-face-attribute 'fringe nil
                      :background "LemonChiffon"
                      :stipple "None")
  (set-face-attribute 'mode-line  nil
                      :background "gray20"
                      :foreground "white"
                      :box nil))

; emacs23 on Windows
(when (and window-system (featurep 'w32-win))
  (set-fontset-font "fontset-standard" 'latin
                    (font-spec :family "DeJavu Sans Mono" :size 12))
  (set-fontset-font "fontset-standard" 'japanese-jisx0208
                    (font-spec :family "ヒラギノ角ゴ Pro W3" :size 14))
  (set-fontset-font "fontset-standard" 'kana
                    (font-spec :family "ヒラギノ角ゴ Pro W3" :size 14))
  (set-fontset-font "fontset-standard" '(#x2460 . #x24ea)
                    (font-spec :family "ＭＳ 明朝" :size 14)))

; Linux
(when (and window-system (string-match "linux" system-configuration))
  ; size 14 / mincho
  ;(set-fontset-font "fontset-standard" 'latin
  ;                  (font-spec :family "Monaco" :size 14))
  ;(set-fontset-font "fontset-standard" 'japanese-jisx0208
  ;                  (font-spec :family "ヒラギノ明朝 Pro W3" :size 14))
  ;(set-fontset-font "fontset-standard" 'kana
  ;                  (font-spec :family "ヒラギノ明朝 Pro W3" :size 14))

  ; size 12 / gothic
  (set-fontset-font "fontset-standard" 'latin
                    (font-spec :family "Inconsolata" :size 12))
  (set-fontset-font "fontset-standard" 'japanese-jisx0208
                    (font-spec :family "VL ゴシック" :size 14))
  (set-fontset-font "fontset-standard" 'kana
                    (font-spec :family "VL ゴシック" :size 14))
  (set-fontset-font "fontset-standard" '(#x2460 . #x24ea)
  (font-spec :family "HG P明朝L Sun" :size 12)))

; emacs24 on Mac
(when (and window-system
           (string-match "^24\." emacs-version)
           (string-match "apple-darwin" system-configuration))
  ;; ASCII フォント
  (set-fontset-font "fontset-standard" 'latin
                    ;(font-spec :family "Menlo" :size 12)
                    ;(font-spec :family "hoge" :size 12)
                    (font-spec :family "Monaco" :size 12)
                    )
  ;; 日本語のフォント
  (set-fontset-font "fontset-standard" 'japanese-jisx0208
                    ;'("Hiragino Maru Gothic ProN")
                    ;'("Hiragino Kaku Gothic ProN")
                    '("Hiragino Mincho ProN")
                    )
  ;; 半角カナのフォント
  (set-fontset-font "fontset-standard" 'katakana-jisx0201
                    ;'("Hiragino Maru Gothic ProN")
                    ;'("Hiragino Kaku Gothic ProN")
                    '("Hiragino Mincho ProN")
                    )
  (setq face-font-rescale-alist
    '(("^-apple-hiragino.*" . 1.2)
      (".*osaka-bold.*" . 1.2)
      (".*osaka-medium.*" . 1.2)
      (".*courier-bold-.*-mac-roman" . 1.0)
      (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
      (".*monaco-bold-.*-mac-roman" . 0.9)
      ("-cdac$" . 1.3)
      (".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)
      (".*Hiragino_Mincho_ProN.*" . 1.2)
      (".*Menlo.*" . 1.0)
      (".*Helvetica Neue.*" . 1.0))))

(create-fontset-from-fontset-spec
 "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-fontset-most,
    latin-iso8859-2:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-2,
    latin-iso8859-3:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-3,
    latin-iso8859-4:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-4,
    cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-5,
    greek-iso8859-7:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-7,
    latin-iso8859-9:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-9,
    japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
    katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
    latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
    japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
    korean-ksc5601:-*-Gulim-normal-r-*-*-12-*-*-*-c-*-ksc5601-*,
    chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
    chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
    chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*" t)

(when window-system
  (if (featurep 'w32-win)
      (setq frame-parameters-alist '((font . "fontset-standard")
                                     (top . 00)
                                     (left . 200)
                                     (width . 80)
                                     (height . 50)))
    (setq frame-parameters-alist '((font . "fontset-standard")
                                   (top . 00)
                                   (left . 850)
                                   (width . 80)
                                   (height . 52))))
  (modify-frame-parameters nil frame-parameters-alist)
  (setq initial-frame-alist
        (append frame-parameters-alist initial-frame-alist))
  (setq default-frame-alist
        (append frame-parameters-alist default-frame-alist)))

;;;
;;; Easy Access to The Files Often Visited (C-x r j ?)
;;;
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?d '(file . "~/Dropbox/PlainText/diary/diary2018"))
(set-register ?p '(file . "~/Dropbox/doc/NRI/Projects"))

;;;
;;; Key bindings
;;;
(keyboard-translate ?\C-h ?\C-?)  ;; It doesn't work in XEmacs???
(global-set-key "\C-ca" 'allen)
(global-set-key "\C-cb" 'compile)       ;; build
(global-set-key "\C-cc" 'calendar)
;(global-set-key "\C-cd" 'diary)
(global-set-key "\C-cd" 'dmacro-exec)
(global-set-key "\C-ce" 'google-translate-at-point)
(global-set-key "\C-cE" 'google-translate-at-point-reverse)
(global-set-key "\C-cf" 'mew)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ch" 'help-for-help)
(global-set-key "\C-ci" 'info)
(global-set-key "\C-ck" 'redo)
(global-set-key "\C-cl" 'skk-latin-region)
(global-set-key "\C-cm" 'mew-send)
(global-set-key "\C-co" "\C-u-1\C-xo")
(global-set-key "\C-cq" 'telgate)
(global-set-key "\C-cr" 'recentf-open-files)
(global-set-key "\C-cs" 'shell)
(global-set-key "\C-ct" 'ispell-word)
(global-set-key "\C-cu" 'skk-undo-kakutei)
(global-set-key "\C-cv" "\M-xview-buffer")
(global-set-key "\C-cw" 'clipboard-kill-ring-save)
(global-set-key "\C-cx" 'hexl-find-file)
(global-set-key "\C-cy" 'clipboard-yank)
(global-set-key "\C-cz" '(lambda () (interactive)
                           (switch-to-buffer "*scratch*")))
(global-set-key "\C-c\\" 'skk-input-by-code-or-menu)
(global-set-key "\C-c$" '(lambda () (interactive)
                           (skk-display-code-for-char-at-point)))
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-c\C-w" 'clipboard-kill-region)
(global-set-key "\C-c\C-y" 'clipboard-yank)
(global-set-key [delete]   'help-for-help)
(global-set-key "\C-c\M-%" 'query-replace-regexp)
(global-set-key "\M-\C-j" 'swbuff-switch-to-next-buffer)
(global-set-key "\M-\C-k" 'swbuff-switch-to-previous-buffer)
(global-set-key "\C-c;" '(lambda () (interactive)
                           (insert-string (current-time-string))))
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-z")
(cond (window-system
       (global-set-key [mouse-1]      'ignore) ;; Use double-click mouse1
       (global-set-key [mouse-2]      'ignore)
       (global-set-key [mouse-3]      'ignore)
       (global-set-key [down-mouse-1] 'ignore)
       (global-set-key [down-mouse-2] 'ignore)
       (global-set-key [down-mouse-3] 'ignore)
       (global-set-key [drag-mouse-1] 'ignore)
       (global-set-key [drag-mouse-2] 'ignore)
       (global-set-key [drag-mouse-3] 'ignore)
       (global-set-key [double-mouse-1] 'mouse-set-point)
       (global-set-key [double-mouse-2] 'browse-url-netscape)
       (global-set-key [double-mouse-3] 'mouse-edic)))
(when (string-match "apple-darwin" system-configuration)
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super)))

;;;
;;; Execute some commands
;;;
;(setenv "LANG" "ja_JP.eucJP")  ;; need for mew-nmz
(setenv "LC_ALL" "C")  ;; need for dired
(setenv "LANG" "C")
(cd "~")
(setq debug-on-error nil)

;;;
;;; end of ".emacs"
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-indent-mode nil)
 '(safe-local-variable-values (quote ((paragraph-indent-text-mode . off))))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
