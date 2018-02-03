;;;
;;; Insert follows in your .emacs
;;;    (global-set-key "\C-cr" 'dmacro-exec)
;;;    (autoload 'dmacro-exec "dmacro" nil t)
;;;
(defvar dmacro-str nil "キーボード・マクロ文字列")
(defvar dmacro-key (car (where-is-internal 'dmacro)) "キーバインド")

(defun dmacro ()
  "キー操作の繰り返しを検出し、キーボード・マクロに登録、実行する"
  (interactive)
  (let ((s (dmacro-get)))
    (if (null s)
	(message "No Redo Action")
      (execute-kbd-macro s))))

(defun dmacro-get ()
  (let* ((rkeys (recent-keys)))
    (if (dmacro-now rkeys)
	dmacro-str
      (setq dmacro-str (dmacro-search rkeys))
      (setq last-kbd-macro dmacro-str))))

(defun dmacro-now (str)
  (let* ((len (length dmacro-key))
	 (i 1))
    (catch 'break
      (while (<= i len)
	(let ((key (elt dmacro-key (- len i))))
	  (if (not (and (equal (elt str (- 100 i)) key)
			(equal (elt str (- 100 i len)) key)))
	      (throw 'break i)))
	(setq i (+ i 1))))
    (> i len)))

(defun dmacro-search (str)
  (let ((i 3))
    (catch 'break
      (while (< i 50)
	(let ((first  (- 98 (* 2 i)))
	      (second (- 98 i))
	      (len (- i 1)))
	  (catch 'not-equal
	    (while (<= 0 len)
	      (if (not (equal (elt str (+ first len)) 
			      (elt str (+ second len))))
		  (throw 'not-equal nil))
	      (setq len (- len 1))))
	  (if (equal len -1)
	      (throw 'break i))
	  (setq i (+ i 1)))))
    (if (<= 50 i)
	nil
      (aset str 98 0)
      (aset str 99 0)
      (vnthcdr (- 98 i) str))))

(defun vnthcdr (i vec)
  (if (< i (length vec))
      (vconcat (vector (elt vec i)) (vnthcdr (1+ i) vec))
    '[]))
