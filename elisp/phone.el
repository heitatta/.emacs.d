(setq forms-file "~/doc/data/phone")
(setq forms-format-list 
      '("======= 電話帳 =======\n"
	"\n"
	"名前 : "    3  "\n" 
	"番号 : "    2  "\n"
	"\n"
	(make-string 20 ?-) "\n"
	"\n"
	"備考 : "    4  "\n"
	"キー : "    1  "\n"))
(setq forms-number-of-fields 4)
(setq forms-field-sep ":")
(setq forms-read-only t)
(setq forms-multi-line "\^k")
(setq forms-read-file-filter nil)
(setq forms-write-file-filter nil)
(setq forms-new-record-filter nil)
(setq forms-modified-record-filter nil)