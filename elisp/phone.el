(setq forms-file "~/doc/data/phone")
(setq forms-format-list 
      '("======= �d�b�� =======\n"
	"\n"
	"���O : "    3  "\n" 
	"�ԍ� : "    2  "\n"
	"\n"
	(make-string 20 ?-) "\n"
	"\n"
	"���l : "    4  "\n"
	"�L�[ : "    1  "\n"))
(setq forms-number-of-fields 4)
(setq forms-field-sep ":")
(setq forms-read-only t)
(setq forms-multi-line "\^k")
(setq forms-read-file-filter nil)
(setq forms-write-file-filter nil)
(setq forms-new-record-filter nil)
(setq forms-modified-record-filter nil)