;;;
;;; genjis.el --- This program converts title of the story of Genji
;;;               in variable `mule-version' from Latin script to
;;;               JIS X0208 Kanji.
;;;
;;; Copyright (C) 1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Created: 1995/7/7
;;; Version:
;;;	$Id: genjis.el,v 2.0 1996/01/18 13:24:24 morioka Exp $
;;; Keywords: emulation, compatibility, NEmacs, Mule, XEmacs
;;;
;;; This file is part of tl (Tiny Library).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Commentary:
;;;
;;; この module を load すると、mule-version の中に入っている源氏物語
;;; の巻名が漢字で表示されます。
;;; 変数 mule-genji-version-alist の値は Mule の配布 package に含まれ
;;; る etc/VERSIONS に基づいています。
;;;
;;; Code:

(require 'tl-seq)

(defvar si:mule-version mule-version) 

(defvar mule-genji-version-alist
  '(("KIRITSUBO"      . "桐壺")
    ("HAHAKIGI"	      . "帚木")
    ("UTSUSEMI"	      . "空蝉")
    ("YUUGAO"	      . "夕顔")
    ("WAKAMURASAKI"   . "若紫")
    ("SUETSUMUHANA"   . "末摘花")
    ("MOMIJINOGA"     . "紅葉賀")
    ("HANANOEN"	      . "花宴")
    ("AOI"	      . "葵")
    ("SAKAKI"	      . "賢木")
    ("HANACHIRUSATO"  . "花散里")
    ("SUMA"	      . "須磨")
    ("AKASHI"	      . "明石")
    ("MIOTSUKUSHI"    . "澪標")
    ("YOMOGIU"	      . "蓬生")
    ("SEKIYA"	      . "関屋")
    ("EAWASE"	      . "絵合")
    ("MATSUKAZE"      . "松風")
    ("USUGUMO"	      . "薄雲")
    ("ASAGAO"	      . "槿")
    ("OTOME"	      . "少女")
    ("TAMAKAZURA"     . "玉鬘")
    ("HATSUNE"	      . "初音")
    ("KOCHOU"	      . "胡蝶")
    ("HOTARU"	      . "蛍")
    ("TOKONATSU"      . "常夏")
    ("KAGARIBI"	      . "篝火")
    ("NOWAKE"	      . "野分")
    ("MIYUKI"	      . "行幸")
    ("FUJIBAKAMA"     . "藤袴")
    ("MAKIBASHIRA"    . "真木柱")
    ("UMEGAE"	      . "梅枝")
    ("FUJINOURABA"    . "藤裏葉")
    ("WAKANA-KAMI"    . "若菜上")
    ("WAKANA-SHIMO"   . "若菜下")
    ("KASHIWAGI"      . "柏木")
    ("YOKOBUE"	      . "横笛")
    ("SUZUMUSHI"      . "鈴虫")
    ("YUUGIRI"	      . "夕霧")
    ("MINORI"	      . "御法")
    ("MABOROSHI"      . "幻")
;;; ("KUMOGAKURE"     . "雲隠") ; 番外 (Extra volume)
    ("NIOUMIYA"	      . "匂宮")
    ("KOUBAI"	      . "紅梅")
    ("TAKEGAWA"	      . "竹河")
    ("HASHIHIME"      . "橋姫")
    ("SHIIGAMOTO"     . "椎本")
    ("AGEMAKI"	      . "総角")
    ("SAWARABI"	      . "早蕨")
    ("YADORIGI"	      . "宿木")
    ("AZUMAYA"	      . "東屋")
    ("UKIFUNE"	      . "浮舟")
    ("KAGEROU"	      . "蜻蛉")
    ("TENARAI"	      . "手習")
    ("YUMENOUKIHASHI" . "夢浮橋")
    ))

(setq mule-version
      (let ((ret (some-element
		  (function
		   (lambda (pair)
		     (string-match (car pair) mule-version)
		     ))
		  mule-genji-version-alist)
		 ))
	(if ret
	    (concat (substring mule-version 0 (match-beginning 0))
		    (cdr ret)
		    (substring mule-version (match-end 0))
		    )
	  )))


;;; @ end
;;;

(provide 'genjis)

;;; genjis.el ends here
