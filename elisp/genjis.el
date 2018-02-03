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
;;; $B$3$N(B module $B$r(B load $B$9$k$H!"(Bmule-version $B$NCf$KF~$C$F$$$k8;;aJ*8l(B
;;; $B$N4,L>$,4A;z$GI=<($5$l$^$9!#(B
;;; $BJQ?t(B mule-genji-version-alist $B$NCM$O(B Mule $B$NG[I[(B package $B$K4^$^$l(B
;;; $B$k(B etc/VERSIONS $B$K4p$E$$$F$$$^$9!#(B
;;;
;;; Code:

(require 'tl-seq)

(defvar si:mule-version mule-version) 

(defvar mule-genji-version-alist
  '(("KIRITSUBO"      . "$B6MTd(B")
    ("HAHAKIGI"	      . "$BVdLZ(B")
    ("UTSUSEMI"	      . "$B6u@f(B")
    ("YUUGAO"	      . "$BM<4i(B")
    ("WAKAMURASAKI"   . "$B<c;g(B")
    ("SUETSUMUHANA"   . "$BKvE&2V(B")
    ("MOMIJINOGA"     . "$B9HMU2l(B")
    ("HANANOEN"	      . "$B2V1c(B")
    ("AOI"	      . "$B0*(B")
    ("SAKAKI"	      . "$B8-LZ(B")
    ("HANACHIRUSATO"  . "$B2V;6N$(B")
    ("SUMA"	      . "$B?\Ka(B")
    ("AKASHI"	      . "$BL@@P(B")
    ("MIOTSUKUSHI"    . "$B_:I8(B")
    ("YOMOGIU"	      . "$BK)@8(B")
    ("SEKIYA"	      . "$B4X20(B")
    ("EAWASE"	      . "$B3(9g(B")
    ("MATSUKAZE"      . "$B>>Iw(B")
    ("USUGUMO"	      . "$BGv1@(B")
    ("ASAGAO"	      . "$B\](B")
    ("OTOME"	      . "$B>/=w(B")
    ("TAMAKAZURA"     . "$B6Lr#(B")
    ("HATSUNE"	      . "$B=i2;(B")
    ("KOCHOU"	      . "$B8UD3(B")
    ("HOTARU"	      . "$B7V(B")
    ("TOKONATSU"      . "$B>o2F(B")
    ("KAGARIBI"	      . "$Bd@2P(B")
    ("NOWAKE"	      . "$BLnJ,(B")
    ("MIYUKI"	      . "$B9T9,(B")
    ("FUJIBAKAMA"     . "$BF#8S(B")
    ("MAKIBASHIRA"    . "$B??LZCl(B")
    ("UMEGAE"	      . "$BG_;^(B")
    ("FUJINOURABA"    . "$BF#N"MU(B")
    ("WAKANA-KAMI"    . "$B<c:Z>e(B")
    ("WAKANA-SHIMO"   . "$B<c:Z2<(B")
    ("KASHIWAGI"      . "$BGpLZ(B")
    ("YOKOBUE"	      . "$B2#E+(B")
    ("SUZUMUSHI"      . "$BNkCn(B")
    ("YUUGIRI"	      . "$BM<L8(B")
    ("MINORI"	      . "$B8fK!(B")
    ("MABOROSHI"      . "$B88(B")
;;; ("KUMOGAKURE"     . "$B1@1#(B") ; $BHV30(B (Extra volume)
    ("NIOUMIYA"	      . "$BFw5\(B")
    ("KOUBAI"	      . "$B9HG_(B")
    ("TAKEGAWA"	      . "$BC]2O(B")
    ("HASHIHIME"      . "$B66I1(B")
    ("SHIIGAMOTO"     . "$BDGK\(B")
    ("AGEMAKI"	      . "$BAm3Q(B")
    ("SAWARABI"	      . "$BAaOO(B")
    ("YADORIGI"	      . "$B=ILZ(B")
    ("AZUMAYA"	      . "$BEl20(B")
    ("UKIFUNE"	      . "$BIb=.(B")
    ("KAGEROU"	      . "$BiqiY(B")
    ("TENARAI"	      . "$B<j=,(B")
    ("YUMENOUKIHASHI" . "$BL4Ib66(B")
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
