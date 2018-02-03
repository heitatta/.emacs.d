(defvar title "姓名判断    for NEmacs/Mule. (C) yoshinov@etl.go.jp")
(defvar version "ver 2.0 (1995.3)")
(defvar patches1 "add seimei-region by akiba@aa.cs.keio.ac.jp")
(defvar patches2 "for mule version by yoichi@esasd.ksp.fujixerox.co.jp")
(defvar patches3 "for ne/mu version by ksakai@mtl.t.u-tokyo.ac.jp")
(defvar original "姓名判断 version 1.02 Copyright(C) 1991 S.Ameno, 沢渡香澄")
;;
;; 姓名判断 for NEmacs/Mule version. by yoshinov@etl.go.jp
;;
;; 1. How to setup....(設定方法)
;;     あなたの.emacsに以下の一行を入れてください。
;; (autoload 'seimei "seimei" "seimei" t)
;; もちろん、load-pathで検索できるディレクトリにseimei.elを入れてください。
;;
;; 2. How to use......(使用方法) 次の三通りです。
;; a)
;;    M-x seimei
;; b)
;;    姓と名の間に空白をあけて書き、そこにregionを設定してから
;;    M-x seimei-region
;;
;;    ただし、seimei.elを一度読み込んでおくか、.emacsに
;;    (autoload 'seimei-region "seimei" "seimei" t)
;;    と書き込んでおく必要があります。老婆心ながら…:-)
;; c)
;;    姓と名の間に空白をあけて一行に一人づつ書きます。ウレシハズカシ！
;;    M-x seimei-buffer
;;
;;    ただし、seimei.elを一度読み込んでおくか、.emacsに
;;    (autoload 'seimei-buffer "seimei" "seimei" t)
;;    と書き込んでおく必要があります。再び、老婆心ながら…:-)
;;
;; 3. Bugs and other info.
;;   seimei.c とは、得点の計算精度が異なります。elで浮動小数点は…(;_;)
;;   文字が80画以上の名前は1画に戻ります。
;;   漢字画数辞書bufferは、終了時にもkill-bufferされません。(for 再利用)
;;   byte-compileするとseimei.elcはmule固定あるいはnemacs固定になりますので、
;;   muleとnemacsを共存させている方は御留意下さい。(by yoichi)
;;
;; 3.1 姓名判断には流派がいろいろあるけど…？
;;
;;   漢字の画数の数え方自体が流派によって異なるようです。また、ある流派の吉
;;   数が別の流派の大凶数になったりするようです。天地同画への注意も、陰陽バ
;;   ランスも、五行も、音感もない、ということで、本当に気にしている人は本を
;;   お読みになるか、専門の方に見て頂くことをお勧めします。ただ、本プログラ
;;   ムは小島白楊先生の解釈によるものです。
;;
;;   参考　姓名判断入門　ナツメ社　定価１０１０円（本体９８１円）
;;
;;   ---- 以下、沢渡香澄さんの作られたseimei.docより転載です。 ---
;;   なお、表示メッセージの著作権者である小島白楊先生の連絡先は
;;   次のとおりです。(先生御本人の希望により明示しています)
;;   神奈川県三浦郡葉山町堀内1950-10  〒240-01    0468-75-2933
;;   -------------------------------------------------------------
;;
;;   なお、漢字の画数に関しては御自身で手直しすることが出来ます。seimei.el
;;   の後半部分は画数辞書になっておりますが、画数が変化するごとに\nがくるよ
;;   うになっています。ですから、例えばdefaultで４画の「五」を５画に変更し
;;   たい、ということであれば、\nをひとつ越えたところに移動させればよいわけ
;;   です。
;;
;; 3.2 英文字を入れると天罰が下る…。
;;   ver 1.03em より、破邪の祈祷を致しましたので天罰は下りませぬ。
;;
;;   エラーが出た場合にはお知らせ下さい。

(defvar Seimei-Buffer "*姓名判断*" "Buffer name for 姓名判断")
(defvar Seimei-Kanji-Kakusuu-Dic-Buffer " *漢字画数辞書*"
  "Buffer name for 漢字画数辞書*")
(defvar Float-Buffer " *浮動小数点演算用*"
  "Buffer name for 浮動小数点演算用*")
(defvar Seimei-TMPBUFFER " *SEIMEI-tmpbuf*"
  "Buffer name for 文字列の長さ計算用*")
(defvar seimei-char-width (length "あ") "日本語文字の大きさ")
(defconst seimei-erase-buffer t "姓名判断の結果を、毎回消去する時にはt")

;;;###autoload
(defun seimei-buffer ()
  (interactive)
  (save-excursion
    (let ((cbuf (current-buffer))
	  (seimei-erase-buffer nil))
      (set-buffer (get-buffer-create Seimei-Buffer))
      (erase-buffer)
      (insert (format "%s\n\t\t\t\t\t%s\n" title version))
      (set-buffer cbuf)
      (goto-char (point-min))
      (while (re-search-forward "^[^ ]* [^ \n]*$" nil t)
	(seimei-region (match-beginning 0) (match-end 0))))))

;;;###autoload
(defun seimei-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (if (search-forward " " end t)
	(seimei (buffer-substring start (match-beginning 0))
		(buffer-substring (match-end 0) end))
      (error "Please Set Region on @姓<SPC>名@"))))

;;;###autoload
(defun seimei (sei mei)
  "姓名判断を行います。"
  (interactive "S姓名判断:「姓」をどうぞ>\nS次に「名」をどうぞ>")
  (setup-dic) ;辞書バッファの設定
;  (my-debugger-setup)
  (catch 'globaltag nil
	 (let ((res (get-buffer-create Seimei-Buffer))
	       (dic (get-buffer Seimei-Kanji-Kakusuu-Dic-Buffer))
	       (seimeilist nil)(seimeilen nil)(seimeikaku nil)(tmpkaku nil)
	       (tmp 0)(point0 0)(point1 0)(point2 0)(point3 0)(point4 0)
	       (orgtmp 0)(smsum 0)(slc 0)(lc 0))
	   (setq seimeilist (list (format "%s" sei) (format "%s" mei)))
	   (setq seimeilen (list (strlen sei) (strlen mei)))
	   (set-buffer dic)
	   (while (< slc 2) ;姓と名で2回ループする。
	     (while (not (eq (car seimeilen) lc))
	       (goto-char (point-min))
	       (if (search-forward
		    (substring (car seimeilist) lc
			       (+ lc seimei-char-width)) (point-max) t)
		   (setq tmpkaku (append tmpkaku
					 (list (count-lines (point-min)
							    (point)))))
		 (progn
		   (seimei-sorry-poor-dic sei mei
		    (substring (car seimeilist) lc (+ lc seimei-char-width)))
		   (throw 'globaltag nil)))
	       (setq lc (+ lc seimei-char-width)))
	     (setq seimeilist (cdr seimeilist));名をセット。
	     (setq seimeilen (cdr seimeilen)) ;名(の長さ)をセット。
	     (setq seimeikaku (append seimeikaku (list tmpkaku)))
	     (setq tmpkaku nil)
	     (setq lc 0)
	     (setq slc (1+ slc)))
	   (setq smsum ;姓画数合計と名画数合計をリストに格納。
		 (list (addcell (car seimeikaku))
		       (addcell (car (cdr seimeikaku)))))
	   
	   (setup-float-buffer)
	   
	   (set-buffer res)
	   (if seimei-erase-buffer
	       (progn
		 (erase-buffer)
		 (insert (format "%s\n\t\t\t\t\t%s\n" title version)))
	     (insert "\n"))
	   (insert (format "***** %s %s *****\n" sei mei))
	   (setq tmp (+ (car smsum)(car (cdr smsum))))
	   (setq orgtmp tmp)
	   (while (< 80 tmp) (setq tmp (- tmp 80)))
	   (setq point0 (car (cdr (nth tmp unsei))))
	   (insert (format "総運 %2d画 総合  \t %s\n" orgtmp (car (nth tmp unsei))))
	   (setq tmp (car smsum))
	   (setq orgtmp tmp)
	   (while (< 80 tmp) (setq tmp (- tmp 80)))
	   (setq point1 (car (cdr (nth tmp unsei))))
	   (insert (format "天運 %2d画 先祖運\t %s\n" orgtmp (car (nth tmp unsei))))
	   (setq tmp (car (cdr smsum)))
	   (setq orgtmp tmp)
	   (while (< 80 tmp) (setq tmp (- tmp 80)))
	   (setq point2 (car (cdr (nth tmp unsei))))
	   (insert (format "地運 %2d画 個性  \t %s\n" orgtmp (car (nth tmp unsei))))
	   (setq tmp (+ (car (reverse (car seimeikaku)))(car (car (cdr seimeikaku)))))
	   (setq orgtmp tmp)
	   (while (< 80 tmp) (setq tmp (- tmp 80)))
	   (setq point3 (car (cdr (nth tmp unsei))))
	   (insert (format "人運 %2d画 社会性\t %s\n" orgtmp (car (nth tmp unsei))))
	   (setq tmp (+ (if (null (cdr (reverse (car seimeikaku))))
			    (car (car seimeikaku))
			  (addcell (cdr (reverse (car seimeikaku)))))
			(if (null (cdr (car (cdr seimeikaku))))
			    (car (car (cdr seimeikaku)))
			  (addcell (cdr (car (cdr seimeikaku)))))))
	   (setq orgtmp tmp)
	   (while (< 80 tmp) (setq tmp (- tmp 80)))
	   (setq point4 (car (cdr (nth tmp unsei))))
	   (insert (format "外運 %2d画 環境運\t %s\n" orgtmp (car (nth tmp unsei))))
	   (insert (format "総運+人運 "))
	   (insert-power (* point0 point3) Pow1/2-7)
	   (insert (format "  天運+地運+外運 "))
					;    (debugger-insert (format "1 %s 2 %s 4 %s = %s" point1 point2 point4 (* (* point1 point2) point4) ))
	   (insert-power (* (* point1 point2) point4) Pow1/3-3)
	   (insert (format "  得点 " ))
	   (insert-float-add)
	   (display-buffer res)
;	   (delete-other-windows (selected-window))
;	   (set-window-buffer (split-window (selected-window)) res)
	   (seimei-fin))))

(defun insert-float-add ()
  (let ((afbuf (get-buffer Float-Buffer))
	(cbuf (current-buffer))(tmp 0)(upper 0)(lower 0))
    (set-buffer afbuf)
    (goto-char (point-min))
    (if (> (string-to-int emacs-version) 18)
	(progn
	  (set-buffer cbuf)
	  (setq tmp (number-to-string (+ (read afbuf) (read afbuf))))
	  (string-match "[^\\.]*\\.." tmp)
	  (insert (substring tmp (match-beginning 0) (match-end 0))))
      (setq tmp (read afbuf))
      (forward-line 1)
      (setq upper (+ tmp (read afbuf)))
      (goto-char (point-min))
      (read afbuf)
      (setq tmp (read afbuf))
      (read afbuf)
      (setq lower (+ tmp (read afbuf)))
      (if (< 9 lower)
	  (progn
	    (setq upper (1+ upper))
	    (setq lower (- lower 10))))
      (set-buffer cbuf)
      (insert (format "%s.%s\n" upper lower)))))

(defun setup-float-buffer ()
  (save-excursion
    (let ((tmp (get-buffer-create Float-Buffer)))
      (set-buffer tmp)
      (erase-buffer))))

(defun insert-power (num var)
  "var = Pow1/2-7 : sqrt(num) * 0.7という演算の結果をCバッファに出力。
   var = Pow1/3-3 : pow(num, 1/3) * 0.3という演算の結果をCバッファに出力。
   更に、加算バッファにも出力する。"
  (interactive "P")
  (let ((afbuf (get-buffer-create Float-Buffer))
	(flag t))
    (while flag
      (if (eq (car var) nil)
	  (setq flag nil)
	(if (eq (car (car var)) num)
	    (progn
	      (insert (format "%s" (car (cdr (car var)))))
	      (insert-string-to-buffer afbuf
				       (format "%s\n" (car (cdr (car var)))))
	      (setq flag nil))
	  (setq var (cdr var)))))))

(defun insert-string-to-buffer (buf str)
  (interactive "P")
  (save-excursion
    (set-buffer buf)
    (insert str)))

(defun addcell (lst)
  "lstに与えられたリストの要素を全て合計します。"
  (let ((tmp 0))
    (mapcar (function (lambda (n) (setq tmp (+ tmp n)))) lst)
    tmp))

(defun seimei-sorry-poor-dic (sei mei notfound)
  (let ((res (get-buffer Seimei-Buffer)))
    (set-buffer res)
    (erase-buffer)
    (insert (format "%s\n\t\t\t\t\t%s\n" title version))
    (insert (format "***** %s %s *****\n\n" sei mei))
    (insert (format "異国文字「%s」を含んだ名前は判断しかねます…。あしからず。\n" notfound))
    (delete-other-windows (selected-window))
    (set-window-buffer (split-window (selected-window)) res)))

(defun seimei-fin ()
  (let ((tmp (get-buffer Seimei-TMPBUFFER))
	(afc (get-buffer Float-Buffer)))
    (kill-buffer tmp)
    (kill-buffer afc)))

(defun seimei-quit ()
  (let ((res (get-buffer Seimei-Buffer))
	(dic (get-buffer Seimei-Kanji-Kakusuu-Dic-Buffer))
	(dbg))
    (seimei-fin)
    (kill-buffer res)
    (kill-buffer dic)
    (if (get-buffer "*debugger*")
	(kill-buffer (get-buffer "*debugger*")))))

(defun no-error-forward-char (num)
  (if (not (= (point) (point-max)))
      (forward-char num)))

(defun setup-dic ()
  "漢字画数辞書を設定します。既に同名のバッファがある時には再利用します。"
  (if (not (get-buffer Seimei-Kanji-Kakusuu-Dic-Buffer))
      (let ((dic (get-buffer-create Seimei-Kanji-Kakusuu-Dic-Buffer))
	    (sklist Seimei-Kaku-list))
	(set-buffer dic)
	(while sklist
	  (insert (format "%s" (car sklist)))
	  (setq sklist (cdr sklist)))
	(setq buffer-read-only t))))

(defun strlen (str)
  "文字列の長さを数えます。"
  (interactive "P")
  (save-excursion
    (let ((tmpbuf (get-buffer-create Seimei-TMPBUFFER)))
      (set-buffer tmpbuf)
      (erase-buffer)
      (insert (format "%s" str))
      (1- (point-max)))))

;(defun new-get-buffer-create (buf)
;  "既に同名のバッファがあるときには、それを消してから作り直します。"
;  (interactive "P")
;  (if (not (eq (get-buffer buf) nil))
;      (kill-buffer buf))
;  (get-buffer-create buf))

(defun my-debugger-setup ()
  "自分でつくらな、あかんとは…。"
  (save-excursion
    (let ((tmp (get-buffer-create "*debugger*")))
      (set-buffer tmp)
      (erase-buffer))))

(defun debugger-insert (str)
  "文字列を引数にとって、*debugger* bufferに書き出す。"
  (interactive "P")
  (save-excursion
    (let ((debugger (get-buffer "*debugger*")))
      (set-buffer debugger)
      (insert str))))

(defvar Pow1/2-7 '( (25 "3.5") (50 "5.0") (100 "7.0")
		   (200 "9.9") (400 "14.0") (500 "15.7")
		   (800 "19.8") (1000 "22.1") (1600 "28.0")
		   (3200 "39.6") (4000 "44.3") (6400 "56.0")
		   (8000 "62.6") (10000 "70.0")))

(defvar Pow1/3-3 '( (125 "1.5") (250 "1.9") (500 "2.4")
		  (1000 "3.0") (2000 "3.8") (2500 "4.1")
		  (4000 "4.8") (5000 "5.1") (8000 "6.0")
		  (10000 "6.5") (16000 "7.6") (20000 "8.1")
		  (32000 "9.5") (40000 "10.3") (50000 "11.1")
		  (64000 "12.0") (80000 "12.9") (100000 "13.9")
		  (128000 "15.1") (160000 "16.3") (256000 "19.0")
		  (320000 "20.5") (400000 "22.1") (512000 "24.0")
		  (640000 "25.9") (800000 "27.8") (1000000 "30.0")))

(defvar unsei '(
	("" 100)
	("勢いのある吉数" 80)
	("別離をしいられる凶数" 10)
	("調和と安定の吉数" 80)
	("破滅の人生を招く凶数" 10)
	("均衡を得る吉数" 80)
	("恵みに満ちた吉数" 80)
	("波乱を克する勇猛の吉数" 80)
	("忍耐が発展を生む吉数" 80)
	("不遇と孤独を暗示する凶数" 10)
	("すべてが空漠に帰する大凶数" 5)
	("たくましい成長を遂げる吉数" 80)
	("意志薄弱で挫折する凶数" 10)
	("才能に恵まれた吉数" 80)
	("苦労の多い非運の凶数" 10)
	("着々と発展する吉数" 80)
	("恵まれた境遇を得る吉数" 80)
	("剛毅で気迫のある吉数" 80)
	("才知と行動力に富む吉数" 80)
	("未完の大器に終わる半吉数" 40)
	("破綻と孤愁の凶数" 10)
	("独立と功名の吉数" 80)
	("無力感に悩まされる凶数" 10)
	("朝日が昇る勢いの吉数" 80)
	("人と物に恵まれる大吉数" 100)
	("勝気に過ぎて損する半吉数" 40)
	("波乱と異変の凶数" 10)
	("強情が不運を招く半吉数" 40)
	("変人奇運の大凶数" 5)
	("虚栄が災いする半吉数" 40)
	("試練が盛衰を決める半吉数" 40)
	("堅実に成功をつかむ大吉数" 100)
	("意外な幸運に恵まれる吉数" 80)
	("王者の風格あふれる大吉数" 100)
	("苦悩と悲痛の大凶数" 5)
	("知性・感性に富む吉数" 80)
	("無謀が災いを招く凶数" 10)
	("勤勉・実直な吉数" 80)
	("一芸に秀でる吉数" 80)
	("栄華をきわめる大吉数" 100)
	("慢心が敵をつくる半吉数" 40)
	("賢明な対応で成功する吉数" 80)
	("才能を生かせない半吉数" 40)
	("財運に見離される半吉数" 40)
	("失意に打ちのめされる凶数" 10)
	("苦難を克服できる吉数" 80)
	("明暗の著しい半吉数" 40)
	("念願が成就する吉数" 80)
	("才知と徳を備えた吉数" 80)
	("落差の激しい半吉数" 40)
	("幸運が長続きしない半吉数" 40)
	("吉凶の波間に浮かぶ半吉数" 40)
	("標的を射止める大吉数" 100)
	("見栄に苦しむ半吉数" 40)
	("災厄続きの悲惨な大凶数" 5)
	("極端にはしりやすい半吉数" 40)
	("失意と無力の凶数" 5)
	("希望の光に満ちた吉数" 80)
	("苦悩の後に幸福が待つ吉数" 80)
	("精神力の弱さで敗北する凶数" 10)
	("暗闇の不安に戦く凶数" 10)
	("自惚を自戒して発展する吉数" 80)
	("計画倒れになりがちな半吉数" 40)
	("順調に発展する大吉数" 100)
	("衰退をたどる凶数" 10)
	("新緑の輝きを放つ大吉数" 100)
	("野望ついえる凶数" 10)
	("幸運に浴する吉数" 80)
	("努力が必ず報われる吉数" 80)
	("窮乏・渋滞の凶数" 10)
	("破滅に向かう大凶数" 5)
	("平凡と忍従の半吉数" 40)
	("確実さに欠ける半吉数" 40)
	("晩年に栄える吉数" 80)
	("うつろな人生を送る凶数" 10)
	("平穏無事な吉数" 80)
	("思慮深く偏屈な凶数" 10)
	("明暗が交錯する半吉数" 40)
	("環境に流される半吉数" 40)
	("締まりのない凶数" 10)
	("無為・空漠の凶数" 10)
	("最極・復元の大吉数" 100)))

(defvar Seimei-Kaku-list '(
	"くしつのへノフヘレっー一乙丶丿亅\n"
	"いうこてとひめりろんアイカクコスセソトナニヌハヒマムメヤユラリル"
	"ワンぺプペぃぅァィャュ〃九七十人丁刀二入乃八卜又了力乂亠儿冂冖冫"
	"几凵勹匕匚匸卩厂厶\n"
	"あえかけさすせちにぬみむもやゆよらるれわウエオキケサシタチツテミ"
	"モヨロヲぐじづべぴブベパピぁぇゥェォッゃゅょョ々下干丸久及弓巾己"
	"乞口工叉才三山士子勺女小上丈刃寸千川大土之亡凡万巳也夕与个于兀刄"
	"囗夂夊孑宀尢尸屮巛已幺广廴廾弋彑彡彳\n"
	"おきそたねはふまをネホごでどびガグゴズゼゾドバビぉ井允引丑云円王"
	"化火牙介刈仇牛凶斤区欠月犬元幻戸五互午公勾孔今支止氏尺手什升少冗"
	"心仁壬水切双太丹中弔爪天斗屯内匂廿日巴反比匹不夫父仏分文片方乏毛"
	"木勿尤匁厄友予六弌丐亢从仍仄仆仂兮卆卅卞夬夭尹乢弖戈扎攴攵无旡曰"
	"歹殳毋气爻爿\n"
	"なほがげざずぜぢぱぷギゲザジダヂヅデポヴ圧以右卯瓜永凹央加可禾外"
	"且叶瓦刊甘丘旧去巨玉句兄穴玄乎古功巧広弘甲号込左冊札皿仕仔司史四"
	"市示叱失写主囚収汁出処召尻申世正生斥石仙占匝他打代台凧只叩旦庁辻"
	"汀田奴冬凸乍尼白半氾犯皮疋必氷付布弗払丙平辺弁戊母包北本末未民矛"
	"目矢由幼用立令礼丕丱丼弍仗仞仭仟囘册冉刋匆丗卉卮夘叮叨叭叺圦夲孕"
	"它尓屶戉朮癶艾辷\n"
	"ぎぞだばぶぽボ旭扱安伊夷衣亥芋印因吋宇羽迂臼曳汚仮会回灰各汗缶企"
	"伎危机気吉吃休吸朽汲兇共匡叫仰曲刑圭血件伍交光向后好江考行合此艮"
	"再在旨死糸至字寺次而耳自汐式芝守朱州舟充戎夙旬巡匠庄丞色尽迅成西"
	"舌先尖舛全壮早争存多対宅托辿団地弛池竹仲虫兆吊伝吐灯当同凪汝弐肉"
	"如任年肌伐帆汎妃百伏米忙朴毎亦迄牟名牝妄有羊吏両列劣老肋亙亘价伉"
	"伜决冱冲冰凩刔刎匈卍吁圷圸夛夸奸妁屹幵并忖戍戌扞扣扛扠扨攷朿朶杁"
	"朸朷汕汢犲网耒聿艸芍芒虍襾赱阡\n"
	"ぼ亜芦杏位囲医壱応沖牡何伽花我快戒改芥貝角苅完肝含岐希忌汽妓技却"
	"求灸究亨狂局均芹近吟玖串君祁形系芸迎決見言呉吾坑孝宏抗攻更劫克告"
	"困些佐沙坐災材冴坂阪作伺志孜私似児宍社車杓灼寿秀住初助序床抄肖条"
	"杖状伸臣芯身辛図吹杉声赤折宋走即束足村汰妥体択沢但辰谷男町沈佃低"
	"呈廷弟兎杜努投豆禿酉沌呑那尿妊忍把芭売伯麦抜伴判釆否庇批尾肘扶芙"
	"吻扮兵別返甫呆芳邦坊妨忘防吠没妙杢戻冶役佑邑余妖抑沃来乱卵利李里"
	"良伶冷励呂労弄牢亊佚估佛佝佗佇兌冏况刧刪劬劭甸匣吽呀听吭吼吮吶吩"
	"吝呎囮坎圻址坏壯夾妝佞妣妍孚孛斈尨屁岌岑岔妛巫巵帋弃彷忻忤忸忱忰"
	"扼抂抉找抒抓抖抃抔抛收攸旱曵杆杞杠杙杣杤汞汪沂沍沚沁沛汾汨汳沒沐"
	"泛犹狃狆狄瓧甬甼疔皀皃矣竍糺罕肛肓肚芫芟芬乕豕豸邨阨阮阯\n"
	"阿宛姐或依委易育雨泳英奄延沿炎苑於往押旺欧殴岡価佳果河苛茄迦画芽"
	"怪拐劾拡学岳茅侃官岸玩岩奇祈季宜祇杵泣居拒拠享京供侠協怯況尭欣金"
	"狗苦具空屈沓卦径茎券肩弦呼固姑狐股虎佼効幸庚拘昂杭肯肱刻国忽狛坤"
	"昏昆妻采肴咋刷参使刺始姉枝祉肢事侍治竺実舎者邪若取受呪周宗叔述所"
	"妾尚承招昇昌松沼垂炊枢制姓征性青斉昔析拙岨狙阻卒其陀岱苔卓拓坦担"
	"知宙忠抽注苧帖長直坪定底抵邸泥的迭典店妬宕東到毒突届苫奈迩乳念杷"
	"波拝杯拍泊迫函板版彼披泌肥非枇表苗府怖斧阜附侮武服沸物併並歩奉宝"
	"庖抱放朋法泡房肪牧奔妹枚枕抹沫侭味岬命明免茂孟盲門夜弥油林例怜苓"
	"炉和枠乖亞亟佶侈侏侘佻佩佰侑佯來侖兒兔兩冽凭刮刳刹劼劵卷咏呵咎呟"
	"呱呷咒呻咀呶咄咐咆囹坩垈坡坿垉侫妲姆孥屆岫岻岶岼岷峅岾帚帙帑帛廸"
	"弩弭徃徂彿忝忿怡怙怐怩怛怕怫怦怏怺戔拔拗拑抻拆拈拌拊拂拇拉抬杲昊"
	"昃旻杳枉杰枩杼杪枌枋枦枡枅歿殀毟氓氛泄泱泓沽泗泅泝沮沱沾沺泯泙泪"
	"炙炒爭爬牀狎狒瓩畄疚疝盂矼祀秉穹竏籵罔羌肭冐肬臾舍苡苣苟苒苴苳苺"
	"范苻苹苞茆苜茉苙虱衫軋迚迪邯邱邵陂隶隹鼡\n"
	"娃哀姶茜虻按威為畏胃郁茨咽姻胤姥荏映栄洩盈疫怨屋卸音科架珂俄臥廻"
	"悔恢海界皆咳垣柿革恰括活冠巻姦柑看竿紀軌客虐逆急笈級糾峡挟狭粁衿"
	"粂軍係型契荊計建研県限孤弧故枯胡後侯厚垢巷恒洪皇紅荒郊香拷恨査砂"
	"哉砕咲削昨柵拶珊姿屍思指施持室狩首拾洲秋臭酋柔重祝俊春盾叙昭省乗"
	"城浄拭食信侵神甚帥是政星牲窃宣専泉浅洗染穿前祖奏相草荘送促則俗柁"
	"耐待怠胎退茸単炭胆段茶昼柱挑勅珍津追栂柘亭剃貞帝訂点度怒逃洞峠独"
	"栃南虹祢廼派盃背肺矧柏畑発叛卑飛毘眉美柊彦秒品負赴封風柄頁変便保"
	"胞某冒勃殆盆昧柾俣迷姪面籾耶約柳勇宥幽柚祐洋要洛律侶亮厘玲郎歪亰"
	"俔俟俎俘俑俚俐俤俥兪冑凾剏剄剋剌勁匍卻厖呰哇咢咸咥咬哄哈咨咫哂咤"
	"咾咼哘咯圀囿垓垠垳垤垪垰竒奕奐奎姨姜姙姚孩屎屏峇峙庠弯彖徊很徇恠"
	"怎怱恪恟恊恆恍恃恤恂恬恫扁拏拜挌拮拱挧挂拯拵畋斫昵昶昴昜昿曷朏枷"
	"柯枴柬枳柩枸柤柞柝柢柮枹柎柆柧殄殃洟衍洶洫洽洸洙洵洳洒洌炯炬炸炳"
	"炮爰爼牴狢狠狡珈玳珎玻珀瓮瓲瓰瓱畍畊畉畆疥疣癸皈盻眈眇眄矜砌砒禹"
	"禺秕穽竕竓笂紆紂缸罘胛胥胙胝胄胚胖脉舁茵茴茖茲茱荀茹荐荅茯茫茗茘"
	"莽衵衽衲袂訃迥迢迯酊閂陌陏陋韋韭鳬\n"
	"唖挨逢案員院烏唄浦益悦宴翁荻俺恩夏家荷華蚊峨晦害浬格核株釜栢桓莞"
	"陥既帰記起飢鬼桔砧宮挙卿恐恭胸脅桐倶矩屑栗桑訓郡珪恵桂桁倹兼剣拳"
	"軒原個庫娯悟候倖晃校浩紘耕航貢砿降高剛骨根唆差座挫宰栽砦剤財朔窄"
	"索桜殺捌晒桟蚕残師紙脂時疾柴射紗借酌弱殊珠酒修従峻准殉純書徐恕除"
	"哨宵将消症祥称笑辱唇娠振晋浸疹真秦針訊陣逗粋衰畝凄栖逝脆隻席脊扇"
	"栓栴閃租素倉捜挿造息捉速袖孫帯泰啄託狸耽値恥致畜逐秩衷酎凋朕通庭"
	"悌挺逓釘哲展徒途砥砺倒党凍唐套島桃涛討透胴匿涜特悩納能蚤破馬俳配"
	"倍梅狽這秤剥莫畠隼班畔般匪疲秘被桧姫紐俵豹病浜敏浮粉紛陛勉圃捕倣"
	"俸峰峯捧砲剖紡埋哩桝脈粍眠娘冥耗紋涌容浴莱流留竜旅凌料倫涙烈恋連"
	"朗浪狼倭脇乘亳俛倚倨倔倪倥倅俶倡倩倬俾俯們倆冓冤冦冢凅凉剞剔勍叟"
	"哥哦唏唔哽哮哭哺哢唳圄埀埃埆埔埒埓埖奚奘娥娟娑娜娉娚宦宸尅屓屐峩"
	"峽峺峭峪崋弉徑恚恁恷恣恙悁悍悃悚悄悛悖悒悧悋拿挈捐挾捍捏捩效旃旆"
	"旁旄晏晄晉晁晟栞框栩桀桍栲桎栫桙档殷氤氣浣涓浤浚浹浙涎涕涅烟烋烝"
	"烙狹狷珥珮珞珱畛畚痂疳痃疵疽疸疼疱皋皰盍眩眤眞眥眦眛砠祠祗祟祚祕"
	"祓秧秬秡秣窈站竚竝笏笊笆笄笋粃粐紜紕紊缺罟罠羔翅翆耆耄耘耙耿耻胯"
	"胱胼舐舩舫舮芻莓莅莚莪莟莢莖茣莎莇莊荼莵荳荵莠莉莨虔蚓蚣蚩蚪蚋蚌"
	"衄衂袁衾袗袒袮袙袢袍訖訐訌豈豺赳躬迴逅迹迺迸郢郤郛釖釟釡釛陜陞陝"
	"陟陦髟鬥鬯鬲\n"
	"悪梓袷庵尉惟異移萎域逸淫陰液掩焔黄桶菓貨械崖涯蛎殻郭掛笠梶喝渇葛"
	"椛兜乾勘患貫眼基寄規亀偽掬菊脚救球虚許魚強教郷菌躯偶釧掘袈啓掲渓"
	"畦経蛍訣倦健牽険現絃舷袴梧康控梗黒惚頃婚梱混痕紺彩採済祭斎細菜崎"
	"埼笹匙皐惨産斬紫視痔鹿雫執悉偲捨赦斜蛇釈寂授終習週渋宿淑粛術淳渚"
	"庶商唱娼捷梢渉章紹菖訟剰常情埴深紳進笥推酔崇据菅雀清盛惜戚責接設"
	"雪旋船措曽粗組爽掃掻曹巣窓側族唾舵堆袋逮第琢脱探淡蛋断窒猪著帳張"
	"彫眺頂鳥捗陳掴壷紬釣停偵梯笛添甜転堵屠菟都悼梼盗淘祷陶動堂萄得寅"
	"惇豚捺軟猫捻粘埜脳婆排敗培陪粕舶販挽菱畢彪票描彬貧瓶埠婦冨符部副"
	"閉偏娩菩崩烹萌訪望釦堀麻密務猛問野訳唯悠郵庸欲翌淀梨理陸率掠略琉"
	"粒隆梁涼猟陵淋累婁偃假偕偐偈做偖偬偸處凰剪剳剱勗匐匏區厠參曼唹啀"
	"啌售啜啅啖啗唸啝圉圈國堊埣堋梦婀婬婉娵娶婢婪孰寃寇寉將專崕崗崟崛"
	"崑崔崢崚崙崘帶帷弸彗從徙徘徠惧悗悸惓悴悽惆悵惘戛掖掎掀掫捶掏掉掟"
	"掵捫敖敕敍敘斛旌晞晝晤晧晨晢朖梳桷桿梟梏梭梔條梛梃梹桴梵梠梺梍桾"
	"欸欷殍毬毫淹渕渊涵淇淦涸淆淬淞淌淨淒淅淺淙淤淕淪淮渮烱焉烽牾犁倏"
	"猗猊猜猖猝琅珸瓠瓸瓷畩畤畧痍痊痒皎盖盒眷眸硅窕竡笳笘笙笞笵笨笶絅"
	"絋紮紲紿紵絆羞羝羚翊耜聊聆脛脩脣脯舂舸舳菴萓菫菎菽萃菘萋菁菷萇菠"
	"菲萍萢萠菻蚶蚯蛄蛆蚰蛉蚫衒袞袵袤袰袿袱裃裄覓訛訝訥谺豼戝貭貪貮跂"
	"趾趺軛軣逑逕逡逍逞逖逋逧逎扈酖酘釼釵釶鈬閇閊陷陲陬勒竟馗鹵麥麸\n"
	"葵握渥絢粟偉椅飲渦厩閏運雲営瑛詠越堰援甥奥温過賀絵開階凱街蛙鈎覚"
	"割萱粥寒喚堪換敢棺款間閑雁喜幾揮期棋稀貴欺喫黍給渠距喬暁極僅勤欽"
	"琴筋喰寓遇隅隈敬景軽戟結喧圏堅捲検硯減湖菰雇御喉慌港硬絞腔項詐最"
	"犀裁堺策傘散斯詞歯滋軸湿屡煮惹就衆集竣循順暑勝掌晶湘焼焦硝粧証詔"
	"象場畳植殖森診尋靭須酢厨遂随椙晴棲税絶善然曾疏疎訴創喪惣痩葬装測"
	"属揃尊遜詑堕惰替貸隊達巽棚湛短弾智遅筑着註貯喋朝脹超椎痛塚堤提程"
	"貼渡登塔搭棟湯痘等答筒統董童道敦遁鈍韮葱覗琶廃媒買萩博硲筈溌筏蛤"
	"斑飯晩番蛮悲扉斐費備琵弼筆逼媛評蛭斌富普葡葺復幅淵焚雰塀遍補募報"
	"傍帽棒貿満湊無椋婿棉貰悶愉揖湧猶裕遊雄揚揺葉遥陽絡落嵐痢裡葎硫量"
	"琳塁裂煉廊禄惑椀湾腕舒傀傚傅冕剴剩勞凖厦厥啣喙喀喊喟啻啾喘喞單啼"
	"喃喩喇喨圍堙堝堡堽壺壹壻奢奠媚孳寔寐孱嵜嵌嵒嵎嵋幄幃幀幇廁廂弑彭"
	"徨悳惡惠愕惶愀惴惺愃愡惻惱愎戞掣掾揩揀揆揣揉插揶揄敞敝晰暃暎朞椏"
	"椁棊椈棘椢椦棡椌棍棔棧棕椒椄棗棣椥棹棠棯椨椪椚椣椡棆盜欹殘殕殼毳"
	"毯渭湮渙湲湟渾渣湫渫湶湍渟湃渺湎渤渝游溂焜焙爲牋犂犇猴猯猩猥琥琲"
	"琺甦畫畭畴痙痣痞發皖皓皴睇硴祺稈稍窗窘窖竢竦筐筍筌筅筬筝粤粭粢粫"
	"粡粨絳絖絎絲絨絮絏絣翕翔耋聒腋隋腆脾腓腑萸葭萪萼蒄葷葫葮蒂葩葆萬"
	"葯葹萵葢蛔蛞蛩蛬蛟蛛蛯裙覃覘觚觜觝訶詁詛詒詆詈貂貽貳貶賁赧趁跏跚"
	"跖跌跛跋躰軼軻軫辜逶逵逹鄂酣酥釉鈞釿鈔鈕鈑鈩閔閖隍靫靱颪馭馮黹堯\n"
	"愛葦飴暗意違溢碓園煙猿遠鉛塩嫁暇禍嘩蛾雅解塊慨碍蓋該較隔楽滑褐蒲"
	"勧寛幹感漢頑棄義詰業禁禽愚虞窟靴群傾携継罫詣隙傑嫌献絹遣源誇跨鈷"
	"鼓瑚碁溝鉱腰嵯裟債催塞歳載罪榊碕搾嗣獅詩試資雌飼慈蒔辞嫉腫愁蒐酬"
	"舜楯準署傷奨照蒋詳鉦蒸飾触寝慎新腎睡瑞嵩数裾勢聖誠跡摂節戦煎羨腺"
	"詮賎践禅塑楚遡鼠僧想蒼賊続損楕滞腿滝蛸嘆暖痴稚置馳蓄牒腸跳賃槌椿"
	"碇禎艇鼎溺鉄填殿電塗塘働督椴頓楢馴畷楠農牌楳煤漠鉢鳩塙搬煩頒微蒜"
	"楓福腹墓蓬蜂豊飽睦幌幕蓑稔夢盟滅摸蒙爺靖愈猷誉預傭楊溶蓉裸雷酪裏"
	"溜虜稜鈴零廉漣蓮賂路楼榔話賄詫碗亂亶會傴傲僉傳僂剿剽勣勦飭勠匯嗚"
	"嗅嗟嗄嗜嗤嗔圓嗇塢塋塰毀塒壼奧媼媾嫋嫂媽嬲嫐寞尠嵬嵳嵶幎廈彁彙徭"
	"慍愆惷愍愾愧慊愼愴愽慄戡搜搖搆搓搦搶搗搨搏斟旒暈暉暄暘椶楹楷楜楸"
	"楫楔楾楮椹楴椽楙椰楡楞楝榁楪飮歇歃毓溪溘滉溷滓溽溯滄溲滔溏溥滂溟"
	"煥煦煢煌煖煬猾獏瑕琿瑟瑙瑁瑜瑶甞畸當痾痿痼瘁痰痺痲痳皙盞睚睨睫睛"
	"睥矮碎碆硼碚碌祿稘稙稠稟禀筺筵筥筴筧筰筱筮粳粲粱粮糀經綉絛綏絽綛"
	"綟罨罩罧羣耡聘肄肆肅腱腮腥腦腴腟舅艀蒭蓊蒹蒿蒟蓙蓍蒻蓚蓐蓁蓆蓖蒡"
	"號蜒蜆蜈蜀蜃蛻蜑蜉蜍蛹蜊衙裔裘裝褂裼裨裲褄觧詼詭詬詢誅誂誄豢貉貅"
	"貊貲賈賍跪跫跟跣躱軾輊輅輌辟遏遐遑遒遉逾遖鄒酩鉞鉗鉅鉉鉤鉈鈿鉋鉐"
	"鉚閘閙閠隘隕隗雎雋雉雍雹靹韵頏頌飩飫骭髢鳧鳰鳫麁黽\n"
	"斡綾維稲蔭隠嘘蔚餌駅榎厭演嘉寡歌箇魁概劃廓赫閣樺鞄慣潅管関旗疑漁"
	"僑境銀駆窪熊頚語誤構綱膏酵閤豪穀酷獄漉魂瑳際察雑算酸誌爾磁漆蔀遮"
	"種綬需銃塾緒嘗彰裳障蝕榛塵翠頗摺精製誓静碩説煽銭銑漸噌層槍漕総綜"
	"聡遭像増憎駄態奪竪歎端綻蜘嫡徴暢銚漬蔦綴摘滴適嶋銅徳読鳶賑認寧箔"
	"駁箸肇髪罰閥碑緋樋鼻稗漂腐複聞碧蔑箆輔慕暮鳳貌鉾僕墨槙膜慢漫蔓箕"
	"蜜銘鳴綿模網餅誘様熔踊璃僚領緑瑠暦歴練漏蝋僊僖僞僥僭僣僮兢冩匱厮"
	"厰嘔嗷嘖嗾嗽嘛嗹團圖塲塹墅壽夐夥奬奩嫣嫗嫦嫩嫖孵寤實寢寥對嶌嶇嶄"
	"嶂幗幔麼廐廏廖慇愨愿愬慂慷慘慚慴慯慥慱慟慝慓慵截搴摧摶摎敲暝曄榲"
	"榮槐榿槁槓榾槎寨槊槝榻槃榧樮榑榠榜榕榴槞榱歉殞滿漑滬滸滾滲漱滯漲"
	"滌漾漓滷煕熈熏熄熕犒犖獎瑯瑰瑣瑪甄甃甅瘋瘍瘉皸皹盡睿睾睹碣碵碪碯"
	"禊稱窩竭竰箝箘箟箍箜箚箋箒箏箙粹粽綺綮綣綵緇綽綫綢綯綸綰緕翡聚聟"
	"聢膃膈膊膀膂臧臺與蔆蔡蓿蓴蔗蔘蔟蔕蔔蓼蜴蜿蜷蜻蜥蜩蜚裹裴褌褊褓褝"
	"覡誨誡誑誥誦誚誣貍趙跼踈踉跿輕輒辣遘遞鄙酳酲銕銜銖銓銛鋩閨閧靤鞅"
	"靼鞁靺鞆韶颯颱馼骰髣髦皷齊槇遙瑤\n"
	"鞍慰遺噂影鋭謁閲縁横鴎億稼蝦課餓駕撹確潟噛歓澗監緩諌翫器嬉毅畿輝"
	"儀戯誼窮蕎緊駈駒勲慶慧稽劇撃潔権糊稿麹撮撒賛暫賜質蕊趣熟潤遵醇諸"
	"鋤廠樟蕉衝賞嘱審震諏澄請蝉撰潜箭線選遷糎槽蔵諾誰箪誕談鋳駐樗潮蝶"
	"調墜槻潰締鄭敵徹撤賭蕩踏導憧撞鴇縄熱撚播罵輩賠蝿箱幡範盤磐蕃罷誹"
	"髭膝標瓢廟鋲賓敷膚賦撫舞蕪噴墳憤幣弊蔽僻篇編舗鋪穂褒鋒暴頬撲摩魅"
	"緬黙憂窯養慾履劉慮寮諒遼輪霊憐魯論蕨價僵儉儁儂儚冪凛劍劈匳噎噐嘴"
	"嘶嘲嘸嘯墹墟墫墸墮嫺嫻嬌嬋寫嶢嶝嶐幟幢幤廣廝廚廛廢廡彈慳慙慫憇憬"
	"憔憚憫憮戮摯撕撓撥撩撈數槨樂樛槿槹槲槧樅樞槭樔槫樊樒樣樓樌樢歐殤"
	"毆麾滕潁漿澆潺潸澁潯潛潭澂潼潘澎澑潦濆潴熨熬獗瑩瑾璋甍瘟瘧瘠瘡瘢"
	"瘤皚皺瞎瞋瞑磑磆磋磔碾碼磅磊禝稻稾稷穃窰篋篁篌篏箴篆糅糂緜緘緝緤"
	"緞緲緡縅罸羯羮翦翩聨膠膕膤膣膓膵舖蕚蔬蕀蕣蕘蕈蕁蕋蕕蝠蝟蝸蝌蝎蝴"
	"蝗蝨蝮蝙蝓蝣蝪蟒褞褥褪褫諄諍諂諚豌貎賤賣賚踝踞踐踟踪輙輓輜輟輛輦"
	"遨遯鄲鄰醋醉醂鋏銹銷錺錵閭霄霆靠鞋鞏鞐頤頡餃餉駟駛駝駘駑髯髫髮髴"
	"髱鬧魄魃魴鴉鴈鴃鴆麩麪黎齒\n"
	"穐鮎緯謂窺叡穎頴衛燕薗鴛鴬憶穏壊懐骸獲樫鴨憾翰還館舘機徽橘鋸禦彊"
	"橋興凝錦薫憩激憲賢諺醐衡鋼甑墾錯錆餐諮鴫縞錫儒樹輯獣縦薯樵鞘壌嬢"
	"錠薪親錐錘整醒積薦膳操黛醍濁樽壇築諜諦蹄澱燈糖頭篤橡噸曇薙謎燃濃"
	"薄縛醗噺繁避錨蕗鮒奮壁縫膨謀穆磨麺薬薮諭輸融擁謡頼龍燐隣隷錬篭録"
	"豫儘儕儔冀劔劒劑辨辧勳勵噫噤噬噪嚆圜墺墻壅嬖學寰嶬嶮廨廩彜徼憙憖"
	"憊憑懌懊懈懆憺罹懍戰擔撼據擒擅擇撻擂旙曁暹曉暾暼暸橄橲樶橸橇橢橙"
	"橦橈樸歙歔殪殫濂澳澣澡澤澹澪濛燗熹熾燒燉燔燎默獪獨璢璞甌甎疂瘴瘰"
	"瘻盥盧瞠瞞磬磧磚窶篝篩簑篦篥篳糘糒糢緻縊縣縡縒縟縉縋縢羲耨膩膰臈"
	"臻艙艘薀薤薈薑薊薨蕭薔薛薇薜蕷蕾薐螢螟螂蟇蟆衞襁褶褸覩覦諫諳諧諤"
	"諱謔諠諢諷諞諛諡豎豬赭蹂踵踰踴躾輳輻輹遶隨遲邁錏鋺鍄錮錙錢錚錣錻"
	"閼閻閹閾險隧霍雕霈霓霎霑霏霖霙靜靦頸頷頽餒餔餘餝駭駮駱駲駢骼髷髻"
	"鬨鮓鮃鮑鮖鮗鴪鴦鴣鴟鴕鴒麈麭黔龜\n"
	"闇磯嬰襖臆霞嚇橿轄竃環韓癌擬犠鞠矯謹櫛鍬繋謙鍵厳檎糠講購鴻壕濠懇"
	"鮭擦薩鮫燦篠謝爵繍醜縮駿曙償礁醤鍾燭趨績繊鮮燥糟霜戴濯鍛檀聴鎚鍔"
	"嬬擢鍍謄瞳瀞鍋濡膿頻糞瞥鮪儲優輿翼螺覧療瞭嶺齢聯藁儖儡簒燮營嚀嚊"
	"嚏壓壑壗嬪嬶孺嶽嶷嶼彌應懃懋懦戲擘擱擧舉擠擡擣擯斂曖曚朦檜檐檍檠"
	"檄檢檣檗檬檪歛氈濤澀濟濕濬濔濘濱濮燠燬燧燵牆犧獰癇癈癆癘盪瞰瞶磽"
	"磴禧齋禪穗穉窿簔簀簇簓篷簗簍篶糜總縱繆縻縵縹繃縷縲縺罅翳聳聲聰膾"
	"膸膽臀臂膺臉舊艝艚艱藉薺藏薹藐虧螯蟋螽蟀蟐雖螫蟄螳螻蠎襃襄褻襌覬"
	"覯謌謇謚謖謐謗謠謨譁豁谿貔貘賽賺賻蹊蹇蹉蹌蹐蹈蹕轅轂輾邂遽邀邉醢"
	"鍜鍠鍼鍮鍖闊濶闃闍闌隱隲隰隸襍鞜顆颶餡餞餤餠馘駻駸騁鮟鮠鮨鮴鵄鵁"
	"鴿鴾鵆鵈麋黏黜點黝黻鼾齔龠\n"
	"鵜鎧穫額顎鎌簡観顔騎襟顕験鯉鵠鎖瞬藷擾穣織職雛蹟繕礎叢鎗騒贈題瀦"
	"懲鎮藤闘難禰嚢藩謬覆癖鞭翻繭麿癒曜濫藍糧臨類雙嚠嚔壙壘彝懣懴戳擶"
	"擴擲擺擽攅斃斷旛檮櫁櫃櫂檸檳歟歸殯瀉瀋濺瀑瀁瀏濾燻燼燹燿獵璧瓊甕"
	"甓癜瞹瞿瞼瞽瞻矇礇礒礑禮穡穢竅竄邃簣簧簪簟繦繧繝繖繞繙繚羂羃翹聶"
	"臍臑艟藪藕藝藥藜蟯蟲蟠襠覲觴謳鞫謦謫謾豐贄贅蹙蹤蹠蹣轌轉轆邇醫醪"
	"釐鎰鎬鎭鎔鎹闕闔闖雜霤鞨鞦鞣顏顋餬餮馥騏騅髀鬆鬩魏魍魎鯀鯊鮹鯆鯏"
	"鯑鯒鵝鵞鵤鵑鵐鵙麌黠鼕鼬\n"
	"鯵韻艶蟹贋願蟻鏡繰警鶏鯨鹸鯖璽識蹴髄瀬蘇藻臓騨鯛瀧寵鏑轍顛覇曝爆"
	"簸譜簿鵬霧鵡羅蘭離麗簾櫓麓勸嚥嚮壞壜壟嬾寳廬懷懶攀曠檻櫞櫑櫟櫚殱"
	"濳瀛瀚瀝瀘瀟爍牘犢獸獺瓣疆疇癡礪礙穩簷簫簽籀繹繪繩羆羹羶羸臘艤艢"
	"艨蘂藹蘊蘓蘋藾藺蘆蘢蠅蠏蠍蟾蟶蟷蠖襞襦襪覈譌譏譎證譖譛譚贊贇蹶蹲"
	"蹼躇軅轎辭邊醯鏖鏨鏥鏘鏃鏝鏐鏈鏤關隴霪鞳鞴韜韲餽餾饂騙鯣鯢鯤鯔鯡"
	"鯲鯱鯰鵲鶉鶇鵯鵺麒麕麑靡黼\n"
	"馨鰍巌議競響饗懸護纂鐘譲醸籍鐙騰櫨瀕耀欄鰐嚶嚴壥壤孃孅孀寶廰懺懽"
	"攘曦朧蘗櫪蘖灌瀰瀾瀲爐獻瓏癢蘯矍礦礬礫竇籌籏糯糲繼繻纃繽辮罌譱聹"
	"臙臚蘚蘰蠑蠕襤襭覺觸譫譟譬譯贏贍躁躅躄軆轗醵醴釋鏗鐚鐔鐓鐃鐇鐐鐡"
	"闡霰飄飃饉饅騫騷驀鬪鰕鰔鰉鰓鰌鰆鰈鰒鰊鰄鰛鶫鶚鶤鶩鹹黥黨齣齟齠齡\n"
	"鰯艦顧轟鐸鶴纏灘鰭魔躍鑓露儺儷囂嚼囁囃囀囈屬巉巍廱懼懾攝攜曩霸權"
	"櫻欅櫺歡殲爛瓔癨癩癪癧龝竈籃籔籐籖繿纈纉續纐纎罍飜臟艪蠣蠢蠡襯譴"
	"譽贐齎贔躋躊轜辯醺鐶鐫鐵鐺鑁闥闢霹飆饐饋饑饒饌驅驂驃騾髏鬘魑鰮鰥"
	"鰤鰡鰰鶯鶲鷄鷁鶻鶸鶺鷆鷏鷂麝黯齦齧\n"
	"鰻驚轡讃襲鱈聾儼儻竸囎巓巒彎懿攤灑疊疉癬禳穰竊籠籘籟鬻糴纒罎羇聽"
	"艫蘿蠧襴覽覿讀贓贖躓躑躔轢鑒鑄霽霾韃顫饕驕驍鬚鰺鱇鰲鱆鰾鷙鷓齬齪"
	"龕\n"
	"鰹鑑讐鱒鱗麟鷲巖戀攪攣攫變欒癰籤籥纓纔纖罐蠱襷讌讎躙轣轤邏鑛鑠鑢"
	"鑞鑚靨韈齏顯驛驗髑髓體髞鬟鱚鷸鷦鷭鷯黐黴\n"
	"鷺鷹囑囓癲矗纛羈艷蠶蠹衢觀讒讓讖讙軈釀鑪靄靆靈靂韆顰驟鬢魘鱠鱧鷽"
	"鼇齷齲齶\n"
	"欝廳攬灣籬糶臠蠻覊躡釁鑰鑵靉顱鬣鹽黌鼈\n"
	"欖矚讚躪鑷驢鬮鱶黶\n"
	"鑽鑼鑾顴顳驥驤鱸黷\n"
	"纜钁鑿驩鸚\n"
	"鬱欟爨驪鸛\n"
	"驫鸞\n" )
  "SEIMEI-KANJI-KAKUSUU-LIST*" )
