(defvar horoscope '(("3/21" "4/20" "â≤órç¿" "aries")
		    ("4/21" "5/21" "â≤ãçç¿" "taurus")
		    ("5/22" "6/21" "ëoéqç¿" "gemini")
		    ("6/22" "7/22" "äIç¿" "cancer")
		    ("7/23" "8/22" "éÇéqç¿" "leo")
		    ("8/23" "9/23" "â≥èóç¿" "virgo")
		    ("9/24" "10/23" "ìVîâç¿" "libra")
		    ("10/24" "11/22" "Â∂ç¿" "scorpio")
		    ("11/23" "12/21" "éÀéËç¿" "sagittarius")
		    ("12/22" "1/20" "éRórç¿" "capricorn")
		    ("1/21" "2/18" "êÖïrç¿" "aquarius")
		    ("2/19" "3/20" "ãõç¿" "pisces")))

(defun horoscope-list-form (horoscope-entry)
  (let ((start (car horoscope-entry))
	(end   (nth 1 horoscope-entry))
	st en l)
    (string-match "\\([0-9]*\\)/\\([0-9]*\\)" start)
    (setq st (+ (* 100 (string-to-number 
			(substring start (match-beginning 0) (match-end 0))))
		(string-to-number
		 (substring start (match-beginning 2) (match-end 2)))))
    (string-match "\\([0-9]*\\)/\\([0-9]*\\)" end)
    (setq en (+ (* 100 (string-to-number 
			(substring end (match-beginning 0) (match-end 0))))
		(string-to-number 
		 (substring end (match-beginning 2) (match-end 2)))))
    (list st en (nth 2 horoscope-entry))))

(defun horoscope1 (month day)
  (let ((horo horoscope)
	(d (+ (* 100 month) day)))
    (catch 'break
      (while horo
	(let* ((l  (car horo))
	       (il (horoscope-list-form l)))
	  (if (and (<= (car il) d) (<= d (nth 1 il)))
	      (throw 'break (nth 2 il))))
	(setq horo (cdr horo))))))

(defun horoscope (str)
  (interactive "sMonth/Day: ")
  (if (string-match "\\([0-9]*\\)/\\([0-9]*\\)" str)
      (let ((m (string-to-number (substring str (match-beginning 0)
					        (match-end 0))))
	    (d (string-to-number (substring str (match-beginning 2)
					        (match-end 2)))))
	(message (horoscope1 m d)))))

(provide 'horoscope)
