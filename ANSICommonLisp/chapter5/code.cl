(dolist (x '(a b c d e))
  (format t "~A " x)
  (if (eql x 'c)
      (return 'done)))

(defun foo ()
  (return-from foo 27))


(defun read-integer(str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
	(if i
	    (setf accum (+ (* accum 10) i))
	    (return-from read-integer nil))))
    accum))

(tagbody
   (setf x 0)
 top
   (setf x (+ x 1))
   (format t "~A " x)
   (if (< x 10)
       (go top)))


;; condition
(defun our-member-cond (obj lst)
  (cond ((atom lst) nil)
	((eql (car lst) obj) lst)
	(t (our-member-cond obj (cdr lst)))))


;; iteration
(defun show-quares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i 1))))


;; multiple value
(multiple-value-bind (s m h) (get-decoded-time)
  (format t "~A:~A:~A" h m s))


;; abort
(defun super ()
  (catch 'abort
    (sub)
    (format t "We'll never see this.")))

(defun sub ()
  (throw 'abort 99))



;; date arithmetic
(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))

(apply #'+ mon)

(setf nom (reverse mon))

(setf sums (maplist #'(lambda (x)
			(apply #'+ x))
		    nom))

(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 334 365))

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
	   (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))


(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
	(dotimes (i (- y yzero) d)
	  (incf d (year-days (+ yzero i))))
	(dotimes (i (- yzero y) (- d))
	  (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y)
      366
      365))

(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
	    (d (- (year-days y)) (- d (year-days y))))
	   ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
	    (prev 0 d)
	    (d (year-days y) (+ d (year-days y))))
	   ((> d n) (values y (- n prev))))))

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
	    ((> n 59) (nmon (- n 1)))
	    (t (nmon n)))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
    (values m (+ 1 (- n (svref month (- m 1)))))))

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))
