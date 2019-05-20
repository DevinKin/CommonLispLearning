;;1.将下列表达式翻译成没有使用 let 与 let* ，并使同样的表达式不被求值 2 次。
;;(a) (let ((x (car y)))
;;      (cons x x))
;; 调用匿名函数，传入 (car y)
((lambda (x) (cons x x)) (car y))

;;(b) (let* ((w (car x))
;;           (y (+ w z)))
;;      (cons w y))
((lambda (w)
   ((lambda (y)
      (cons w y)) (+ w z))) (car x))


;;2.使用 cond 重写 29 页的 mystery 函数。（译注: 第二章的练习第 5 题的 (b) 部分)
(defun mystery (x y)
  (cond
    ((null y) nil)
    ((eql (car y) x) 0)
    (t (let ((z (mystery x (cdr y))))
	 (and z (+ z 1))))))

;;3.定义一个返回其实参平方的函数，而当实参是一个正整数且小于等于 5 时，不要计算其平方。
(defun doubleX(x)
  (cond
    ((and (integerp x) (<= x 5)) x)
    (t (* x x))))


;;4.使用 case 与 svref 重写 month-num
(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))
(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))
(defun month-num (m y)
  (case m
    ((1 2) (svref month (- m 1)))
    ((3 4 5 6 7 8 9 10 11 12) (+ (svref month (- m 1))
				 (if (leap? y) 1 0)))))

;;5.定义一个迭代与递归版本的函数，接受一个对象 x 与向量 v ，并返回一个列表，包含了向量 v 当中，所有直接在 x 之前的对象：
;;recursion
(defun precedes-recursion(e v)
  (if (< (length v) 2)
      nil
      (if (equal e (aref v 1))
	  (union (list (aref v 0))
		 (precedes e (subseq v 1)))
	  (precedes e (subseq v 1)))))

;;iteration
(defun precedes-iteration (e v)
  (let ((len (length v))
	(res nil))
    (if (< len 2)
	nil
	(dotimes (pos len res)
	  (if (and (> pos 0) (equal e (aref v pos)))
	      (setf res (union res (list (aref v (- pos 1))))))))))


;;6.定义一个迭代与递归版本的函数，接受一个对象与列表，并返回一个新的列表，在原本列表的对象之间加上传入的对象：
;; recursion
(defun intersperse-recursion (sep lst)
  (if (null lst)
      nil
      (cons (car lst)
	    (if (null (cdr lst))
		nil
		(cons sep (intersperse-recursion sep (cdr lst)))))))

;; iteration
(defun intersperse-iteration (sep lst)
  (let ((res nil)
	(len (length lst)))
    (dotimes (pos len res)
      (setf res (append res (if (> pos 0) (list sep)) (list (nth pos lst)))))))


;;7.定义一个接受一系列数字的函数，并在若且唯若每一对（pair）数字的差为一时，返回真，使用
;; recursion
(defun siblings1 (&rest lst)
  (if (null lst)
      t
      (if (null (cdr lst))
	  nil
	  (and (= (abs (- (car lst) (car (cdr lst)))) 1)
	       (apply #'siblings1 (cdr (cdr lst)))))))

;; do
(defun siblings2 (&rest lst)
  "do version"
  (if (oddp (length lst))
      nil
      (if (or (null lst)  (null (cdr lst)))
	  nil
	  (do ((nlst lst (cddr nlst))
	       (fst (car lst) (car nlst))
	       (snd (cadr lst) (cadr nlst)))
	      ((or (null (car nlst)) (null (cdr nlst))) (return t))
	    (if (not (= 1 (abs (- fst snd))))
		(return nil))))))

(defun siblings3 (&rest lst)
  (if (oddp (length lst))
      (return-from siblings3 nil))
  (mapc #'(lambda (pair)
	   (if (not (= 1 (abs (- (car pair) (cdr pair)))))
	       (return-from siblings3 nil))) (make-pair-list lst))
  (return-from siblings3 t))

(defun make-pair-list (lst)
  (if (or (null lst) (oddp (length lst)))
      nil
      (progn
	(do ((fst (car lst) (car c))
	     (snd (cadr lst) (cadr c))
	     (c (cddr lst) (cddr c))
	     (res nil))
	    ((or (null fst) (null snd)) (return res))
	  (setf res (append res (list (cons fst snd))))))))


;;8.定义一个单递归函数，返回两个值，分别是向量的最大与最小值。
