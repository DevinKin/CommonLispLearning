;; 1.描述下列表达式求值之后的结果：
(+ (- 5 1) (+ 3 7))  ;14
(list 1 (+ 2 3))   ; (1 5)
(if (listp 1) (+ 1 2) (+ 3 4))  ; 7
(list (and (listp 3) t) (+ 1 2)) ; (nil 3)


;; 2.给出 3 种不同表示 (a b c) 的 cons 表达式
(cons 'a '(b c))
(cons 'a (list 'b 'c))
(cons () '(a b c))


;; 3.使用 car 与 cdr 来定义一个函数，返回一个列表的第四个元素。
(defun myFouth (lst)
  (car (cdr (cdr (cdr lst)))))


;; 4.定义一个函数，接受两个实参，返回两者当中较大的那个。
(defun myMax (arg1 arg2)
  (if (> arg1 arg2)
      arg1
      arg2))

;; 5.这些函数做了什么？
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))
;; 判断列表x中是否有nil的元素

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))
;; 返回x在列表y中的索引


;; 6.下列表达式， x 该是什么，才会得到相同的结果？
					;(car (x (cdr '(a (b c) d))))
					;result: B
(car (car (cdr '(a (b c) d))))
					; answer x=car

					;(x 13 (/ 1 0))
					;result: 13
(or 13 (/ 1 0))
					; answer x=or

					;(x #'list 1 nil)
					;result:(1)
(apply #'list 1 nil)
					;answer: apply


;;7.只使用本章所介绍的操作符，定义一个函数，它接受一个列表作为实参，如果有一个元素是列表时，就返回真。
(defun has-list-1 (lst)
  (and (not (null lst))
       (or (listp (car lst))
	   (has-list (cdr lst)))))

(defun has-list-2 (lst)
  (let ((flag nil))
    (dolist (ele lst)
      (if (listp ele)
	  (setf flag t)))
    flag))


;;8.给出函数的迭代与递归版本：
;;a.接受一个正整数，并打印出数字数量的点。
(defun exa(n)
  (do ((i n (- i 1)))
      ((< i 1) 'done)
    (format t "* ")))

(defun exa-recursion(n)
  (if (> n 0)
      (progn
	(format t "* ")
	(exa-recursion (- n 1)))))


;;b.接受一个列表，并返回 a 在列表里所出现的次数。 
(defun appear-count(lst)
  (let ((count 0))
    (if (listp lst)
	(dolist (ele lst)
	  (if (listp ele)
	      (setf count (+ count (appear-count ele)))
	      (if (eql ele 'a)
		  (setf count (+ count 1)))))
	(if (eql lst 'a)
	    (setf count (+ count 1))))
    count))


(defun appear-count-recursion(lst)
  (let ((count 0))
    (if (not (null lst))
	(if (listp (car lst))
	    (setf count (+ count (appear-count-recursion (car lst))))
	    (progn
	      (if (eql 'a (car lst))
		  (setf count (+ count 1)))
	      (setf count (+ count (appear-count-recursion (cdr lst)))))))
    count))


;;9.一位朋友想写一个函数，返回列表里所有非 nil 元素的和。他写了此函数的两个版本，但两个都不能工作。请解释每一个的错误在哪里，并给出正确的版本。
;; error version
(defun summit (lst)
  (remove nil lst)
  (apply #'+ lst))
;; correct version
(defun summit-fix (lst)
  (apply #'+ (remove nil lst)))


;; error version
(defun summit (lst)
  (let ((x (car lst)))
    (if (null x)
	(summit (cdr lst))
	(+ x (summit (cdr lst))))))
;; correct version
(defun summit2-fix (lst)
  (let ((x (car lst))
	(res (cdr lst)))
    (if (null x)
	(if (null res)
	    0
	    (summit2-fix res))
	(+ x (summit2-fix res)))))
