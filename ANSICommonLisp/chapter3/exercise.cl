;; 2.写一个保留原本列表中元素顺序的 union 版本：
(defun new-union(lst1 lst2)
  (let ((nlst2 (copy-list lst2)))
    (dolist (ele1 lst1)
      (setf nlst2 (remove ele1 nlst2)))
    (append lst1 nlst2)))


;;3. 定义一个函数，接受一个列表并返回一个列表，指出相等元素出现的次数，并由最常见至最少见的排序：
(defun occurrences (lst)
  (let ((nlst nil))
    (dolist (ele lst)
      (if (null (assoc ele nlst))
	  (setf nlst (append nlst
			     (list (cons ele (countele ele lst)))))))
    (mysort nlst)))

(defun mysort(lst)
  (sort lst #'(lambda (x y)
		(if (> (cdr x) (cdr y))
		    t
		    nil))))

(defun countele(n lst)
  (let ((count 0))
    (dolist (ele lst)
      (if (eql n ele)
	  (setf count (+ count 1))))
    count))


;;4. 为什么 (member '(a) '((a) (b))) 返回 nil？
					;answer: 官方文档可知，member只搜索top level的List。
					;然而此处的top level的List是指一个包含了(a)，(b)元素的列表和nil组成的一个Cons对象。
					;member使用了eql来比较对象,'(a) 和 ((a) (b))里面不是同一个对象。


;;5.假设函数 pos+ 接受一个列表并返回把每个元素加上自己的位置的列表：
;; recursion
(defun pos+recursion (lst)
  (if (null lst)
      nil
      (cons (car lst) (mapcar #'(lambda (x) (incf x)) (pos+recursion (cdr lst))))))

;; iteration
(defun pos+iteration (lst)
  (let ((pos 0)
	(nlst nil))
    (dolist (ele lst)
      (setf nlst (append nlst (list (+ ele pos))))
      (setf pos (+ pos 1)))
    nlst))

;; mapcar
(defun pos+mapcar (lst)
  (let ((pos 0))
    (mapcar #'(lambda (x)
		(setf x (+ x pos))
		(setf pos (+ pos 1))
		x)
	    lst)))

;;6.经过好几年的审议，政府委员会决定列表应该由 cdr 指向第一个元素，而 car 指向剩下的列表。定义符合政府版本的以下函数：
(defun mycons (obj1 obj2)
  (let ((lst '(nil . nil)))
    (setf (cdr lst) obj1
	  (car lst) obj2)
    lst))

(defun mylists (&rest objects)
  (let ((end (car (last objects)))
	(nlst nil))
    (dolist (ele (cdr (reverse objects)))
      (push ele nlst))
    (if (not (null end))
	(push end nlst))
    nlst))


(defun mylength(lst)
  (if (not (null lst))
      (+ 1 (mylength (car lst)))
      0))

(defun mymember(item lst)
  (if (equal item (cdr lst))
      lst
      (mymember item (car lst))))



;; 7.修改图 3.6 的程序压缩算法程序，使它使用更少 cons 核。 （提示：使用点状列表）
;; 不懂


;; 8.定义一个函数，接受一个列表并用点状表示法印出：
(defun showdots (lst)
  (let ((next (car lst)))
    (if (consp next)
	(progn
	  (showdots next)
	  (format nil "(~A . ~A)" next (showdots (cdr lst))))
	(if (null lst)
	    nil
	    (format nil "(~A . ~A)" next (showdots (cdr lst)))))))

;;9.写一个程序来找到 3.15 节里表示的网络中，最长有限的路径 (不重复)。网络可能包含循环。
