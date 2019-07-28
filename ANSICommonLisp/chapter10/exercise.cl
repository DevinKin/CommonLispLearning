;;1.如果 x 是 a ， y 是 b 以及 z 是 (c d) ，写出反引用表达式仅包含产生下列结果之一的变量:
;;(a) ((C D) A Z)
`(,z ,x z)
;;(b) (X B C D)
`(x ,y ,@z)
;;(c) ((C D A) Z)
`((,@z ,x) z)

;;2.使用 cond 来定义 if 。
(cond (test a)
      (t b))

;;3.定义一个宏，接受一个数字 n ，伴随着一个或多个表达式，并返回第 n 个表达式的值:
;;> (let ((n 2))
;;    (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
;;3
(defmacro nth-expr (n &rest exprs)
  `(nth (- ,n 1) (list ,@exprs)))

;;4.定义 ntimes (167 页，译注: 10.5 节)使其展开成一个 (区域)递归函数，而不是一个 do 表达式。
(defmacro ntimes! (n &body body)
  `(labels ((rec (i) (if (> i 0)
                        (progn
                          ,@body
                          (rec (1- i)))
                        nil))
            )
     (rec ,n)))


;;5.定义一个宏 n-of ，接受一个数字 n 与一个表达式，返回一个 n 个渐进值:
(defmacro n-of (n expr)
  (let ((lst (gensym))
        (st (gensym)))
    `(do ((,st 0 (1+ ,st))
          (,lst nil (cons ,expr ,lst)))
         ((>= ,st ,n)
          (reverse ,lst)))))

;;6.定义一个宏，接受一变量列表以及一个代码主体，并确保变量在代码主体被求值后恢复 (revert)到原本的数值。
(defmacro revert (lst &body body)
  `((lambda ,lst ,@body) ,@lst))


;;7.下面这个 push 的定义哪里错误？
;;(defmacro push (obj lst)
;;  `(setf ,lst (cons ,obj ,lst)))

;;lst 为nil的时候，就报错
(let ((i -1)
      (lst '((1 2 3) '(4 5 6))))
  (push 0 (nth (incf i) lst)))

(let ((i -1)
      (lst '((1 2 3) '(4 5 6))))
  (mypush 0 (nth (incf i) lst)))


;;8.定义一个将其参数翻倍的宏:
(defmacro mydouble (x)
  `(setf ,x (* ,x 2)))

(let ((x 1))
  (double x)
  x)
