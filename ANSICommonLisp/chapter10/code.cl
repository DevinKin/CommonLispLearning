;; eval函数
(eval '(+ 1 2 3))

(eval '(format t "Hello"))

;;顶层：读取-求值-打印循环
(defun our-toplevel ()
  (do ()
      (nil)
    (format t "~%> ")
    (print (eval (read)))))

;;coerce
(coerce '(lambda (x) x) 'function)

;; compile
(compile nil '(lambda (x) (+ x 2)))


;; macro
(defmacro nil! (x)
  (list 'setf x nil))

(macroexpand-1 '(nil! x))

;;Backquote
(setf a 1 b 2)
`(a is ,a and b is ,b)
(A IS 1 AND B IS 2)


(defmacro nil! (x)
  `(setf ,x nil))


(setf lst '(a b c))
`(lst is ,lst)
`(its elements are ,@lst)


;; 目标效果
(let ((x 0))
  (while (< x 10)
         (princ x)
         (incf x)))

;; while宏
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro ntimes (n &rest body)
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))

(defmacro ntimes (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
         ((>= ,g ,n))
       ,@body)))


(defmacro cah (lst)
  `(car ,lst))

(define-modify-macro our-incf (&optional (y 1)) +)

;; 实用的宏函数
(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))


;; for宏使用
(for x 1 8
  (princ x))
