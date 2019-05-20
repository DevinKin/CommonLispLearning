(setf (symbol-function 'add2)
      #'(lambda (x) (+ x 2)))

(defun (setf primo) (val lst)
  (setf (car lst) val))

(let ((x (list 'a 'b 'c)))
  (setf (primo x) 480)
  x)


(defun foo (x)
  "Implements an enhanced paradigm of diversity"
  x)

(documentation 'foo 'function)


(defun philosoph (thing &optional property)
  (list thing 'is property))

(defun philosoph (thing &optional (property 'fun))
  (list thing 'is property))

(defun keylist (a &key x y z)
  (list a x y z))
(keylist 1 :y 2)
(keylist 1 :y 3 :x 2)


;; common utils
(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setf wins obj
		    max score))))
	(values wins max))))

(single? '(a))

(append1 '(a b c) 'd)

(map-int #'identity 10)

(map-int #'(lambda (x) (random 100))
	 10)

(filter #'(lambda (x)
	    (and (evenp x) (+ x 10)))
	'(1 2 3 4 5 6 7))

(most #'length '((a b) (a b c) (a)))


;; closures
(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
         args))


(setf fn (let ((i 3))
	   #'(lambda (x) (+ x i))))
(funcall fn 2)

(defun add-to-list (num lst)
  (mapcar #'(lambda (x)
	      (+ x num))
	  lst))

(defun make-adder (n)
  #'(lambda (x)
      (+ x n)))

(setf add3 (make-adder 3))


(let ((counter 0))
  (defun reset ()
    (setf counter 0))
  (defun stamp ()
    (setf counter (+ counter 1))))
(list (stamp) (stamp) (reset) (stamp))


(defun our-complement (f)
  #'(lambda (&rest args)
      (not (apply f args))))


;; Function Builders
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))
;; 构建一个函数，先给取参数的平方根，取整后再放回列表里，接着返回。
(mapcar (compose #'list #'round #'sqrt)
	'(4 9 16 25))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
	#'(lambda (&rest args)
	    (or (apply fn args) (apply disj args))))))

;; disjoin 接受一个或多个谓词作为参数，当任一谓词返回真时，返回真。
(mapcar (disjoin #'integerp #'symbolp)
	'(a "a" 2 3))


;; conjoin 接受一个或多个谓词作为参数，当所有谓词返回真时，返回真
(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args))))))

(mapcar (conjoin #'integerp #'symbolp)
	'(a "a" 2 3))


(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))
