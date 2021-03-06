<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. 第六章-函数</a></li>
<li><a href="#sec-2">2. 全局函数</a></li>
<li><a href="#sec-3">3. 局部函数</a></li>
<li><a href="#sec-4">4. 参数列表</a></li>
<li><a href="#sec-5">5. 实用函数</a></li>
<li><a href="#sec-6">6. 闭包</a></li>
<li><a href="#sec-7">7. 函数构造器</a></li>
<li><a href="#sec-8">8. 动态作用域</a></li>
<li><a href="#sec-9">9. 编译</a></li>
<li><a href="#sec-10">10. 使用递归</a></li>
</ul>
</div>
</div>

# 第六章-函数<a id="sec-1" name="sec-1"></a>

# 全局函数<a id="sec-2" name="sec-2"></a>

谓词 `fboundp` 用于判断是否有个函数的名字与给定的符号绑定。

    (fboundp '+)

如果一个符号是函数的名字，则 `symbol-function` 会返回它。

    (symbol-function '+)

可以通过 `symbol-function` 给函数配置某个名字。

    (setf (symbol-function 'add2)
          #'(lambda (x) (+ x 2)))

通过把 `defun` 的第一个实参变成形式的列表 `(setf f)` ，可以如下。第一个实参代表新的数值，剩下的实参代表了传给 `f` 的参数。

    (defun primo (lst) (car lst))
    
    (defun (setf primo) (val lst)
      (setf (car lst) val))
    
    ;; 调用第二个函数，会改变列表x的值。
    (let ((x (list 'a 'b 'c)))
      (setf (primo x) 480)
      x)

由于字符串是Lisp表达式，可以出现在代码的主体。字符串本身是没有副作用的，除非它是最后一个表达式，否则不会造成任何差别。如果让字符串成为 `defun` 定义的函数主体的第一个表达式，那么该字符串可以作为函数的文档字符串。

函数 `documentation` 可以获取函数的文档字符串。

    (defun foo (x)
      "Implements an enhanced paradigm of diversity"
      x)
    
    (documentation 'foo 'function)

# 局部函数<a id="sec-3" name="sec-3"></a>

通过 `defun` 或 `symbol-function` 搭配 `setf` 定义的函数是全局函数。

局部函数可以使用 `labels` 来定义，第一个实参是一个新局部函数的定义列表，形式如下

    (name parameters . body)

`labels` 表达式的剩余部分，调用 `name` 相当于调用 `(lambda parameters . body)` 。

    (labels ((add10 (x) (+ x 10))
             (consa (x) (cons 'a x)))
      (consa (add10 3)))

`labels` 表达式所定义的局部函数，可以被其他任何在此定义的函数引用，包括自己。

    (labesl ((len (lst)
                  (if (null lst)
                      0
                      (+ (len (cdr lst)) 1))))
            (len '(a b c)))

`do` 表达式同样可以被解释成调用递归函数。

    (do ((x a (b x))
         (y c (d y)))
        ((test x y) (z x y))
      (f x y))
    
    ;;等同于
    
    (labels (rec (x y)
                 (cond ((test x y)
                        (z x y))
                       (t
                        (f x y)
                        (rec (b x) (d y))))))

# 参数列表<a id="sec-4" name="sec-4"></a>

在函数的形参列表里的最后一个变量前，插入 `&rest` 符号(剩余参数)，那么当这个函数被调用时，这个变量会被设成一个带有剩余参数的列表。

    (defun our-funcall (fn &rest args)
      (apply fn args))

`&optional` 出现在函数的形参列表，说明参数是可选参数。

    (defun philosoph (thing &optional property)
      (list thing 'is property))

`&optional` 可以指定缺省值，默认的缺省值是 `nil` 。

    (defun philosoph (thing &optional (property 'fun))
      (list thing 'is property))

选择性参数的缺省值可以不是常量，可以是任何的Lisp表达式。若表达式不是常量，它会在每次需要用到缺省值时被重新求值。

`&key` 关键字参数是更灵活的选择性参数，如果放在一个形参列表，那在 `&key` 之后的形参都是选择性的。当函数被调用时，这些参数会被识别出来，参数的位置在哪不重要，而是使用符号 `:` 识别出来。

    (defun keylist (a &key x y z)
      (list a x y z))
    (keylist 1 :y 2)
    (keylist 1 :y 3 :x 2)

`&key` 关键字参数缺省默认为 `nil` ，可以在形参列表明确指定缺省值。

关键字与其他相关的参数可以被剩余参数(rest)收集起来，并传递给其他期望收到这些参数的函数。

    (defun our-adjoin (obj lst &rest args)
      (if (apply #'member obj lst args)
          lst
          (cons obj rest)))

`destructuring-bind` 宏在通常情况下，每个模式中作为第一个参数的子树，可以使用关键字参数。

    (destructuring-bind ((&key w x) &rest y) '((:w 3) a)
      (list w x y))

# 实用函数<a id="sec-5" name="sec-5"></a>

Lisp构造列表的标准做法之一是创建一个累积器 `acc` ，初始化是 `nil` ，并将之后的对象累积起来。累积完毕，反转累积器件。

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

# 闭包<a id="sec-6" name="sec-6"></a>

`typecase` 函数接受一个实参，并依照类型对对应的代码进行求值。

    (defun combiner (x)
      (typecase x
        (number #'+)
        (list #'append)
        (t #'list)))
    (defun combine (&rest args)
      (apply (combiner (car args))
             args))

如果函数在词法变量的作用域里被定义时，函数仍可以引用到那个变量，即便函数被作为一个值返回了，返回至词法变量被创建的上下文之外。

    (setf fn (let ((i 3))
               #'(lambda (x) (+ x i))))
    (funcall fn 2)

当函数引用到外部定义的变量时，这外部定义的变量称为自由变量。函数引用到自由的词法变量时，称之为闭包。只要函数还在，变量就必须一起存在。

在lambda表达式里面的变量num是自由的，因此，我们传递了一个闭包给 `mapcar` 。

    (defun add-to-list (num lst)
      (mapcar #'(lambda (x)
                  (+ x num))
              lst))
    
    (defun make-adder (n)
      #'(lambda (x)
          (+ x n)))
    
    CL-USER> (setf add3 (make-adder 3))
    #<CLOSURE (LAMBDA (X) :IN MAKE-ADDER) {100408998B}>
    CL-USER> (setf add27 (make-adder 27))
    #<CLOSURE (LAMBDA (X) :IN MAKE-ADDER) {1004137AAB}>

`complement` 函数接受一个谓词，并返回谓词的补数。

    (mapcar (complement #'oddp)
                        '(1 2 3 4 5 6))

# 函数构造器<a id="sec-7" name="sec-7"></a>

Dylan是Common Lisp和Scheme的混合物，它有大量返回函数的函数。

Dylan包含 `compose` ， `disjoin` ， `conjoin` ， `curry` ， `rcurry` 以及 `always` 。

`compose` 接受一个或多个函数，并返回一个依序将其参数应用的新函数。

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

`disjoin` 接受一个或多个谓词作为参数，当任一谓词返回真时，返回真。

    (defun disjoin (fn &rest fns)
      (if (null fns)
          fn
          (let ((disj (apply #'disjoin fns)))
            #'(lambda (&rest args)
                (or (apply fn args) (apply disj args))))))
    (mapcar (disjoin #'integerp #'symbolp)
            '(a "a" 2 3))

`conjoin` 接受一个或多个谓词作为参数，当所有谓词返回真时，返回真。

    (defun conjoin (fn &rest fns)
      (if (null fns)
          fn
          (let ((conj (apply #'conjoin fns)))
            #'(lambda (&rest args)
                (and (apply fn args) (apply conj args))))))
    
    (mapcar (conjoin #'integerp #'symbolp)
              '(a "a" 2 3))

如果考虑将谓词定义成集合， `disjoin` 返回传入的参数的并集，而 `conjoin` 则是返回传入参数的交集。

    cddr = (compose #'cdr #'cdr)
    nth = (compose #'car #'nthcdr)
    atom = (compose #'not #'consp)
         = (rccury #'typep 'atom)
    <= = (disjoin #'< #'=)
    listp = (disjoin #'< #'=)
          = (rcurry #'typep 'list)
    1+ = (curry #'+ 1)
       = (rcurry #'+ 1)
    1+ = (rurry #'- 1)
    mapcan = (compose (curry #'apply #'nconc) #'mapcar)
    complement = (curry #'compose #'not)

函数 `curry` 与 `rcurry` (right curry) 两者都接受一个函数及某些参数，并返回一个期望剩余参数的新函数。

    (defun curry (fn &rest args)
      #'(lambda (&rest args2)
          (apply fn (append args args2))))
    
    (defun rcurry (fn &rest args)
      #'(lambda (&rest args2)
          (apply fn (append args2 args))))
    CL-USER> (funcall (curry #'- 3) 2)
    1
    CL-USER> (funcall (rcurry #'- 3) 2)
    -1

`always` 函数接受一个参数并原封不动返回此参数的函数。

    (defun always (x)
      #'(lambda (&rest args)
          x))

# 动态作用域<a id="sec-8" name="sec-8"></a>

词法作用域的词法变量作用于局部变量。

动态作用域的特别变量作用于特别变量。

在词法作用域下，一个符号引用到上下文中符号名字出现的地方。

    (let ((x 10))
      (defun foo ()
        x))
    (let ((x 20)) (foo))

动态作用域，我们在环境中函数被调用的地方寻找变量。要使一个变量是动态作用域的，我们需要在任何它出现的上下文中声明它是 `special` 。

    (let ((x 10))
      (defun foo ()
        (declare (special x))
        x))

函数内的 `x` 就不会再引用到函数定义里的按个词法变量，但会引用到函数被调用时，当下所存在的任何特别变量 `x` 。

    (let ((x 20))
      (declare (special x))
      (foo))

新的变量被创建出来之后，一个 `declare` 调用可以在代码的任何地方出现。
`special` 声明是独一无二的，因为它可以改变程序的行为。

通过在顶层调用 `setf` 来设置全局变量，是隐式地将变量声明为特殊变量。

    (setf x 30)
    (foo)

在一个文件里的代码，如果不想依赖隐式的特殊声明，可以使用 `defparameter` 取代。

动态作用域的用途：用来暂时给某个全局变量赋新值。

`*print-base*` ，缺省值是10，可以改变为16显示十六进制数字输出。重新绑定 `*print-base*` 。

    (let ((*print-base* 16))
      (printc 32))

# 编译<a id="sec-9" name="sec-9"></a>

Common Lisp函数可以独立编译或每个文件编译。

`compiled-function-p` 用来检查一个函数是否有被编译。

    (compiled-function-p #'foo)

`compile` 函数可以编译某个函数。

    (defun habi (x)
      (+ x 1))
    (compile 'habi)

`compile-file` 编译整个Common Lisp文件。

# 使用递归<a id="sec-10" name="sec-10"></a>

要用递归来解决一个问题，需要做两件事
1.  你必须要示范如何解决问题的一般情况，通过将问题切分成有限小并更小的子问题。
2.  你必须要示范如何通过——有限的步骤，来解决最小的问题——基本用例。
