<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. 第五章-控制流</a></li>
<li><a href="#sec-2">2. 区块</a></li>
<li><a href="#sec-3">3. 语境</a></li>
<li><a href="#sec-4">4. 条件</a></li>
<li><a href="#sec-5">5. 迭代</a></li>
<li><a href="#sec-6">6. 多值</a></li>
<li><a href="#sec-7">7. 中止</a></li>
<li><a href="#sec-8">8. 日期运算</a></li>
</ul>
</div>
</div>

# 第五章-控制流<a id="sec-1" name="sec-1"></a>

# 区块<a id="sec-2" name="sec-2"></a>

Common Lisp有三个构造区块的基本操作符 `progn` ， `block` ， `tagbody`

`progn` 使用最后一个表达式作为返回值，因此涵盖了副作用。

`block` 第一个实参应为符号，作为区块的名字。在主体的任何地方，可以停止求值，并通过使用 `return-from` 指定区块的名字立即返回。

    (block head
      (format t "Here we go.")
      (return-from head 'idea)
      (format t "We'll never see this."))

`return-from` 函数允许程序从代码的任何地方退出。第一个实参是区块名，第二个实参是返回值。

`return` 宏把传入的参数当作封闭区块 `nil` 的返回值。

    (block nil
      (return 27))

`tagbody` 里面可以带一个标签，使用 `(go 标签)` 可以将控制权移交到标签后的表达式。

    (tagbody
       (setf x 0)
     top
       (setf x (+ x 1))
       (format t "~A " x)
       (if (< x 10)
           (go top)))

# 语境<a id="sec-3" name="sec-3"></a>

概念上来说， `let` 表达式等同于函数调用。函数可以用名字来引用，也可以通过使用一个lambda表达式从字面上来引用。

    (let ((x 2)
          (y (+ x 1)))
      (+ x y))
    ;; 等价于
    ((lambda (x y) (+ x y))
     2 (+ x 1))

`let*` 可以依赖同一个表达式所设立的另一个变量。

    (let* ((x 1)
           (y (+ x 1)))
      (+ x y))

`destructuring-bind` 宏是通用化的 `let` 。其接受单一变量，一个模式，一个或多个变量所构成的树。并将他们与某个实际的树所对应的部分做绑定。

    (destructuring-bind (w (x y) . z) '(a (b c) d e)
      (list w x y z))

# 条件<a id="sec-4" name="sec-4"></a>

`when` 函数接收一个测试表达式与一个代码主体，当测试表达式返回真，则对主体求值。

    (setf that 1)
    (when (oddp that)
      (format t "Humm, that's odd.")
      (+ that 1))

`unless` 函数接收相同的实参，但仅在测试表达式返回假时，才对主体求值。

    (setf that 1)
    (unless (evenp that)
      (format t "Humm, that's odd.")
      (+ that 1))

`cond` 函数允许多个条件判断，每个条件相关代码隐含在 `progn` 里。

    (defun our-member (obj lst)
      (if (atom lst)
          nil
          (if (eql (car lst) obj)
              lst
              (our-member obj (cdr lst)))))
    ;; 可以被定义成
    (defun our-member-cond (obj lst)
      (cond ((atom lst) nil)
            ((eql (car lst) obj) lst)
            (t (our-member-cond obj (cdr lst)))))

`cond` 子句的 `t` 条件永远成立，通常放在最后面，作为缺省的条件式。没有子句符合时，则 `cond` 返回 `nil` 。

`case` 表达式接收一个键值实参以及零个或多个键值相对应的子句。使用第一个参数的值被拿来和子句的键值做比较（使用 `eql` ）。如果匹配时，子句剩余的表达式会被求值，并将最后一个求值作为 `case` 的返回值。

    (defun month-length (mon)
      (case mon
        ((jan mar may jul aug oct dec) 31)
        ((apr jun sept nov) 30)
        (feb (if (leap-year) 29 28))
        (otherwise "unknown month")))

`case` 表达式缺省子句的键值可以是 `t` 或 `otherwise` 。如果没有子句符合时，或是子句之包含键值时， `case` 返回 `nil` 。

# 迭代<a id="sec-5" name="sec-5"></a>

`do` 第一个参数是说明变量规格的列表，形式如下， `initial` 和 `update` 形式是选择性的

    (variable initial update)

`do` 的 `update` 形式引入到多个变量。

    (let ((x 'a))
      (do ((x 1 (+ x 1))
           (y x x))
          ((> x 5))
        (format t "(~A ~A)  " x y)))

`do*` 的任何 `initial` 或 `update` 形式。

    (do* ((x 1 (+ x 1))
          (y x x))
         ((> x 5))
      (format t "(~A ~A)  " x y))

`dotimes` 给定某个 `n` ，将会从整数 `0` ，迭代至 `n-1` 。

    (dotimes (x 5 x)
      (format t "~A" x))

`dolist` 和 `dotimes` 初始列表的第三个表达式皆可省略，省略时为 `nil` 。

`mapc` 函数与 `mapcar` 类似，但不会 `cons` 一个新列表作为返回值，所以使用的唯一理由是为了副作用。

    (mapc #'(lambda (x y)
              (format t "~A ~A  " x y))
          '(hip flip slip)
          '(hop flop slop))

`mapc` 总是返回第二个参数。

# 多值<a id="sec-6" name="sec-6"></a>

Common Lisp 可以返回零个或多个数值，最多可以返回几个值取决于具体的实现，至少可以返回19个值。

内置的 `get-decoded-time` 返回 9 个数值来表示现在的时间：秒，分，时，日期，月，年，天，以及另外两个数值。

`values` 函数返回多个数值。它把传入的参数全部返回。

    (values 'a nil (+ 2 4))

如果一个 `values` 表达式，是函数主体最后求值的表达式，它所返回的数值变成函数的返回值。

    ((lambda () ((lambda () (values 1 2)))))

如果预期只有一个返回值，则第一个以外的之会被欸舍弃。

    (let ((x (values 1 2)))
      x)

不带实参使用 `values` ，是可能不返回值的。

    (values)
    (let ((x (values)))
      x)

可以使用 `multiple-value-bind` 接收多个参数。

    (multiple-value-bind (x y z) (values 1 2 3)
      (list x y z))
    
    (multiple-value-bind (x y z) (values 1 2)
      (list x y z))
    
    (multiple-value-bind (s m h) (get-decoded-time)
      (format t "~A:~A:~A" h m s))

可以使用 `multiple-value-call` 接收多个参数进行调用。

    (multiple-value-call #'+ (values 1 2 3))

`multiple-value-list` 是以 `#'list` 作为第一个参数来调用 `multiple-value-call`

    (multiple-value-list (values 'a 'b 'c))

# 中止<a id="sec-7" name="sec-7"></a>

可以使用 `catch` 和 `throw` 在数个函数调用里将控制权转移回来。

    (defun super ()
      (catch 'abort
        (sub)
        (format t "We'll never see this.")))
    
    (defun sub ()
      (throw 'abort 99))

一个 `catch` 表达式接受一个标签，标签可以是任何类型的对象，伴随着一个表达式主体。

一个 `throw` 带有特定标签会导致 `catch` 表达式直接返回。

如果没有一个 `catch` 符合与匹配的标签时， `throw` 会产生一个错误。

调用 `error` 同时中断了执行，本来会将控制权转移到调用树的更高点，它将控制权转移给Lisp错误处理器。

    (progn
      (error "Opps!")
      (format t "After the error."))

可以使用 `unwind-protect` 防止代码被 `throw` 与 `error` 打断。

一个 `unwind-protect` 接收任何数量的实参，并返回第一个实参的值。即使第一个实参的求值被打断，剩下的表达式仍然会被求值。

    (setf x 1)
    (catch 'abort
      (unwind-protect
           (throw 'abort 99)
        (setf x 2)))

# 日期运算<a id="sec-8" name="sec-8"></a>

日期转换为天数

    (defconstant month
      #(0 31 59 90 120 151 181 212 243 273 334 365))
    
    (defconstant yzero 2000)
    
    (defun leap? (y)
      (and (zerop (mod y 4))
           (or (zerop (mod y 400))
               (not (zerop (mod y 100))))))
    
    (defun date-num (d m y)
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

天数转换为日期

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
