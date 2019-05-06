<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. 求值</a></li>
<li><a href="#sec-2">2. 数据</a>
<ul>
<li><a href="#sec-2-1">2.1. 符号</a></li>
<li><a href="#sec-2-2">2.2. 列表</a></li>
</ul>
</li>
<li><a href="#sec-3">3. 列表操作</a></li>
<li><a href="#sec-4">4. 真和假</a></li>
<li><a href="#sec-5">5. 函数</a></li>
<li><a href="#sec-6">6. 输入和输出</a></li>
<li><a href="#sec-7">7. 变量</a></li>
<li><a href="#sec-8">8. 赋值</a></li>
<li><a href="#sec-9">9. 函数式编程</a></li>
<li><a href="#sec-10">10. 迭代</a></li>
<li><a href="#sec-11">11. 函数作为对象</a></li>
<li><a href="#sec-12">12. 类型</a></li>
</ul>
</div>
</div>

# 求值<a id="sec-1" name="sec-1"></a>

`Common Lisp` 的求值规则是从左到右求值。

`quote` 是一个特殊的操作符，它的求值规则是原封不动的返回传过来的实参。

`Lisp` 提供 quote 作为一种保护表达式不被求值的方式

# 数据<a id="sec-2" name="sec-2"></a>

## 符号<a id="sec-2-1" name="sec-2-1"></a>

符号是英语的单词，无论怎么输入，通常被转换为大写。

符号通常不对自身求值，需要引用符号的话，需要在符号前加单引号 

## 列表<a id="sec-2-2" name="sec-2-2"></a>

列表是由被括号包住的零个或多个元素来表示。

使用list可以创建列表，list是一个函数。

    (list 'my (+ 2 1) "Sons")

如果一个列表被引用，则求值规则对列表自身来求值。如果没有被引用，则列表被视为代码，依求值规则对列表求值后，返回它的值。

    (list '(+ 2 1) (+ 2 1))

空列表有两种表示方法。使用 `()` 或者 `nil` 。

# 列表操作<a id="sec-3" name="sec-3"></a>

函数 `cons` 可以用来构造列表，如果传入的第二个实参是列表，则返回由两个参数所构成的新的列表。新列表为第一个实参加上第二个实参。

    (cons 'a '(a b c))
    
    (cons 'a (cons 'b nil))

取出列表元素的基本函数是 `car` 和 `cdr`

`car` 返回列表的第一个元素。

`cdr` 返回列表第一个元素之后的所有元素。

# 真和假<a id="sec-4" name="sec-4"></a>

在Common Lisp中，符号 `t` 代表逻辑真的缺省值。与 `nil` 相同， `t` 也是对自身求值的。

函数的返回值将被解释成逻辑真或逻辑假时，则此时称此函数为谓词（predicate），在Common Lisp里面，谓词的名字通常以p结尾。

`null` 函数和 `nil` 函数的参数为 =nil=，返回真。

`if` 表达式语法

    (if (test-expression)
        (then-expression)
        (else-expression))

任何非 `nil` 的东西，在逻辑上下文通通都被视为真。

逻辑操作符 `and` 和 `or` 有短路效应，是宏，可以绕过一般的求值规则。

# 函数<a id="sec-5" name="sec-5"></a>

符号是变量的名字，符号本身是以对象的方式存在。符号和列表必须被引用。

列表必须被引用，否则就会被视为代码。

符号必须被引用，否则会被当作变量。

    (defun our-third (x)
      (car (cdr (cdr x))))
    
    
    (defun sum-greater (x y z)
      (> (+ x y) z))

# 输入和输出<a id="sec-6" name="sec-6"></a>

`format` 函数是输出函数。

`format` 中 `~A` 表示了被填入的位置，而 `~%` 表示一个换行。

`read` 函数是一个完整的Lisp解析器，不仅可以读入字符，还可以解析读入的内容，解析成Lisp对象

# 变量<a id="sec-7" name="sec-7"></a>

`let` 操作符可以引入新的局部变量。

`let` 操作符语法

    (let ((bingding var1) (binding var2))
      (body))

`defparameter` 宏用于设置全局变量。

全局变量可以在任何地方取得，除了定义了相同名字的区域变量的表达式里。在命名全局变量的时候，我们通常以星号开始星号结束。

`defconstant` 宏用于设置全局变量。

    (defparameter *glob* 99)
    (defconstant limit (+ *glob* 1))

`boundp` 函数用于检查某些符号，是否为一个全局变量或常量。这里的 `symbol` 要用单引号或者 `quote` 括起来，否则符号会被求值。

    (boundp '*glob*)

# 赋值<a id="sec-8" name="sec-8"></a>

`setf` 是最常用的赋值操作符，可以用来为全局变量或局部变量赋值。

`setf` 第一个实参如果是符号，而且符号不是某个局部变量的名字，则会把这个符号设置为全局变量。

通过 `setf` 赋值，可以隐式地创建全局变量。但一般使用 `defparameter` 创建全局变量比较好。

    (setf x (list 'a 'b 'c))

如果传入 `setf` 的第一个实参是表达式或者变量名，第二个实参的值会被插入到第一个实参所引用的位置。

    CL-USER> (setf (car x) 'n)
    N
    CL-USER> x
    (N B C)

可以传入偶数个实参给 `setf`

    (set a 'b
         c 'd
         e 'f)

# 函数式编程<a id="sec-9" name="sec-9"></a>

函数式编程意味着编写利用返回值而工作的程序，不产生副作用（除了返回值，还会输出等操作）。

编写程序尽量少使用副作用

    (setf x (remove 'a x))

# 迭代<a id="sec-10" name="sec-10"></a>

`do` 宏是是最基本的迭代操作符。

`do` 宏第一个实参是一组变量的规格说明列表，每个元素可以是以下形式

    (variable initial update)

起初每个变量会被赋值为 `initial` ，每一次迭代，执行一次 `update`

`do` 宏第二个实参可以包含一个或多个表达式，第一个表达式用来测试迭代是否结束。接
下来在列表中的表达式会依序被求值，最后一个表达式的求值会被用作 `do` 宏的返回值，直到迭代结束。

    (defun show-squares (start end)
      (do ((i start (+ i 1)))
          ((> i end) 'done)
        (format t "~A ~A~%" i (* i i))))

`progn` 接受任意数量的表达式，依次求值，并返回最后一个表达式的值。

    (defun show-squares-recursion (i end)
      (if (> i end)
          'done
          (progn
            (format t "~A ~A~%" i (* i i))
            (show-squares-recursion (+ i 1) end))))

`dolist` 接受以下形式的参数 `(variable expression)` ，跟着一个具由表达式的函数主体，函数主体会被求值，而变量相继于表达式所返回的列表元素绑定。

    (defun our-length (lst)
      (let ((len 0))
        (dolist (obj lst)
          (setf len (+ len 1)))
        len))

# 函数作为对象<a id="sec-11" name="sec-11"></a>

`funciton` 是一个特殊的操作符，它会返回相关联的对象，无需引用（quote）它的实参。

\#' 可以作为 `funciton` 的缩写，这个缩写称为升引号。

`apply` 函数接受一个函数和实参列表，并返回把传入函数应用在实参列表的结果

    (apply #'+ '(1 2 3 4))

`funcall` 函数和 `apply` 做的事情一样，但不需要把实参包装成列表。

    (funcall #'+ 1 2 3)

`lambda` 表达式里的 `lambda` 不是一个操作符，只是一个符号。
在Common Lisp里，可以用列表表达函数，函数在内部会被表示成独特的函数对象。可以不需要 `lambda` 操作符

    (lambda (x) (+ x 100))

`defun` 宏，创建一个函数并给函数命名。
要直接引用一个函数，我们可以使用lambda表达式。
lambda表达式是一个列表，列表包含符号 `lambda` ，接着是形参列表，以及零个或多个表达式组成的函数体。

    (funcall #'(lambda (x) (+ x 100))
             1)

# 类型<a id="sec-12" name="sec-12"></a>

在Lisp里面，只有数值才有类型，而不是变量。  

Common Lisp类型 `t` 是所有类型的基类，所以每个对象都属于 `t` 类型。

函数 `typep` 接受一个对象和一个类型，然后判断对象是否为该类型。

    (typep 27 'integer)
