<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. 第五章-控制流</a></li>
<li><a href="#sec-2">2. 区块</a></li>
<li><a href="#sec-3">3. 语境</a></li>
<li><a href="#sec-4">4. 条件</a></li>
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

    (let ((x 7)
          (y 2))
      (format t "Number")
      (+ x y))
    ;; 等价于
    ((lambda (x y)
       (format t "Number")))

`let*` 可以依赖同一个表达式所设立的另一个变量。

    (let* ((x 1)
           (y (+ x 1)))
      (+ x y))

`destructuring-bind` 宏是通用化的 `let` 。其接受单一变量，一个模式，一个或多个变量所构成的树。并将他们与某个实际的树所对应的部分做绑定。

    (destructuring-bind (w (x y) . z) '(a (b c) d e)
      (list w x y z))

# 条件<a id="sec-4" name="sec-4"></a>
