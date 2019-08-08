- [第7章-宏：标准控制构造](#sec-1)
  - [WHEN和UNLESS](#sec-1-1)
  - [COND](#sec-1-2)
  - [AND、OR和NOT](#sec-1-3)
  - [循环](#sec-1-4)
  - [DOLIST和DOTIMES](#sec-1-5)
  - [DO](#sec-1-6)
  - [强大的LOOP](#sec-1-7)

# 第7章-宏：标准控制构造<a id="sec-1"></a>

## WHEN和UNLESS<a id="sec-1-1"></a>

```common-lisp
(when (spam-p current-message)
  (file-in-spam-folder current-message)
  (update-spam-database current-message))
```

`when` 宏的定义，当条件为真时，执行代码块。

```common-lisp
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
```

`unless` 宏的定义，当条件为假时，执行代码块。

```common-lisp
(defmacro unless (condition &rest body)
  `(if (not ,condition)
       (progn ,@body)))
```

## COND<a id="sec-1-2"></a>

`COND` 宏用于多分之条件判断。

```common-lisp
(cond
  (test-1 form*)
  ...
  (test-N form*))
```

## AND、OR和NOT<a id="sec-1-3"></a>

`NOT` 函数是逻辑非，对真值取反。

`AND` 和 `OR` 是宏，实现了对任意数量子表达式的逻辑合取和析取操作，并被定义成宏以便支持“短路”特性。

## 循环<a id="sec-1-4"></a>

`DOLIST` 宏和 `DOTIMES` 宏没有 `DO` 操作符灵活，但是语法渐变。

`LOOP` 宏用了一种简洁的方式来表达特定的常用循环结构。

## DOLIST和DOTIMES<a id="sec-1-5"></a>

`DOLIST` 宏的基本形式如下

```common-lisp
(dolist (var list-form)
  (body-form*))
```

`DOTIMES` 宏是用于循环计数的高级循环构造，基本形式如下

```common-lisp
(dotimes (var count-form)
  (body-form*))
```

`DOTIMES` 和 `DOLIST` 都可以使用 `RETURN` 来提前中断循环。

## DO<a id="sec-1-6"></a>

`DO` 允许绑定任意数量的变量，并且变量值在每次循环中的改变方式也是完全可控的，也可以定义测试条件来决定何时种植循环，并可以提供一个形式，在循环结束时进行求值来为 `DO` 表达式整体生成一个返回值。基本形式如下

```common-lisp
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)
```

`variable-definition` 引入了一个将存在于循环体作用域之内的变量。单一变量定义的完整形式是一个包含三个元素的列表。

-   `step-form` 是可选的。

```common-lisp
(var init-form step-form)
```

在每次迭代开始时以及所有循环变量都被指定新值之后， `end-test-form` 会被求值，只要其值为 `NIL` ，迭代过程会继续，依次求值所有的 `statement` 。

## 强大的LOOP<a id="sec-1-7"></a>

`LOOP` 宏有两大类：简化的和扩展的。

简化的 `LOOP` 就是不绑定任何变量的无限循环，基本形式如下

```common-lisp
(loop
      (body-form*))
```

使用简化的 `LOOP` 宏。

```common-lisp
(loop
  (when (> (get-universal-time) *some-future-date*)
    (return))
  (format t "Waiting ~%")
  (sleep 60))
```

`LOOP` 宏对前十个平方数求和。

```common-lisp
(loop for x from 1 to 10 summing (expt x 2))
```

`LOOP` 宏计算第11个斐波那契数

```common-lisp
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return a))
```

符号 `across` 、 `and` 、 `below` 、 `collecting` 、 `counting` 、 `finally` 、 `for` 、 `from` 、 `summing` 、 `then` 和 `to` 都是一些循环的关键字，他们的存在表明当前正在使用扩展的 `LOOP` 。
