- [函数](#sec-1)
  - [可选形参](#sec-1-1)
  - [剩余形参](#sec-1-2)
  - [关键字形参](#sec-1-3)
  - [混合不同的形参类型](#sec-1-4)
  - [函数返回值](#sec-1-5)
  - [作为数据的函数](#sec-1-6)
  - [匿名函数](#sec-1-7)

# 函数<a id="sec-1"></a>

函数使用 `DEFUN` 宏来定义，基本结构如下

```common-lisp
(defun name (parameter*)
  "Optional document string."
  body-form*)
```

函数文档字符串可以通过 `DOCUMENTATION` 函数来获取。

## 可选形参<a id="sec-1-1"></a>

为了定义一个带有可选形参的函数，在必要形参的名字后面放置符号 `&optional` ，后接着可选形参的名字。

```common-lisp
(defun foo (a b &optional c d)
  (list a b c d))
```

可选形参可以提供默认值。

可选形参列表可以提供一个带 `-supplied-p` 后缀的谓词，判断该变量是否使用了可选形参的默认值， `T` 为使用了， `NIL` 为使用了调用者传递的参数。

```common-lisp
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(foo 1 2)
(1 2 3 NIL)

(foo 1 2 3)
(1 2 3 T)


(foo 1 2 4)
(1 2 4 T)
```

## 剩余形参<a id="sec-1-2"></a>

`&rest` 符号之后包括一堆形参。任何满足了必要和可选形参之后的其余所有实参都被收集到一个列表里面。

## 关键字形参<a id="sec-1-3"></a>

在任何必要的 `&optional` 和 `&rest` 形参之后，可以加上符号 `&key` 以及任意数量的关键字形参标识符。

关键字形参也可以提供一个默认值形式以及一个 `supplied-p` 变量名。

```common-lisp
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foo :a 1)
(1 0 1 NIL)
```

想让调用者用来制定形参的关键字不同于实际参数名，可以将形参名替换成一个列表。

```common-lisp
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))


(foo :apple 10 :box 20 :charlie 30)
(10 20 30 T)
```

## 混合不同的形参类型<a id="sec-1-4"></a>

如果同时使用 `&optional` 形参和 `&key` 形参的函数，可能就应该将它变成全部使用 `&key` 形参的形式。

## 函数返回值<a id="sec-1-5"></a>

`RETURN-FROM` 操作符可以立即以任何值从函数中间返回。第一个实参为它想要返回的代码块名。

```common-lisp
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))
```

## 作为数据的函数<a id="sec-1-6"></a>

`FUNCTION` 操作符的便捷写法 `#'`

`FUNCALL` 用于编写代码时确切知道传递给函数多少个实参。第一个实参是被调用的函数对象，其余的实参被传递到该函数中。

```common-lisp
(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(plot #'exp 0 4 1/2)
*
**
***
*****
********
*************
*********************
**********************************
*******************************************************
NIL
```

`APPLY` 用于编写代码时不清晰传递给函数多少个实参。第一个实参是被调用的函数对象，第二个实参是列表，函数应用在列表中的值上。

## 匿名函数<a id="sec-1-7"></a>

可以使用 `LAMBDA` 表达式创建匿名的函数，形式如下

```common-lisp
(lambda (parameters) body)
```

`LAMBDA` 表达式的另一重要用途是制作闭包，即捕捉了其创建时环境信息的函数。
