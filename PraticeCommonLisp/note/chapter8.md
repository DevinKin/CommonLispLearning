- [第8章-如何自定义宏](#sec-1)
  - [DEFMACRO](#sec-1-1)
  - [示例宏：do-primes](#sec-1-2)
  - [宏形参](#sec-1-3)
  - [生成展开式](#sec-1-4)
  - [堵住漏洞](#sec-1-5)
  - [用于编写宏的宏](#sec-1-6)

# 第8章-如何自定义宏<a id="sec-1"></a>

理解宏的关键在于必须清楚地知道那些生成的代码的代码(宏)和那些最终构成程序的代码(所有其他内容)之间的区别。

宏运行的时期被成为宏展开期，和运行期不同，后者是正常代码(包括由宏生成的代码)实际运行的阶段。

宏展开期间无法访问到那些仅存在于运行期的数据。

下面的函数，将 `x` 设为一个变量，用它保存传递给一个对 `foo` 调用的实参。但在宏展开期，比如当编译器正在运行 `WHEN` 宏的时候，唯一可用的数据就是源代码。

由于程序尚未运行，也没有对 `foo` 的调用，因此也没有值关联到 `x` 上。相反，编译器传递给 `WHEN` 的值只是代表源代码的 `Lisp` 列表，也即 `(> x 10)` 以及 `(print 'big)` 。

```common-lisp
(defun foo (x)
  (when (> x 10)
    (print 'big)))
```

宏的作用是生成某些事情的代码，并非直接做任何事情。

## DEFMACRO<a id="sec-1-1"></a>

`DEFMACRO` 的基本形式如下

```common-lisp
(defmacro name (parameter*)
  "Optional documentation string."
  body-form*)
```

编写宏的步骤如下

-   编写示例的宏调用以及它应当展开成的代码，反之亦然。
-   编写从示例调用的参数中生成手写展开式的代码。
-   确保宏抽象不产生“泄露”。

## 示例宏：do-primes<a id="sec-1-2"></a>

编写一个迭代在相继的素数上的宏

首先写两个工具函数：

-   用来测试给定的数是否为素数
-   用来返回大于或等于其实参的下一个素数

```common-lisp
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
```

至少需要一个宏调用示例以及它应当展开成的代码。

```common-lisp
(do-primes (p 0 19)
  (format t "~d " p))
```

按照已有的宏的模式操作的宏比那些引入了无所谓的新颖语法的宏更易于理解和使用。

```common-lisp
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))
```

## 宏形参<a id="sec-1-3"></a>

由于传递给宏的实参是代表宏调用源代码的Lisp对象，因此任何宏的第一步工作都是提取出那些对象中用于计算展开式的部分。

可以用两个形参来定义 `do-primes` ，一个用来保存该列表，另一个 `&rest` 形参保存形式体。

```common-lisp
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))
```

解构形参列表形式，可以自动检查错误。

```common-lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
```

## 生成展开式<a id="sec-1-4"></a>

`` ` `` 反引用表达式与引用表达式很相似，除了可以解引用特定的值的表达式，即前面加上 `,` ，还可以加上 `@,` 表示表达式的值(必须是一个列表)可以拼接到所在的列表中。

列表构造代码

```common-lisp
(defmacro do-primes-a ((var start end) &body body)
  (append '(do)
          (list (list (list var
                            (list 'next-prime start)
                            (list 'next-prime (list '1+ var)))))
          (list (list (list '> var end)))
          body))
```

`MACROEXPAND-1` 是一个函数，接受任何Lisp表达式作为参数并返回做宏展开一层的结果。

```common-lisp
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))) ((> P 19)) (FORMAT T "~d " P))
```

## 堵住漏洞<a id="sec-1-5"></a>

宏可能以三种方式泄露其内部工作细节

过多地对end子形式求值。

```common-lisp
(macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " p)))
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
    ((> P (RANDOM 100)))
  (FORMAT T "~d " P))
```

过多地对end子形式求值，修复代码如下

```common-lisp
(defmacro do-primes ((var start end)) &body body
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))
```

函数 `GENSYM` 在其每次被调用时返回唯一的符号， `ending-value` 只属于宏内部的细节，修复代码如下

```common-lisp
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (ending-value ,end))
         ((> ,var ,ending-value))
       ,@body)))
```

修复后，宏展开式如下

```common-lisp
(let ((ending-value 0))
  (macroexpand-1 '((do-primes (p 0 10)
                     (incf ending-value p))))
  ending-value)
```

使用宏的规则

-   除非有特殊理由，否则需要将展开式中的任何子形式放在一个位置上，使其求值顺序与宏调用的子形式相同。
-   除非有特殊理由，否则需要确保子形式仅被求值一次，方法是在展开式中创建变量来持有求值参数形式所得到的值，然后在展开式中所有需要用到该值的地方使用这个变量。
-   在宏展开期使用 `GENSYM` 来创建展开式中用到的变量名。

## 用于编写宏的宏<a id="sec-1-6"></a>

`with-gensyms` 宏需要达到的目标

```common-lisp
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (end-value start-value)
    `(do* ((,start-value ,start)
           (,var (next-prime ,start-value) (next-prime (1+ ,var)))
           (,end-value ,end))
         ((> ,var ,end-value))
       ,@body)))
```

`with-gensyms` 宏

```common-lisp
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
```

`once-only` 宏，用来生成以特定顺序仅求值特定宏参数一次的代码。

```common-lisp
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))
```
