- [第9章-实践：建立单元测试框架](#sec-1)
  - [两个最初的尝试](#sec-1-1)
  - [重构](#sec-1-2)
  - [修复返回值](#sec-1-3)
  - [更好的结果输出](#sec-1-4)
  - [抽象诞生](#sec-1-5)
  - [测试层次体系](#sec-1-6)
  - [总结](#sec-1-7)

# 第9章-实践：建立单元测试框架<a id="sec-1"></a>

每个测试用例最终归结为一个布尔表达式，要么是真要么是假。

## 两个最初的尝试<a id="sec-1-1"></a>

对所有的测试用例都予以求值并用 `AND` 将结果连在一起

```common-lisp
(defun test-+ ()
  (and
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))
```

找出每个测试用例的运行情况， `FORMAT` 指令中的 `~:[FAIL~;PASS~]` 部分将导致 `FORMAT` 第一个格式实参为假时打印出 `FAIL` ，其他情况下为 `pass` 。

```common-lisp
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

## 重构<a id="sec-1-2"></a>

消除重复的 `FORMAT` 相似调用的最简单方法就是创建一个新函数。

```common-lisp
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))
```

使用 `report-result` 来代替 `FORMAT` 编写 `test-+`

```common-lisp
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

摆脱测试用例表达式的重复以及由此带来的错误标记结果的风险。真正想要的应该是可以将表达式同时看做代码(为了获取结果)和数据(用来作为标签)。意味着这里需要一个宏。

```common-lisp
(defmacro check (form)
  `(report-result ,form ',form))

(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))
```

消除 `check` 的重复调用，定义 `check` 来接受任意数量形式并将它们中的每个都封装在一个对 `report-result` 的调用里。

```common-lisp
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

;; 等价于
(PROGN
  (REPORT-RESULT (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (REPORT-RESULT (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (REPORT-RESULT (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

## 修复返回值<a id="sec-1-3"></a>

修改 `report-result` ，使其在报告时顺便返回测试用例结果。

```common-lisp
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)
```

`AND` 存在短路行为，一旦某个测试用例失败了就会跳过其余的测试。使用 `PROGN` 来代替。

所需的宏 `combine-results`

```common-lisp
(combine-results
 (foo)
 (bar)
 (baz))


(let ((result t))
  (unless (foo) (setf result nil))
  (unless (bar) (setf result nil))
  (unless (baz) (setf result nil))
  result)
```

`combine-results` 宏的定义

```common-lisp
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))
```

用 `combine-results` 代替 `PROGN` 来修复 `check`

```common-lisp
(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))
```

## 更好的结果输出<a id="sec-1-4"></a>

如果编写了大量测试，可能就要以某种方式将它们组织起来，而不是将它们全塞进一个函数里面。

```common-lisp
;; 对*函数添加一些测试用例
(defun test-*()
  (combine-results
    (check
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

;; 两个测试函数，用另一个函数来运行所有测试
(defun test-arithmetic()
  (combine-results
    (test-+)
    (test-*)))
```

将测试结果可以显示每个测试用例来自什么函数。将当前所在测试函数的信息传递给 `report-result`

-   设计动态变量解决该问题。创建一个动态变量使得每个测试函数在调用 `check` 之前将其函数名绑定于其上， `report-result` 就可以无需理会 `check` 来使用它了。

```common-lisp
;; 第一步，在最上层声明这个变量
(defvar *test-name* nil)

;; 第二步，修改report-result，使其在FORMAT输出中包含*test-name*
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)


(defun test-+()
  (let ((*test-name* 'test-+))
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-*()
  (let ((*test-name* 'test-*))
    (check
      (= (* 2 2) 4)
      (= (+ 3 5) 15))))
```

## 抽象诞生<a id="sec-1-5"></a>

为了得到一个完整的抽象，你需要用一种方法来表达“这是一个测试函数”，并且这种方法要能将所有模式所需的全部代码都生成出来。

由于试图捕捉的模式是一个 `DEFUN` 加上一些样板代码，所以需要写一个宏使其展开成 `DEFUN` 。 宏 `deftest` 定义如下

```common-lisp
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))


;; 使用deftest宏重写test-+
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))



;; 使用deftest宏重写test-*
(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))
```

## 测试层次体系<a id="sec-1-6"></a>

`test-arithmetic` 测试套件函数在 `test-+` 上层，应该以层面的组织方式它。对 `*test-name*` 作一个小改变，就可以用测试用例的“全称”路径来报告结果了。

```common-lisp
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (test-arithmetic))
```

## 总结<a id="sec-1-7"></a>

测试框架代码如下

```common-lisp
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))


(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the result of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
```
