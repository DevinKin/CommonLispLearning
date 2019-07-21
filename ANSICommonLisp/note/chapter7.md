- [第7章-输入和输出](#sec-1)
  - [流](#sec-1-1)
  - [输入（Input）](#sec-1-2)
  - [输出（Output）](#sec-1-3)
  - [示例：字符串替换](#sec-1-4)
  - [宏字符](#sec-1-5)

# 第7章-输入和输出<a id="sec-1"></a>

## 流<a id="sec-1-1"></a>

流是用来表示字符来源或终点的Lisp对象。

输入缺省是从 `*standard-input*` 流读取(read)。输出缺省是在 `*standard-output*` 流读取(format)。

路径名（pathname）是一种指定一个文件的可移植方式。路径名包含了六个部分。

-   host
-   device
-   directory
-   name
-   type
-   version

`make-pathname` 函数可以产生一个路径

```common-lisp
(setf path (make-pathname :name "myfile"))
```

`open` 函数可以打开一个文件，它接受一个路径名以及大量的选择性关键字参数，如果打开成功，返回一个指向文件的流。

-   `direction` 参数可以设置写入流、从流读取或者同时进行读写操作。对应数值如下
    -   `:input`
    -   `:output`
    -   `:io`
-   `if-exists` 参数说明了如果文件存在时该怎么做。通常它应该是 `:supersede` 取代。

```common-lisp
(setf str (open path :direction :output
                     :if-exists :supersede))
;; 将字符串输入到myfile流，即文件中
(format str "Something~%")
```

`close` 函数可以用于关闭流

```common-lisp
(close str)
```

读取文件实例

```common-lisp
(setf str (open path :direction :input))

(read-line str)

(close str)
```

`with-open-file` 宏第一个参数应该是列表，包含了变量名(打开文件返回流)、伴随着你想传给 `open` 的参数。后面的参数是代码主体，它会被绑定至流的变量一起被求值，其中流是通过剩余的参数传递给 `open` 来创建的。之后这个流会自动被关闭。文件写入动作如下

```common-lisp
(with-open-file (str path :direction :output
                          :if-exists :supersede)
  (format str "Something~%"))
```

## 输入（Input）<a id="sec-1-2"></a>

`read-line` 函数读入换行之前的所有字符，并用字符串返回它们。它接受一个选择性流参数，默认值为 `*standard-input*`

```common-lisp
(progn
  (format t "Please enter your name: ")
  (read-line))
```

`read-line` 接受四个选择性参数

-   流。
-   决定遇到 `end-of-file` 时，是否产生错误。
-   前一个参数为 `nil` 该返回什么。
-   第四个参数通常可以省略

显示一个文件的每一行内容

```common-lisp
(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))
```

`read` 可以把输入解析为 `Lisp` 对象。

`read` 会在第一个表达式之后，停止处理字符，留下剩下的字符给之后读取这个流的函数处理。

`read-from-string` 函数接受一个字符串，并返回第一个读取的表达式。返回第二个值指出停止读取字符串时的位置的数字。

```common-lisp
(read-from-string "a b c")
```

`read-from-string` 接受两个选择性参数(end-of-filep和end-of-file-value)和三个关键字(start、end)参数。

## 输出（Output）<a id="sec-1-3"></a>

`prin1` 、 `princ` 、 `terpri` 三个函数最后一个参数都是选择性流参数，默认值是 `*standard-output*`

`prin1` 输出带双引号， `princ` 不带。

```common-lisp
(prin1 "Hello")
(princ "Hello")
```

`format` 函数接受一个流、一个格式化字符串以及零个或多个额外的参数。 `~S` 格式化指令使用 `prin1` 打印出对象。

```common-lisp
(format t "~S ~A" "z" "z")
"z" z
```

`~F` 格式化指令用来打印向右对齐的浮点数，可接受5个参数

-   要打印字符的总数。缺省值是数字的长度。
-   小数后要打印几位数。缺省值是全部。
-   小数点往后右移几位。缺省值是没有。
-   若数字太长无法满足第一个参数时，要打印字符。如果没有指定的字符，一个过长的数字会尽可能使用它所需的空间被打印。
-   数字开始打印前左边的字符。缺省值是空白。

```common-lisp
(format nil "~10,2,0,'*,' F" 26.21875)
"     26.22"
(format nil "~,2,,,F" 26.21875)
(format nil "~,2F" 26.21875)
"26.22"
```

`format` 取整数时，它不保证会向上进位或向下舍入。

## 示例：字符串替换<a id="sec-1-4"></a>

一个暂时存储输入的队列称作缓冲区。 环状缓冲区实际上是一个向量，是使用的方式使其成为环状。

代码如下

```common-lisp
;; 缓冲区
(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))


(defun buf-pop (b)
  (prog1
      (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new b) (buf-end b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new b) (buf-end b)))

(defun buf-clear (b)
  (setf (buf-start b) -1
        (buf-used b) -1
        (buf-new b) -1
        (buf-end b) -1))


(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

;; 替换方法
(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst old new in out))))

;;替换算法
(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
         (or (setf from-buf (buf-next buf))
             (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
                      (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))
```

## 宏字符<a id="sec-1-5"></a>

宏字符是获得 `read` 特别待遇的字符。 一个宏字符或宏字符组合也称作 `read-macro` 读取宏。许多Common Lisp预定义的读取宏是缩写。

如 `'a` 表达式被读取器展开成 `(quote a)`

可以通过显式调用 `read` 显示被求值的宏字符。

```common-lisp
(car (read-from-string "'a")
QUOTE
```

上面读取宏的方式叫做派发读取宏，第一个字符叫做派发字符。如 `#'` 、 `'`
