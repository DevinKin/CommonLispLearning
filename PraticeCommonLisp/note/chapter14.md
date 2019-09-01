- [第14章-文件和文件I/O](#sec-1)
  - [读取文件数据](#sec-1-1)
  - [读取二进制数据](#sec-1-2)
  - [批量读取](#sec-1-3)
  - [文件输出](#sec-1-4)
  - [关闭文件](#sec-1-5)
  - [文件名](#sec-1-6)
  - [路径名如何表示文件名](#sec-1-7)
  - [构建新路径名](#sec-1-8)
  - [目录名的两种表示方法](#sec-1-9)
  - [其他I/O类型](#sec-1-10)

# 第14章-文件和文件I/O<a id="sec-1"></a>

## 读取文件数据<a id="sec-1-1"></a>

`OPEN` 函数获得一个流并从中读取文件的内容.默认情况下, `OPEN` 返回一个基于字符的输入流. 必要参数是读取的文件名.

```common-lisp
(let ((in (open "/some/file/name.txt")))
  (format t "~a~%" (read-line in))
  (close in))
```

`READ-CHAR` 函数读取单个字符, `READ-LINE` 函数读取一行文本, 去掉行结束字符后作为一个字符串返回.

`READ` 函数读取单一的S-表达式并返回一个Lisp对象.

`CLOSE` 函数关闭打开的流.

如果想打开一个可能不存在的文件, 不想 `OPEN` 函数报错, 可以使用关键字参数 `:if-does-not-exists` 来指定不同的行为, 三个可能的值如下

-   `:error` 报错(默认值)
-   `:create` 继续进行并创建该文件, 然后像它已经存在那样处理.
-   `NIL` 让它返回 `NIL` 来代替一个流

```common-lisp
(let ((in (open "/home/devinkin/name.txt" :if-does-not-exist :create)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))
```

读取函数 `READ-CHAR` , `READ-LINE` 和 `READ` 都接受一个可选的参数, 其默认值为真并指定当函数在文件结尾处调用时是否应该报错. 如果该参数为 `NIL` , 它们在遇到文件结尾时将返回它们第三个参数的值, 默认为 `NIL` .

```common-lisp
(let ((in (open "/home/devinkin/name.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (format t "~a~%" line))
    (close in)))
```

`READ` 是 Lisp独有的, 当每次被调用时, 它会读取单一的S-表达式, 跳过空格和注释, 然后返回由S-表达式表达的Lisp对象.

```common-lisp
;;content of name.txt
#|
(1 2 3)
456
"a string" ; this is a comment
((a b)
 (c d))
|#
(defparameter *s* (open "/home/devinkin/name.txt"))
(read *s*)
;;(1 2 3)
(read *s*)
;;456
(read *s*)
;;"a string"
(read *s*)
;;((A B) (C D))
(close *s*)
;;T
```

## 读取二进制数据<a id="sec-1-2"></a>

为了使用 `OPEN` 函数读取原始字节, 需要传递一个值为 `'(unsigned-byte 8)` 的 `:element-type` 参数.

`READ-BYTE` 函数可以从流中读取单个字节, 返回 `0-255` 的整数. `READ-BYTE` 支持可选的参数以便指定当其被调用在文件结尾时是否应该报错, 以及遇到结尾时返回何值.

## 批量读取<a id="sec-1-3"></a>

`READ-SEQUENCE` 可同时工作在字符和二进制流上. 它接受一个序列(通常是一个向量)和一个流, 然后它会完全尝试来自流的数据填充该序列. 它返回序列中第一个没有被填充的元素的索引, 或是在完全填充的情况下返回该序列的长度.

-   可以传递 `:start` 和 `:end` 关键字参数来指定一个应当被代替填充的子序列. 该序列参数的类型必须足以保存带有该流元素类型的元素.

由于多数操作系统支持某种形式的块I/O, `READ-SEQUENCE` 通常比重复调用 `READ-BYTE` 或 `READ-CHAR` 来填充一个序列更加高效.

## 文件输出<a id="sec-1-4"></a>

可以在 `OPEN` 函数使用关键字参数 `:direction` 指定流的方向, `:output` 值指定为输出流, 可以向文件写入数据.

`:if-exists` 关键字参数可以改变文件存在时报错的行为.

-   `:supersede` 值可以告诉 `OPEN` 函数来替换已有的文件.
-   `:append` 将导致 `OPEN` 打开已有的文件并保证数据被写到文件结尾处.
-   `:overwrite` 返回一个从文件开始处开始的流而覆盖已有数据.
-   `NIL` 将导致OPEN在文件已存在时返回 `NIL` 而不是流.

`OPEN` 打开一个输出流示例.

```common-lisp
(open "/some/file/name.txt" :direction :output :if-exists :supersede)
```

`WRITE-CHAR` 函数会向流写入一个单一的字符, `WRITE-LINE` 函数写入一个字符串并紧跟一个换行.

`WRITE-STRING` 函数写一个字符串而不会添加任何行结束符.

有两个函数可以只打印一个换行

-   `TERPRI` 函数, 终止打印(terminate print), 即无条件地打印一个换行字符.
-   `FRESH-LINE` 打印一个换行字符, 除非该流已经在一行的开始处.

避免由按顺序调用的不同函数所组成的文本输出中额外换行时, `FRESH-LINE` 比较好用.

`PRINT` 打印一个S-表达式, 前缀一个换行及一个空格.

`PRIN1 只打印S-表达式.`

`PPRINT` 使用美化打印器打印S-表达式.

并非所有对象都能以一种 `READ` 可理解的形式打印出来, 当试图使用 `PRINT` , `PRIN1` , 或 `PPRINT` 来打印这样的一种对象, 变量 `*PRINT-READABLY` 会控制.

-   `PRINT-READABLY` 是 `NIL` , `PRINT` 类函数将以导致 `READ` 在试图读取时肯定会报错的特殊语法来打印对象.
-   否则它们直接报错而不打印该对象.

`PRINC` 在打印字符串时不带有引号, 也会打印Lisp对象.

向一个文件写入二进制数据, 在 `OPEN` 打开文件指定 `:element-type` 实参, 并其值为 `'(unsigned-byte 8)` , 就可以使用 `WRITE-BYTE` 写入单独的字节.

`WRITE-SEQUENCE` 批量输出函数, 只要序列汇总的所有元素都是用于该流的适当类型即可.

## 关闭文件<a id="sec-1-5"></a>

如果在 `CLOSE` 之前含有 `RETURN` 或 `RETURN-FROM` , 那就会在没有关闭流的情况下离开 `LET` 语句块.

```common-lisp
(let ((stream (open "/some/file/name.txt")))
  ;; do stuff with stream
  (close stream))
```

特殊操作符 `UNWIND-PROTECT` 可以防止代码被 `throw` 与 `error` 打断.

`WITH-OPEN-FILE` 是构建在 `UNWIND-PROTECT` 上的宏, 可以对打开的文件做关闭流的操作. 基本形式如下

```common-lisp
(with-open-file (stream-var open-argument*)
  body-form*)
```

使用 `WITH-OPEN-FILE` 创建一个新文件, 并写入数据

```common-lisp
(with-open-file (stream "/home/devinkin/name.txt" :direction :output :if-exists :supersede)
  (format stream "Some text."))
```

## 文件名<a id="sec-1-6"></a>

Common Lisp提供了另一种文件名的表示方式: 路径名(pathname)对象.

路径名以一种结构化的方式来表示文件名, 这种方式易于管理而无需捆绑在特定的文件名语法上.

## 路径名如何表示文件名<a id="sec-1-7"></a>

路径名是一种使用6个组件来表示文件名的结构化对象

-   主机(host)
-   设备(device)
-   目录(directory)
-   名称(name)
-   类型(type)
-   版本(version)

上面6个组件都接受原子值, 通常是字符串. 只有目录组件有其进一步的结构, 含有一个目录名(作为字符串)的列表, 其中带有关键字 `:absolute` 或 `:relative` 作为前缀.

`PATHNAME` 函数接受路径名描述符并返回等价的路径名对象.

-   当该描述符是一个路径名时, 它就会被简单地返回.
-   当描述符是一个流时, 最初的文件名就会被抽取出来然后返回.
-   当描述符是一个名字字符串时, 它将根据本地文件名语法来解析.

`PATHNAME-DIRECTORY` , `PATHNAME-NAME` , `PATHNAME-TYPE` 用于检查一个路径名中的单独组件.

```common-lisp
(pathname-directory (pathname "/foo/bar/baz.txt"))
;(:ABSOLUTE "foo" "bar")

(pathname-name (pathname "/foo/bar/baz.txt"))
;"baz"

(pathname-type (pathname "/foo/bar/baz.txt"))
;"txt"
```

`PATHNAME-HOST` , `PATHNAME-DEVICE` 和 `PATHNAME-VERSION` 访问其他三个路径名组件.

路径名有自身的读取语法 `#p` 后接一个爽引号字符串.

```common-lisp
(pathname "/foo/bar/baz.txt")
;#P"/foo/bar/baz.txt"
```

可以使用函数 `NAMESTRING` 将一个路径名转化回一个名字字符串. 它接受一个路径名描述符并返回一个名字字符串.

```common-lisp
(namestring #p"/foo/bar/baz.txt")
```

`DIRECTORY-NAMESTRING` 和 `FILE-NAMESTRING` 分别返回目录组件的元素组合成一个本地目录名, 组合名字和类型组件.

```common-lisp
(directory-namestring #p"/foo/bar/baz.txt")
;"/foo/bar/"
(file-namestring #p"/foo/bar/baz.txt")
;"baz.txt"
```

## 构建新路径名<a id="sec-1-8"></a>

`MAKE-PATHNAME` 函数构造任意路径名, 它对没每个路径名组件都接受一个关键字参数并返回一个路径名, 任何提供了组件都被填入其中而其余的为 `NIL`

```common-lisp
(make-pathname
 :directory '(:absolute "foo" "bar")
 :name "baz"
 :type "txt")
;#P"/foo/bar/baz.txt"
```

基于Windows的Lisp实现将驱动器字母保存在设备组件中, 其他一些实现将它保存在主机组件中.

```common-lisp
(make-pathname :device "c" :directory '(:absolute "foo" "bar") :name "baz")
```

`MAKE-PATHNAME` 的 `:defaults` 关键字参数用一个已有的路径名来构造一个新路径名

```common-lisp
(make-pathname :type "html" :defaults input-file)
```

创建一个带有不同目录组件的路径名

```common-lisp
(make-pathname :directory '(:relative "backups") :defaults input-file)
```

`MERGE-PATHNAMES` 接受两个路径名并合并它们, 用来自第二个路径名的对应值填充地一个路径名中的任何 `NIL` 组件. 如果地一个组件名的目录是相对的, 那么生成的路径名的目录组件将是第一个路径名的目录相对于第二个路径名的目录.

```common-lisp
(merge-pathnames #p"foo/bar.html" #p"/www/html")
;#P"/www/foo/bar.html"
(merge-pathnames #p"foo/bar.html" #p"html/")
;#P"html/foo/bar.html"
```

`ENOUGH-NAMESTRING` 函数可以拆解路径名, 它接受两个路径名, 以便获得一个想对于特定根目录的文件名.

```common-lisp
(enough-namestring #p"/www/html/foo/bar.html" #p"/www/")
;"html/foo/bar.html"
```

创建一个表达相同名字但在不同根目录的路径名.

```common-lisp
(merge-pathnames
 (enough-namestring #p"/www/html/foo/bar/baz.html" #p"/www/")
 #p"/www-backups/")
;#P"/www-backups/html/foo/bar/baz.html"
```

`*DEFAULT-PATHNAME-DEFAULTS*` 可以获取缺失组件的值.

```common-lisp
*DEFAULT-PATHNAME-DEFAULTS*
; #P"/home/devinkin/learning/CommonLispLearning/PraticeCommonLisp/note/"

```

## 目录名的两种表示方法<a id="sec-1-9"></a>

目录名表示方式有两种

-   将目录当成文件形式对待, 将名字字符串中最后一个元素放在名称和类型组件中.
-   目录形式将名字中的所有元素都放在目录组件中, 留下名称和类型组件为 `NIL`

```common-lisp
;; 文件形式
(make-pathname :directory '(:absolute "foo") :name "bar")

;; 目录形式
(make-pathname :directory '(:absolute "foo" "bar"))
```

`PROBE-FILE` 函数可以测试一个对应于某个路径描述符的文件是否存在于文件系统中.

`DELETE-FILE` 接受一个路径名描述符并删除所命名的文件.

`RENAME-FILE` 接受两个路径名描述符, 并将第一个名字命名的文件重命名为第二个名字.

`ENSURE-DIRECTORIES-EXIST` 用来创建目录, 它接受一个路径名描述符并确保目录组件中的所有元素存在并且是目录.

`FILE-WRITE-DATE` 和 `FILE-AUTHOR` 都接受一个路径名描述符, 返回文件上次写入的时间, 返回文件的拥有者.

`FILE-LENGTH` 函数接受一个流, 返回文件的长度.

```common-lisp
(with-open-file (in filename :element-type '(unsigned-byte 8))
  (file-length in))
```

`FILE-POSITION` 接受一个流, 返回文件的当前位置, 即已经被读取或写入该流的元素的数量.

## 其他I/O类型<a id="sec-1-10"></a>

`MAKE-STRING-INPUT-STREAM` 接受一个字符串以及可选的开始和结尾指示符来鉴定字符串中数据应被读取的区域.

```common-lisp
(let ((s (make-string-input-stream "1.23")))
  (unwind-protect (read s)
    (close s)))
```

`MAKE-STRING-OUTPUT-STREAM` 接受一个字符串并返回一个输出流.

`WITH-INPUT-FROM-STRING` 宏从给定字符串中创建字符串输入流, 并绑定在提供的变量的情况下执行它的主体形式.

```common-lisp
(with-input-from-string (s "1.23")
  (read s))

```

`WITH-OUTPUT-TO-STRING` 把新创建的字符串输出流绑定到明明的变量上, 然后执行它的主题, 所有主体形式都被执行以后, `WITH-OUTPUT-TO-STRING` 返回由 `GET-OUTPUT-STREAM-STRING` 返回的值.

```common-lisp
(with-output-to-string (out)
  (format out "hello, world ")
  (format out "~s" (list 1 2 3)))
;"hello, world (1 2 3)"
```

`BROADCAST-STREAM` 是一个输出流, 它将向其写入的任何数据发送到一组输出流上.

`CONCATENATED-STREAM` 是一个输出流, 它从一组输入流中接收其输入, 在每个流的结尾处它从一个流移动到另一个.
