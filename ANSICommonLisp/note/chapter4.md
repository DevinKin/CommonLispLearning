<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. 第四章-特殊的数据结构</a></li>
<li><a href="#sec-2">2. 数组</a></li>
<li><a href="#sec-3">3. 二分搜索</a></li>
<li><a href="#sec-4">4. 字符和字符串</a></li>
<li><a href="#sec-5">5. 序列</a></li>
<li><a href="#sec-6">6. 示例：解析日期</a></li>
<li><a href="#sec-7">7. 结构</a></li>
<li><a href="#sec-8">8. 二叉搜索树</a></li>
<li><a href="#sec-9">9. 哈希表</a></li>
</ul>
</div>
</div>

# 第四章-特殊的数据结构<a id="sec-1" name="sec-1"></a>

# 数组<a id="sec-2" name="sec-2"></a>

`make-array` 函数可以用来构造一个数组，第一个实参为指定数组维度的列表。

    (setf arr (make-array '(2 3) :initial-element nil))

在Common Lisp中，数组至少可以达到七个维度，每个维度最大容纳1024元素。

`:initial-element` 实参是可选的，如果提供这个实参，整个数组会用这个值作为初始值。如果取出数组中未定义的元素，其结果是未定义的。

`aref` 取出数组内的元素，该函数是0索引的。参数是对应数组维度下的索引。下面的例子是取出 `arr[0][0]` 处的元素。

    (aref arr 0 0)

替换数组的某个元素，可以使用 `setf` 和 `aref`

    (setf (aref arr 0 0) 'b)

要表示字面值常量的数组，使用 `#na` 语法，其中 `n` 是数组的维度。

    #2a((b nil nil) (nil nil nil))

如果全局变量 `*print-array*` 为真，数组会以字面值常量显示。

    (setf *print-array* t)
    arr
    #2A((B C NIL) (NIL NIL NIL))

一维数组也叫向量(vector)， `vector` 函数可以用来构造和填满向量，向量的元素可以是任意类型的。

    (setf vec (make-array 4 :initial-element nil))
    
    CL-USER> (vector "a" 'b 3)
    #("a" B 3)

`svref` 函数用于获取向量对应索引的元素。 `sv` 代表 simple vector 的意思。

    (svref vec 0)

# 二分搜索<a id="sec-3" name="sec-3"></a>

    (defun bin-search(obj vec)
      (let ((len (length vec)))
        (and (not (zerop len))
             (finder obj vec 0 (- len 1)))))
    
    (defun finder (obj vec start end)
      (let ((range (- end start)))
        (if (zerop range)
            (if (eql obj (aref vec start))
                obj
                nil)
            (let ((mid (+ start (round (/ range 2)))))
              (let ((obj2 (aref vec mid)))
                (if (< obj obj2)
                    (finder obj vec start (- mid 1))
                    (if (> obj obj2)
                        (finder obj vec (+ mid 1) end)
                        obj)))))))

# 字符和字符串<a id="sec-4" name="sec-4"></a>

字符串是字符的向量，用双引号括起来，来表示一个字符串常量，字符 `c` 可以用 `#\c` 表示。

每个字符都有一个相关的整数，通常是ASCII码。在多数Lisp实现中，函数 `char-code` 返回于字符相关的数字， 函数 `code=char` 返回于数字相关的字符。

字符比较函数 `char<(小于)` ， `char<=(小于等于)` ， `char=(等于)` ， `char>=(大于等于)` ， `char>(大于)` ， `char/=(不等于)` 使用字符相关的数字进行比较。

    (sort "elbow" #'char<)

字符串是字符的向量， `aref` 可以取出对应的字符。

    CL-USER> (aref "abc" 1)
    #\b

可以使用 `char` 函数获取字符串下的某个索引对应的字符

    CL-USER> (char "abc" 1)
    #\b

字符串比较函数 `string-equal` 

    CL-USER> (string-equal "fred" "Fred")
    T

`concatenate` 函数接受一个特定类型的符号，加上一个或多个序列

    CL-USER> (concatenate 'string "not " "to worry")
    "not to worry"

# 序列<a id="sec-5" name="sec-5"></a>

在Common Lisp中，序列类型包含了列表和向量。

`elt` 函数取出序列中对应索引的元素。

    CL-USER> (elt '(a b c) 1)
    B

针对向量的 `mirror?` 函数

    (defun mirror? (s)
      (let ((len (length s)))
        (and (evenp len)
             (do ((forward 0 (+ forward 1))
                  (back (- len 1) (- back 1)))
                 ((or (> forward back)
                      (not (eql (elt s forward)
                                (elt s back))))
                  (> forward back))))))

上面版本的函数可以用在列表，频繁对列表使用 `elt` 的代价上昂贵的，因为列表仅允许顺序存取，而向量允许随机存取。

许多序列函数接受一个或多个标准关键字，关键字如下

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">参数</th>
<th scope="col" class="left">用途</th>
<th scope="col" class="left">缺省值</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">:key</td>
<td class="left">应用至每个元素的函数</td>
<td class="left">identity</td>
</tr>


<tr>
<td class="left">:test</td>
<td class="left">用作比较函数</td>
<td class="left">eql</td>
</tr>


<tr>
<td class="left">:from-end</td>
<td class="left">若为真，反向工作</td>
<td class="left">nil</td>
</tr>


<tr>
<td class="left">:start</td>
<td class="left">起始位置</td>
<td class="left">0</td>
</tr>


<tr>
<td class="left">:end</td>
<td class="left">若有给定，结束位置。</td>
<td class="left">nil</td>
</tr>
</tbody>
</table>

`position` 函数接受所有关键字，返回序列中一个元素的位置，没找到则返回nil。

    CL-USER> (position #\a "fantasia")
    1
    CL-USER> (position #\a "fantasia" :start 3 :end 8)
    4
    CL-USER> (position 'a '((c d) (a b)) :key'car)
    1
    ; 第一个比3大的元素所在的位置
    CL-USER> (position 3 '(1 0 7 5) :test #'<)
    2

`position-if` 函数找到满足谓词的元素，接受除了 `:test` 关键字函数。

    CL-USER> (position-if #'oddp '(2 3 4 5))
    1

`remove-duplicates` 函数仅保留序列中每个元素最后一次出现组成的值。

    CL-USER> (remove-duplicates "abracadabra")
    "cdbra"

函数 `reduce` 用来把序列压缩成一个值，接受两个参数，一个函数一个序列。函数必须是接受两个实参的函数。
在最简单的情况下，一开始函数用序列前两个函数作为实参调用，后续的元素作为下次调用的第二个实参，上一次返回的值作为下一次调用的第一个实参。

    ; intersection-交集
    CL-USER> (reduce #'intersection '((b r a d 's) (b a d) (c a t)))
    (A)

# 示例：解析日期<a id="sec-6" name="sec-6"></a>

需求：接受 "16 Aug 1980" 的字符串，然后返回一个表示日、月、年的整数列表

`graphic-char-p` 函数测试字符是否为图形字符，图形字符是可见的字符，加上空白字符。

解析日期代码

    (defun tokens(str test start)
      (let ((p1 (position-if test str :start start)))
        (if p1
            (let ((p2 (position-if #'(lambda (c)
                                       (not (funcall test c)))
                                   str :start p1)))
              (cons (subseq str p1 p2)
                    (if p2
                        (tokens str test p2)
                        nil)))
            nil)))
    
    (defun constituent (c)
      (and (graphic-char-p c)
           (not (char= c #\ ))))
    
    
    (defun parse-date (str)
      (let ((toks (tokens str #'constituent 0)))
        (list (parse-integer (first toks))
              (parse-month (second toks))
              (parse-integer (third toks)))))
    
    (defconstant month-names
      #("jan" "feb" "mar" "apr" "may" "jun"
        "jul" "aug" "sep" "oct" "nov" "dec"))
    
    (defun parse-month (str)
      (let ((p (position str month-names
                         :test #'string-equal)))
        (if p
            (+ p 1)
            nil)))
    
    (defun read-integer (str)
      (if (every #'digit-char-p str)
          (let ((accum 0))
            (dotimes (pos (length str))
              (setf accum (+ (* accum 10)
                             (digit-char-p (char str pos)))))
            accum)
          nil))

# 结构<a id="sec-7" name="sec-7"></a>

使用 `defstruct` 定义结构。

    (defstruct point
      x
      y)

这里定义了一个 `point` 结构，具有两个字段 `x` 与 `y` 。同时隐式地定义了 `make-point` ， `point-p` ， `copy-point` ， `point-x` 及 `point-y` 函数。

当调用 `defstruct` 时，它自动生成了其他几个函数的定义，有了宏，可以自己定义 `defstruct` 。

定义结构也定义了以结构为名的类型。每个点的类型层级会是，类型 `point` ，接着是类型 `structure` ，再来是类型 `atom` ，最后是 `t` 类型。

    CL-USER> (make-polemic)
    What kind of polemic was it? scarthing
    
    #S(POLEMIC :TYPE SCARTHING :EFFECT NIL)

结构的显示方式可以控制，以及结构自动产生的存取函数是字首。

    (defstruct (point (:conc-name p)
                      (:print-function print-point))
      (x 0)
      (y 0))
    
    (defun print-point (p stream depth)
      (format stream "#<~A, ~A>" (px p) (py p)))
    
    CL-USER> (make-point :x 3 :y 4)
    #<3, 4>

# 二叉搜索树<a id="sec-8" name="sec-8"></a>

实现代码

    (defun bst-remove (obj bst <)
      (if (null bst)
          nil
          (let ((elt (node-elt bst)))
            (if (eql obj elt)
                (percolate bst)
                (if (funcall < obj elt)
                    (make-node
                     :elt elt
                     :l (bst-remove obj (node-l bst) <)
                     :r (node-r bst))
                    (make-node
                     :elt elt
                     :r (bst-remove obj (node-r bst ) <)
                     :l (node-l bst)))))))
    
    (defun percolate (bst)
      (cond ((null (node-l bst))
             (if (null (node-r bst))
                 nil
                 (rperc bst)))
            ((null (node-r bst)) (lperc bst))
            (t (if (zerop (random 2))
                   (lperc bst)
                   (rperc bst)))))
    
    (defun rperc (bst)
      (make-node :elt (node-elt (node-r bst))
                 :l (node-l bst)
                 :r (percolate (node-r bst))))
    
    (defun lperc (bst)
      (make-node :elt (node-elt (node-l bst))
                 :l (percolate (node-l bst))
                 :r (node-r bst)))
    
    ;; 遍历：二叉搜索树
    (defun bst-traverse (fn bst)
      (when bst
        (bst-traverse fn (node-l bst))
        (funcall fn (node-elt bst))
        (bst-traverse fn (node-r bst))))

# 哈希表<a id="sec-9" name="sec-9"></a>

`make-hash-table` 用来构造一个哈希表，它不需要传入参数。

    (setf ht (make-hash-table))

`gethash` 函数传入一个键值于哈希表，获取的键对应的值。没有则返回 `nil`

    (gethash 'color ht)

`gethash` 函数返回两个数值，第一个值是与键值有关的数值，第二个值说明哈希表是否包含任何用此键值来存储的数值。

要设置哈希表的键值对，可以使用如下方式

    (setf (gethash 'color ht) 'reduce)
    CL-USER> (gethash 'color ht)
    REDUCE
    T

哈希表的对象或键值可以是任意类型的。

    (setf bugs (make-hash-table))
    (push "Doesn't take keyword arguments." (gethash #'member bugs))

一个新创建的哈希表，会很方便地是一个空集合。

`remhash` 函数从一个哈希表中移除一个词条。

    (remhash 'apricot fruit)

`maphash` 函数是哈希表迭代函数，它接受两个参数，接受两个参数的函数以及哈希表。该函数会被每个键值对调用，没有特定的顺序。

    (setf (gethash 'shape ht) 'spherical
          (gethash 'size ht) 'giant)
    (maphash #'(lambda (k v)
                 (format t "~A = ~A ~%" k v))
             ht)

`maphash` 总是返回 `nil` ，但可以传入一个会累积数值的函数，把哈希表的词条存在列表里面。

哈希表可以容纳任何数量的元素，但当哈希表空间用完时，它们会被扩张。

自定义哈希表的元素空间大小，可以使用 `:size` 关键字参数。

    (make-hash-table :size 5)

哈希表有某种比较键值的概念。预设是使用 `eql` ，可以使用关键字参数 `:test` 来告诉
哈希表使用 `eq` ， `equal` ，还是 `equalp` 。

    (setf writers (make-hash-table :test #'equal))
