<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. 第三章-列表</a></li>
<li><a href="#sec-2">2. 构造</a></li>
<li><a href="#sec-3">3. 等式</a></li>
<li><a href="#sec-4">4. 为什么Lisp没有指针</a></li>
<li><a href="#sec-5">5. 建立列表</a></li>
<li><a href="#sec-6">6. 示例：压缩</a></li>
<li><a href="#sec-7">7. 存取</a></li>
<li><a href="#sec-8">8. 映射函数</a></li>
<li><a href="#sec-9">9. 树</a></li>
<li><a href="#sec-10">10. 理解递归</a></li>
<li><a href="#sec-11">11. 集合</a></li>
<li><a href="#sec-12">12. 序列</a></li>
<li><a href="#sec-13">13. 栈</a></li>
<li><a href="#sec-14">14. 点状列表</a></li>
<li><a href="#sec-15">15. 关联列表</a></li>
<li><a href="#sec-16">16. 最短路径</a></li>
<li><a href="#sec-17">17. 垃圾</a></li>
</ul>
</div>
</div>

# 第三章-列表<a id="sec-1" name="sec-1"></a>

# 构造<a id="sec-2" name="sec-2"></a>

`cons` 函数把两个对象结合成一个有两部分的对象，称之为 `Cons` 对象。

一个 `Cons` 对象是一对指针，第一个是 `car` ，第二个是 `cdr` 。

一个 `Cons` 对象里的一对指针，可以指向任何类型的对象，包括 `Cons` 对象本身。

当我们构造一个多元素的列表时，我们得到一串 `Cons` 。

函数 `consp` 的参数如果是一个 `Cons` 对象，返回真。

因为所有不是 `Cons` 对象的东西，就是一个原子，判断式 `atom` 可以这样定义。

    (defun our-atom(x)
      (not (consp x)))

`nil` 既是一个原子，也是一个列表。

# 等式<a id="sec-3" name="sec-3"></a>

每一次调用 `cons` ，LIsp会配置一块新的内存给两个指针。用同样的参数调用 `cons` ，实际是两个不同的对象。

    (eql (cons 'a nil) (cons 'a nil))
    ;result: nil

`equal` 函数用于判断两个值是否相等，不需要判断是否为同一对象。

    (equal x (cons 'a nil))

# 为什么Lisp没有指针<a id="sec-4" name="sec-4"></a>

Lisp没有指针的原因是因为每一个值，其概念上来说都是一个指针。

为了效率，Lisp有时会选择使用整数，而不是指针，具体看整数所需的内存空间。

# 建立列表<a id="sec-5" name="sec-5"></a>

函数 `copy-list` 接受一个列表，然后返回此列表的副本。

    (defun our-copy-list(lst)
      (if (atom lst)
          lst
          (cons (car lst) (our-copy-list (cdr lst)))))

函数 `append` 返回任何数目的列表串接。

    (append '(a b) '(c d) 'e)

# 示例：压缩<a id="sec-6" name="sec-6"></a>

游程编码

压缩

    (defun compress (x)
      (if (consp x)
          (compr (car x) 1 (cdr x))
          x))
    
    (defun compr (elt n lst)
      (if (null lst)
          (list (n-elts elt n))
          (let ((next (car lst)))
            (if (eql next elt)
                (compr elt (+ n 1) (cdr lst))
                (cons (n-elts elt n)
                      (compr next 1 (cdr lst)))))))
    
    (defun n-elts (elt n)
      (if (> n 1)
          (list n elt)
          elt))

解压

    (defun uncompress(lst)
      (if (null lst)
          nil
          (let ((elt (car lst))
                (rest (uncompress (cdr lst))))
            (if (consp elt)
                (append (apply #'list-of elt) rest) 
                (cons elt rest)))))
    
    (defun list-of (n elt)
      (if (zerop n)
          nil
          (cons elt (list-of (- n 1) elt))))

# 存取<a id="sec-7" name="sec-7"></a>

`nth` 函数可以找到列表特定位置的元素。

    (nth 0 '(a b c))
    ;result: A

`nthcdr` 函数可以找到第n个cdr。

    (nthcdr 2 '(a b c))
    ;result: (C)

`nth` 和 `nthcdr` 都是零索引的。

函数 `last` 返回列表的最后一个 `Cons` 对象。

    (last '(a b c))
    ;result:(C)

Common Lisp定义了像是 `caddr` 这样的函数，它是 `cdr` 的 `cdr` 的 `car` 的缩写。

这样形式的函数 `cxr` ，其中 `x` 是一个字符串，最多四个 `a` 或 =d=。

# 映射函数<a id="sec-8" name="sec-8"></a>

`mapcar` 函数接受一个函数以及一个或多个列表，并把函数应用到每一个列表的元素的结果，直到列表没有元素为止。

    (mapcar #'(lambda (x) (+ x 10))
            '(1 2 3))
    ;result:(11 12 13)

`maplist` 函数接受一个函数以及一个或多个列表，将列表的渐进的下一个 `cdr` 传入函数。

    (maplist #'(lambda (x) x)
             '(a b c))
    ;result: ((A B C) (B C) (C))

# 树<a id="sec-9" name="sec-9"></a>

`Cons` 对象可以理解为一颗二叉树， `car` 代表左树， `cdr` 代表右树。

`copy-tree` 接受一棵树，并返回一个副本。可以用如下定义

    (defun our-copy-tree (tr)
      (if (atom tr)
          tr
          (cons (our-copy-tree (car tr))
                (our-copy-tree (cdr tr)))))

把各处的 `x` 都替换成 `y` ，调用 `substitue` 是不行的，它只能替换序列中的元素。

`substitute` 用于替换列表中的原子， `subst` 可以用于替换列表中的列表的某些元素，也可以替换原子。

    (substitute 'y 'x '(and (integerp x) (zerop (mod x 2)) x))
    ;;result: (AND (INTEGERP X) (ZEROP (MOD X 2)) Y)

`subst` 用于替换树中的元素。

    (subst 'y 'x '(and (integerp x) (zerop (mod x 2)) x))
    ;;result: (AND (INTEGERP Y) (ZEROP (MOD Y 2)) Y)

`subst` 的定义可以如下

    (defun our-subst (old new tree)
      (if (eql tree old)
          new
          (if (atom tree)
              tree
              (cons (our-subst old new (car tree))
                    (our-subst old new (cdr tree))))))

操作树的函数通常有这种形式， `car` 和 `cdr` 同时做递归，这种函数被称之为双重递归。

# 理解递归<a id="sec-10" name="sec-10"></a>

对于初始条件，递归函数是成立的。假设第n次调用成功返回，第n+1次调用也成功返回，说明递归是有效的。

当一个递归函数不是你想象的那样工作时，通常就是处理基本用例错了。

造成无限循环的递归案例

    (defun our-member (obj lst)
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst))))

# 集合<a id="sec-11" name="sec-11"></a>

`member` 函数返回列表中寻找到目标对象开始的那部分集合。

    (member 'b '(a b c))
    ;result: (B C)

`member` 使用 `eql` 来比较对象，可以使用一种叫做关键字参数的东西来重写缺省的比较方法。

大多数的Common Lisp函数接受一个或多个关键字参数，这些关键字参数不同的地方是，它们不是把对应的参数放在特定的为止作匹配，而是在函数调用中用特殊的标签，称为关键字来作匹配。

一个关键字是一个前面有冒号的符号。

`member` 函数接受的关键字参数是 `:test` 参数。

    (member '(a) '((a) (z)) :test #'equal)

关键字参数总是选择性添加的。如果在一个调用中包含了任何的关键字参数，它们要摆在最后，如果使用了超过一个的关键字参数，摆放顺序无关紧要。

`member` 函数的另一个关键字参数是 `:key` 参数，可以在作比较之前，指定一个函数运用在每一个元素。

    (member 'c '((a b) (c d)) :key #'car)
    
    ((C D))

如果我们想要找到一个元素满足任意判断式像是 `oddp` ，奇数返回真，我们可以使用相关的 =member-if=。

    (member-if #'oddp '(2 3 4))
    ;result: (3 4)

限制版的 `member-if` 实现如下

    (defun our-member-if (fn lst)
      (and (consp lst)
           (if (funcall fn (car lst))
               lst
               (our-member-if fn (cdr lst)))))

函数 `adjoin` 接受一个对象及一个列表，如果对象不是列表的成员，才构造对象到列表上。

    (adjoin 'b '(a b c))
    ;result: (A B C)
    (adjoin 'z '(a b c))
    ;result: (Z A B C)

集合论中的并集、交集和补集，由函数 `union` ， `intersection` ， `set-difference` 实现的。

    CL-USER> (union '(a b c) '(c b s))
    (A C B S)
    CL-USER> (intersection '(a b c) '(b b c))
    (C B)
    CL-USER> (set-difference '(a b c d e) '(b e))
    (D C A)

集合中没有顺序的概念，所以返回的顺序有可能不同。

# 序列<a id="sec-12" name="sec-12"></a>

序列包括了列表与向量。

使用 `subseq` 复制序列的一部分。第一个参数是列表，第二个和第三个参数（可选）是开始位置和结束位置。

    CL-USER> (subseq '(a b c d) 1 2)
    (B)
    CL-USER> (subseq '(a b c d) 1)
    (B C D)

函数 `reverse` 返回将参数中元素顺序颠倒的序列。

    CL-USER> (reverse '(a b c))
    (C B A)

函数 `sort` 接受一个序列以及一个比较两个参数的函数，返回一个有同样元素的序列。

    CL-USER> (sort '(0 2 1 3 8) #'>)
    (8 3 2 1 0)

由于效率原因， `sort` 函数被允许修改传入的序列。

函数 `every` 和 `some` 接受一个判断式及一个或多个序列。当输入一个序列时，它们测试序列元素是否满足判断式。

    CL-USER> (every #'oddp '(1 3 5))
    T
    CL-USER> (some #'evenp '(1 2 3))
    T

# 栈<a id="sec-13" name="sec-13"></a>

`push` 的定义参考如下

    (setf lst (cons obj lst)))

`pop` 的定义参考如下

    (let ((x (car lst)))
      (setf lst (cdr lst))
      x)

`pushnew` 宏是 `push` 的变种，它使用了 `ajoin` 而不是 `cons` 。

    CL-USER> (let ((x '(a b))) (pushnew 'c x) (pushnew 'a x) x)
    (C A B)

# 点状列表<a id="sec-14" name="sec-14"></a>

调用 `list` 所构造的列表，这种列表精确的说称为正规列表。

一个正规列表可以是 `nil` 或者是 `cdr` 是正规列表的 `Cons` 对象。

`cons` 不仅是构造列表，还可以构造一个 `pair` 。 `car` 指向第一个字段， `cdr` 指向第二个字段。

    (setf pair (cons 'a 'b))
    (A . B)

在点状表示法中，每个 `Cons` 对象的 `car` 与 `cdr` 由一个句点隔开来表示。

一个非正规列表的 `Cons` 对象称之为点状列表。非正规列表的 `Cons` 对象通常不是用来表示列表， `(a . b)` 只是一个有两部分的数据结构。

可以用点状表示法表示正规列表。

    CL-USER> '(a . (b . (c . nil)))
    (A B C)

# 关联列表<a id="sec-15" name="sec-15"></a>

一个由 `Cons` 对象组成的列表称之为关联列表，可以理解为map。

    CL-USER> (setf trans '((+ . "add") (- . "subtract")))
    ((+ . "add") (- . "subtract"))

`assoc` 函数用来取出在关联列表中，与给定的键值有关联的 `Cons` 对。

    CL-USER> (assoc '+ trans)
    (+ . "add")
    CL-USER> (assoc '* trans)
    NIL

受限版的 `assoc` 定义如下

    (defun our-assoc (key alist)
      (and (consp alist)
           (let ((pair (car alist)))
             (if (eql key (car pair))
                 pair
                 (our-assoc key (cdr alist))))))

# 最短路径<a id="sec-16" name="sec-16"></a>

函数 `shortest-path` 接受一个起始节点，目的节点以及一个网络，并返回最短路径。

示例中，节点用符号表示，而网络用含一下元素形式的关联列表来表示。 `(node . neighbors)`

最小网络可以如下表示

    (setf min '((a b c) (b c) (c d)))

广度优先搜索算法实现

    (defun shortest-path (start end net)
      (bfs end (list (list start)) net))
    
    (defun bfs (end queue net)
      (if (null queue)
          nil
          (let ((path (car queue)))
            (let ((node (car path)))
              (if (eql node end)
                  (reverse path)
                  (bfs end
                       (append (cdr queue)
                               (new-paths path node net))
                       net))))))
    
    (defun new-paths (path node net)
      (mapcar #'(lambda (n)
                  (cons n path))
              (cdr (assoc node net))))

连续调用 `bfs` 函数，传入路径队列参数顺序如下

    ((A))
    ((B A) (C A))
    ((C A) (C B A))
    ((C B A) (D C A))
    ((D C A) (D C B A))

# 垃圾<a id="sec-17" name="sec-17"></a>

Lisp 系统维护着一段内存称为堆，系统持续追踪堆当中没有使用的内存，把这些内存发放给新的对象。

Lisp 具有垃圾回收机制，会周期性通过搜索堆，回收内存。
