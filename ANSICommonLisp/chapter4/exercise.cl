;;1.定义一个函数，接受一个平方数组（square array，一个相同维度的数组 (n n) )，并将它顺时针转 90 度。
;;后面再做

;;2.阅读 368 页的 reduce 说明，然后用它来定义：
;; 后面再做

;;3.定义一个结构来表示一棵树，其中每个节点包含某些数据及三个小孩。定义：
(defstruct (my-tree (:print-function print-my-tree))
  node
  (a nil)
  (b nil)
  (c nil))

(defun print-my-tree(tree stream depth)
  (format stream "(~A" (my-tree-node tree))
  (when (not (null (my-tree-a tree)))
    (format stream "~A" (my-tree-a tree)))
  (when (not (null (my-tree-b tree)))
    (format stream "~A" (my-tree-b tree)))
  (when (not (null (my-tree-c tree)))
    (format stream "~A" (my-tree-c tree)))
  (format stream ")"))

;;(a) 一个函数来复制这样的树（复制完的节点与本来的节点是不相等（ `eql` ）的）
(setf tree-b (make-my-tree :node 'child-b))
(setf tree-c (make-my-tree :node 'child-c))
(setf tree-a-a (make-my-tree :node 'child-a-a))
(setf tree-a (make-my-tree :node 'child-a :a tree-a-a))
(setf tree (make-my-tree :node 'root :a tree-a :b tree-b :c tree-c))

(defun my-copy-tree (tree)
  (let ((node (my-tree-node tree))
	(a (my-tree-a tree))
	(b (my-tree-b tree))
	(c (my-tree-c tree)))
    (make-my-tree :node node
		  :a (if (null a)
			 nil
			 (my-copy-tree a))
		  :b (if (null b)
			 nil
			 (my-copy-tree b))
		  :c (if (null c)
			 nil
			 (my-copy-tree c)))))

(setf new-tree (my-copy-tree tree))
(eql new-tree tree)
(eql (my-tree-a new-tree) (my-tree-a tree))


;;(b) 一个函数，接受一个对象与这样的树，如果对象与树中各节点的其中一个字段相等时，返回真。
(defun my-tree-search(node tree)
  (cond ((null tree) nil)
	(t
	 (let ((tree-node (my-tree-node tree))
	       (a (my-tree-a tree))
	       (b (my-tree-b tree))
	       (c (my-tree-c tree)))
	   (if (equal node tree-node)
	       t
	       (or (my-tree-search node a)
		   (my-tree-search node b)
		   (my-tree-search node c)))))))



(defstruct (node
	     (:print-function
	      (lambda (node l r)
		(format t "#<~A>" (node-elt node)))))
  elt
  (l nil)
  (r nil))

(defun full-tree-print(bst)
  (if (null bst)
      "nil"
      (let ((elt (node-elt bst))
	    (l (node-l bst))
	    (r (node-r bst)))
	(format nil "(~A ~A ~A)"
		elt
		(full-tree-print l)
		(full-tree-print r)))))


;; 4.定义一个函数，接受一棵二叉搜索树，并返回由此树元素所组成的，一个由大至小排序的列表。
(defun tree-to-list (bst &key (order 'ascendant))
  (if (null bst)
      nil
      (let ((elt (node-elt bst))
	    (l (node-l bst))
	    (r (node-r bst)))
	(cond ((equal order 'ascendant)
	       (append (tree-to-list l) (list elt) (tree-to-list r)))
	      ((equal order 'descendant)
	       (append (tree-to-list r :order 'descendant) (list elt) (tree-to-list l :order 'descendant)))
	      (t nil)))))


(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(if (eql elt obj)
	    bst
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l (bst-insert obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elt elt
		 :r (bst-insert obj (node-r bst) <)
		 :l (node-l bst)))))))

(setf nums nil)

(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf nums (bst-insert x nums #'<)))



;;6.任何哈希表的内容可以由关联列表（assoc-list）来描述，其中列表的元素是 (k . v) 的形式，对应到哈希表中的每一个键值对。定义一个函数：
;;(a) 接受一个关联列表，并返回一个对应的哈希表。
(defun assoc-to-hash(assoclst)
  (if (null assoclst)
      nil
      (progn
	(setf ht (make-hash-table))
	(dolist (map assoclst)
	  (let ((key (car map))
		(value (cdr map)))
	    (setf (gethash key ht) value)))
	ht)))))
;;(b) 接受一个哈希表，并返回一个对应的关联列表。
(defun hash-to-assoc(ht)
  (if (null ht)
      nil
      (progn
	(setf assoclst nil)
	(maphash #'(lambda (k v)
		     (setf assoclst (append (list (cons k v)) assoclst))) ht)
	assoclst)))
