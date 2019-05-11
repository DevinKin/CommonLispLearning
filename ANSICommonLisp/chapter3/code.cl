(defun our-listp(x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (cons x)))


(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
	   (consp y)
	   (our-equal (car x) (car y))
	   (our-equal (cdr x) (cdr y)))))


(defun our-copy-list(lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))


;; run-length encoding
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


;; Access
(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))


;; Tree
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
	    (our-copy-tree (cdr tr)))))

(defun our-subst (old new tree)
  (if (eql tree old)
      new
      (if (atom tree)
	  tree
	  (cons (our-subst old new (car tree))
		(our-subst old new (cdr tree))))))


;; recursion
(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))


;; sets
(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
	   lst
	   (our-member-if fn (cdr lst)))))


;; sequence
(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (let ((mid (/ len 2)))
	   (equal (subseq s 0 mid)
		  (reverse (subseq s mid)))))))


(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

;; push
(setf lst (cons obj lst)))

;; pop
(let ((x (car lst)))
  (setf lst (cdr lst))
  x)


(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))


;; Dotted Lists
(defun proper-list? (x)
  (or (null x)
      (and (consp x)
	   (proper-list? (cdr x)))))


;; Assoc Lists
(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
	 (if (eql key (car pair))
	     pair
	     (our-assoc key (cdr alist))))))


;; shortest path
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
