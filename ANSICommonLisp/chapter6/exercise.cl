;;1.定义一个 tokens 版本 (67 页)，接受 :test 与 :start 参数，缺省分别是 #'constituent 与 0。
(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str :test test :start p2)
		    nil)))
	nil)))


;;2.定义一个 bin-search (60 页)的版本，接受 :key , :test , start 与 end 参数，有着一般的意义与缺省值。(译注: 60 页在 4.1 小节)
(defun bin-search(obj vec < > &key (key #'identity) (test #'eql) (start 0) (end 0 end-provided-p))
  (let ((real-end (or (and end-provided-p end) (length vec))))
    (if (> (- real-end start) 0)
	(let ((mid (floor (/ (+ start real-end) 2))))
	  (if (< obj (aref vec mid))
	      (bin-search obj vec < > :key key :test test :start start :end mid)
	      (if (> obj (aref vec mid))
		  (bin-search obj vec < > :key key :test test :start mid :end real-end)
		  (if (funcall test obj (aref vec mid))
		      obj)))))))

;;3. 定义一个函数，接受任何数目的参数，并返回传入的参数。
(defun my-values(&rest args)
  (values args))

(defun most-two(fn lst)
  (if (null lst)
      (values nil nil nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins))
	     (sndwins (cadr lst))
	     (sndmax (funcall fn sndwins)))
	(when (> sndmax max)
	  (setf tmpmax sndmax
		tmpwins sndwins
		sndmax max
		sndwins wins 
		max tmpmax
		wins tmpwins))
	(dolist (obj (cddr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setf sndmax max
		   sndwins wins
		   max score
		   wins obj))
	    (when (and (> score sndmax) (< score max))
	      (setf sndmax score
		   sndwins obj))))
	(values wins max sndwins sndmax))))

;;5.用 filter (105 页) 来定义 remove-if （没有关键字）。(译注: 105 页在 6.4 小节)
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))


