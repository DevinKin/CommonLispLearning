(defun our-third (x)
  (car (cdr (cdr x))))


(defun sum-greater (x y z)
  (> (+ x y) z))


;; recursion
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

;; IO
(defun askem (string)
  (format t "~A" string)
  (read))


;; Variable
(defun ask-number()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))


;; Iteration
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-recursion (i end)
  (if (> i end)
      'done
      (progn
	(format t "~A ~A~%" i (* i i))
	(show-squares-recursion (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length-recursion (lst)
  (if (null lst)
      0
      (+ (our-length-recursion (cdr lst)) 1)))
