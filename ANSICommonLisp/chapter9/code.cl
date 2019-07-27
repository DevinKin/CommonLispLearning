;;回文函数
(defun palindrome? (x)
  (let ((mid (/ (length x) 2)))
    (equal (subseq x 0 (floor mid))
           (reverse (subseq x (ceiling mid))))))


;;truncate定义
(defun our-truncate (n)
  (if (> n 0)
      (floor n)
      (ceiling n)))

(mapcar #'round '(-2.5 -1.5 1.5 2.5))

(mapcar #'signum '(-2 -0.0 0.0 0 .5 3))


;; 光线追踪

;; 平方
(defun sq (x)
  (* x x))

;; 平方和 (x^2 + y^2 + z^2)
(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

;; 单位向量 (x / (x^2 + y^2 + z^2))
(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))
  x y z)

;; (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2
(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

;; x= -b+_(b^-4ac)^(1/2)/2a 的最小实数x。
(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((discrt (- (sq b) (* 4 a c))))
        (min (/ (+ (- b) discrt) (* 2 a))
             (/ (- (- b) discrt) (* 2 a))))))


;; 最小光线追踪器代码
;; surface结构用来表示模拟世界的物体， color范围：0-黑色，1-白色。
(defstruct surface color)

(defparameter *world* nil)

(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y)))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
                   (- y (y eye))
                   (- z (z eye)))
    (round (* (sendary eye xr yr zr) 255))))

(defun sendary (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (setf surface s hit h dist d))))
      (values surface hit))))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))
