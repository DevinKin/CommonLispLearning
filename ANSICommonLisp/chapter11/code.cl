;; 二维图形的面积
;; 矩形
(defstruct rectangle
  height width)

;; 圆形
(defstruct circle
  radius)

;; 面积
(defun area (x)
  (cond ((rectangle-p x)
         (* (rectangle-width x) (rectangle-height x)))
        ((circle-p x)
         (* pi (expt (circle-radius x) 2)))))

(let ((r (make-rectangle)))
  (setf (rectangle-height r) 2
        (rectangle-width r) 3)
  (area r))


;; CLOS二维图形的面积
(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'raidus) 2)))

(let ((r (make-instance 'rectangle)))
  (setf (slot-value r 'height) 2
        (slot-value r 'width) 3)
  (area r))


;; 创建实例
(setf c (make-instance 'circle))

;; 槽赋值
(setf (slot-value c 'radius) 1)


;; 槽的属性
(defclass circle ()
  ((radius :accessor circle-radius)
   (center :accessor circle-center)))

(setf c (make-instance 'circle))
(setf (circle-radius c) 1)
(circle-radius c)

(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 1)
   (center :accessor circle-center
           :initarg :center
           :initform (cons 0 0))))

(setf c (make-instance 'circle :radius 3))
(circle-radius c)
(circle-center c)


;; 共享的槽
(defclass tabloid ()
  ((top-story :accessor tabloid-story
              :allocation :class)))

(setf daily-blab (make-instance 'tabloid)
      unsolicited-mail (make-instance 'tabloid))

(setf (tabloid-story daily-blab) 'adultery-of-senator)
(tabloid-story unsolicited-mail)


;; 基类
(defclass graphic ()
  ((color :accessor graphic-color :initarg :color)
   (visible :accessor graphic-visible :initarg :visible
            :initform t)))

(defclass screen-circle (circle graphic) ())

(graphic-color (make-instance 'screen-circle
                              :color 'red :radius 3))

(defclass screen-circle (circle graphic)
  ((color :initform 'purple)))

(graphic-color (make-instance 'screen-circle))


;; 优先级
(defclass sculpture () (height width depth))

(defclass statue (sculpture) (subject))

(defclass metalwork () (metal-type))

(defclass casting (metalwork) ())

(defclass cast-statue (statue casting) ())


;; 通用方法
(defclass stuff () ((name :accessor name :initarg :name)))

(defclass ice-cream (stuff) ())

(defclass topping (stuff) ())

(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-scream with ~A topping."
          (name ic)
          (name top)))

(combine (make-instance 'ice-cream :name 'fig)
         (make-instance 'topping :name 'treacle))

(combine 23 'skiddoo)

(defmethod combine (x y)
  (list x y))

(defmethod combine ((ic ice-cream) x)
  (format nil "~A ice-cream with ~A."
          (name ic)
          x))

;; 调用两个对象参数的方法
(combine (make-instance 'ice-cream :name 'grape)
         (make-instance 'topping :name 'marshmallow))

;; 调用一个对象参数的方法
(combine (make-instance 'ice-cream :name 'clam)
         'reluctance)


(defmethod conbine ((x number) (y number))
  (+ x y))


;; 辅助方法
(defclass speaker () ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(speak (make-instance 'speaker)
       "I'm hungry")

(defclass intellectual (speaker) ())

(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))

(defmethod speak :after ((i intellectual) string)
  (princ " in some sense"))

(speak (make-instance 'intellectual)
       "I am hungry")

(defmethod speak :before ((s speaker) string)
  (princ "I think "))

(speak (make-instance 'intellectual) "I am hungry")

(defclass courtier (speaker) ())

(defmethod speak :around ((c courtier) string)
  (format t "Does the King believe that ~A?" string)
  (if (eql (read) 'yes)
      (if (next-method-p) (call-next-method))
      (format t "Indeed, it is a preposterous idea. ~%"))
  'bow)

(speak (make-instance 'courtier) "kings will last")

;; 方法组合
(defgeneric price (x)
  (:method-combination +))
(defclass jacket () ())
(defclass trousers () ())
(defclass suit (jacket trousers) ())

(defmethod price + ((jk jacket)) 350)
(defmethod price + ((tr trousers)) 200)
(price (make-instance 'suit))

(fmakunbound price)


;; 封装
(defpackage "CTR"
  (:use "COMMON-LISP")
  (:export "COUNTER" "INCREMENT" "CLEAR"))
(in-package ctr)

(defclass counter ()
  ((state :initform 0)))

(defmethod increment ((c counter))
  (incf (slot-value c 'state)))

(defmethod clear ((c counter))
  (setf (slot-value c 'state) 0))

(unintern)
