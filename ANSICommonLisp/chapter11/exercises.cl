;;1.替图 11.2 所定义的类定义访问器、 initforms 以及 initargs 。重写相关的代码使其再也不用调用 slot-value 。
(defclass rectangle ()
  ((height :initarg :height :initform 1 :accessor height)
   (width :initarg :width :initform 1 :accessor width)))

(defclass circle ()
  ((radius :initarg :radius :initform 1 :accessor radius)))

(defmethod area ((x rectangle))
  (* (height x) (width x)))

(defmethod area ((x circle))
  (* pi (expt (radius x) 2)))

(let ((r (make-instance 'rectangle)))
  (setf (height r) 2
        (width r) 3)
  (area r))

;;3.假设有若干类别定义如下：
;;(defclass a (c d)   ...)  (defclass e ()  ...)
;;(defclass b (d c)   ...)  (defclass f (h) ...)
;;(defclass c ()      ...)  (defclass g (h) ...)
;;(defclass d (e f g) ...)  (defclass h ()  ...)
;;画出表示类别 a 祖先的网络以及列出 a 的实例归属的类别，从最相关至最不相关排列。
;;替类别 b 也做 (a) 小题的要求。

;;a -> c
;;a -> d -> e
;;  -> d -> f -> h
;;  -> d -> g -> h

;;b -> d -> e
;;  -> d -> f -> h
;;  -> d -> g -> h
;;b -> c

