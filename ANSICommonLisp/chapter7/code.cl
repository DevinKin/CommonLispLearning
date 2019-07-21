;; 创建一个路径
(setf path (make-pathname :name "myfile"))

;; 创建一个可写至"myfile"文件的流
(setf str (open path :direction :output
                :if-exists :supersede))

;; 将字符串输入到myfile流，即文件中
(format str "Something~%")


;; 读取文件示例
(setf str (open path :direction :input))

(read-line str)

(close str)

;; with-open-file宏写入文件
(with-open-file (str path :direction :output
                          :if-exists :supersede)
  (format str "Something~%"))



;; read-line函数
(progn
  (format t "Please enter your name: ")
  (read-line))

;; 顶层显示一个文件的内容
(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

(read-from-string "a b c")

(prin1 "Hello")
(princ "Hello")

(format t "~S ~A" "z" "z")
(format nil "~,2,,,F" 26.21875)
(format nil "~,2F" 26.21875)


;; 缓冲区
(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))


(defun buf-pop (b)
  (prog1
      (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new b) (buf-end b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new b) (buf-end b)))

(defun buf-clear (b)
  (setf (buf-start b) -1
        (buf-used b) -1
        (buf-new b) -1
        (buf-end b) -1))


(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

;; 替换方法
(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst old new in out))))

;;替换算法
(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
         (or (setf from-buf (buf-next buf))
             (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
                      (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))


;; 宏字符
(let ((*print-array* t))
  (vectorp (read-from-string (format nil "~S"
                                     (vector 1 2)))))
