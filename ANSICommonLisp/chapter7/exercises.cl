;;1.定义一个函数，接受一个文件名并返回一个由字符串组成的列表，来表示文件里的每一行。

(defun outputlist(file linelst)
  (with-open-file (out file :direction :output
                            :if-exists :supersede)
    (if (listp linelst)
        (dolist (line linelst)
          (format out "~A~%" line)))))

;;2.定义一个函数，接受一个文件名并返回一个由表达式组成的列表，来表示文件里的每一行。
(defun inputlist (file)
  (with-open-file (in file :direction :input)
    (let ((lst nil))
      (do ((line (read-line in nil 'eof)
                 (read-line in nil 'eof)))
          ((eql line 'eof))
        (push (make-array 1 :initial-element line) lst))
      (reverse lst))))

;;3.假设有某种格式的文件文件，注解是由 % 字符表示。从这个字符开始直到行尾都会被忽略。定义一个函数，接受两个文件名称，并拷贝第一个文件的内容去掉注解，写至第二个文件。
(defun formatfile (inputfile outputfile)
  (with-open-file (in inputfile :direction :input)
    (with-open-file (out outputfile :direction :output
                                    :if-exists :supersede)
      (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
          ((eql line 'eof))
        (do* ((pos 0 (+ pos 1))
              (c (char line pos) (char line pos)))
             ((or (eql pos (length line)) (char= c #\%))
              (format out "~%"))
          (princ c out))))))

;;4.定义一个函数，接受一个二维浮点数组，将其用简洁的栏位显示。每个元素应印至小数点二位，一栏十个字符宽。（假设所有的字符可以容纳）。你会需要 array-dimensions
(defun printfloatarr (tarr)
  (let ((onedlen (array-dimension tarr 0))
        (twodlen (array-dimension tarr 1)))
    (dotimes (pos1 onedlen)
      (dotimes (pos2 twodlen)
        (format t "~10,2,,,F~%" (aref tarr pos1 pos2))))))

(setf tarr (make-array '(2 3)))
(setf (aref tarr 0 0) 12.3123)
(setf (aref tarr 0 1) 11.3123)
(setf (aref tarr 0 2) 10.3123)
(setf (aref tarr 1 0) 19.31)
(setf (aref tarr 1 1) 18.3)
(setf (aref tarr 1 2) 11)


;;5.修改 stream-subst 来允许万用字符 (wildcard) 可以在模式中使用。若字符 + 出现在 old 里，它应该匹配任何输入字符。
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
      (cond ((or (char= c (char old pos))
                 (char= #\+ (char old pos)))
             (format t "~A~%" c)
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
