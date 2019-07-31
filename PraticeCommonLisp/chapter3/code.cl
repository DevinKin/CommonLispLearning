;; 建立一个cd
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; 录入cd函数
(defvar *db* nil)
(defun add-record (cd)
  (push cd *db*))

;; 录入cd
;(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
;(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
;(add-record (make-cd "Home" "Dixie CHicks" 9  t))


;; 打印db的内容
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10t~a~%~}~%" cd)))


;; 读入一行文本
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; 依据提示读入并建立新的cd记录
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Pipped [y/n]: ")))


;; 批量读入cd
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: " )) (return))))


;; 保存和加载数据库
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))


;; 保存数据库到文件中
;(save-db "/home/devinkin/learning/CommonLispLearning/PraticeCommonLisp/chapter3/mycds.db")


;; 加载文件回数据库
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; 载入数据库
;(load-db "/home/devinkin/learning/CommonLispLearning/PraticeCommonLisp/chapter3/mycds.db")

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))

(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

(remove-if-not #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)

;; 根据artist字段搜索
(defun select-by-artist (artist)
  (remove-if-not #'(lambda (cd) (equal (getf cd :artist) artist))
                 *db*))

;; 通用select函数
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))


;; artist选择器
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; 调用artist选择器
(select (artist-selector "Dixie Chicks"))

;; where子句实现
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
      (if ripped-p (equal (getf cd :ripped) ripped) t))))

(select (where :rating 10 :ripped nil))


;; 更新子句实现
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db* (mapcar
              #'(lambda (row)
                  (when (funcall selector-fn row)
                    (if title (setf (getf row :title) title))
                    (if artist (setf (getf row :artist) artist))
                    (if rating (setf (getf row :rating) rating))
                    (if ripped (setf (getf row :ripped) ripped)))
                  row) *db*)))

;; 删除记录
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


;; 优化where
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;; 使用loop宏
(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

;; where重构
(defmacro where (&rest clauses)
  `#'(lambda (cd
       (and ,@(make-comparisons-list clauses))))
