(in-package :cl-user)
(defpackage cl-utils
  (:use :cl)
  (:export :last1
           :single
           :append1
           :conc1
           :mklist
           :longer
           :filter
           :flatten
           :prune
           :find2
           :before
           :after
           :duplicate
           :split-if
           :most
           :best
           :mostn
           :map0-n
           :map1-n
           :mapa-b
           :map->
           :mappend
           :mapcars
           :rmapcar
           :readlist
           :prompt
           :break-loop
           :mkstr
           :symb
           :reread
           :explode))
(in-package :cl-utils)

(proclaim '(inline last1 single append1 conc1 mklist))

;; リストの最後の要素を返す
(defun last1 (lst)
  (car (last lst)))

;; 引数が1個の要素を持つリストかどうかを調べる
(defun single (lst)
  (and (consp lst) (not (cdr lst))))

;; リストの最後に要素を追加する ※非破壊
(defun append1 (lst obj)
  (append lst (list obj)))

;; リストの最後に要素を追加する ※破壊
(defun conc1 (lst obj)
  (nconc lst (list obj)))

;; 要素をリストにして返す
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;; リストの長さを比べる
(defun longer (x y)
  (labels ((compare (x y)
                    (and (consp x)
                             (or (null y)
                                 (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))

;; t値が返される要素全てをリストにして返す
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

;; リストを新しいリストの部分リストとしてまとめる
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;; リストを平滑にする
(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; 引数として与えた関数が真を返す葉は全て除かれる
(defun prune (test tree)
  (labels ((rec (tree acc)
                (cond ((null tree) (nreverse acc))
                      ((consp (car tree))
                       (rec (cdr tree)
                            (cons (rec (car tree) nil) acc)))
                      (t (rec (cdr tree)
                              (if (funcall test (car tree))
                                  acc
                                (cons (car tree) acc)))))))
    (rec tree nil)))

;; 引数に与えた関数が真を返す値と真偽値を返す
(defun find2 (fn lst)
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
          (values (car lst) val)
        (find2 fn (cdr lst))))))

;; あるオブジェクトがリスト内で別のオブジェクトよりも先に現れるかどうかを調べる
(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

;; beforeに加え、リスト内にオブジェクトを見付けたとき、リストのcdr部を返す
(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

;; 2つ目の重複オブジェクト以降のcdrを返す
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

;; 引数に与えた関数が真を返す場所を含めたリストとそれ以前を返す
(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;; 返した要素の与えた最高点も返す
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max score))))
      (values wins max))))

;; 述語がその他の要素全てに対して勝っていると判断した要素を返す
(defun best (fn lst)
  (if (null lst)
      nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
            (setq wins obj)))
      wins)))

;; 点数付関数とリストを引数に取り、関数が最高点を付ける要素全てからなるリストと最高点を返す
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setq max score
                       result (list obj)))
                ((= score max)
                 (push obj result)))))
      (values (nreverse result) max))))

;; 与えられた関数を０からnまで適用したリストを返す
(defun map0-n (fn n)
  (mapa-b fn 0 n))

;; 与えられた関数を１からnまで適用したリストを返す
(defun map1-n (fn n)
  (mapa-b fn 1 n))

;; 与えられた関数をaからステップ増加でbまで適用したリストを返す
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

;; mapa-bの抽象化
(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

;; mapcanの非破壊
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

;; mapcarで複数のリストを処理したいとき
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

;; 再帰的mapcar。ツリーでたどるmapcar
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
    (apply #'mapcar
           #'(lambda (&rest args)
               (apply #'rmapcar fn args))
           args)))

;; ユーザーからの入力をリストで返す
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

;; 質問の表示と回答の読取を統合したもの
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

;; lispのREPLを真似たい時に使える
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
         (return)
       (format *query-io* "~A~%" (funcall fn in))))))

;; 複数のシンボルを文字列にする
(defun mkstr (&rest args)
  (with-output-to-string (s)
                         (dolist (a args) (princ a s))))

;; 文字列をシンボルにする
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; symbの一般型
(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

;; シンボルを引数にとり、その文字から作られるシンボルのリストを返す
(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))



;; blah blah blah.
