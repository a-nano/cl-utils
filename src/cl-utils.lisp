(in-package :cl-user)
(defpackage cl-utils
  (:use :cl)
  (:export :last1
           :single
           :append1
           :conc1
           :mklist
           :longer
           :filter))
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

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;; blah blah blah.
