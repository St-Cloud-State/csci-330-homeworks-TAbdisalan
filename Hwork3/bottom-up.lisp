(defun make-sorted-pairs (lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list (list (car lst))))
    (t (let ((a (car lst))
             (b (cadr lst)))
         (cons (if (<= a b) (list a b) (list b a))
               (make-sorted-pairs (cddr lst)))))))

(defun merge-pairs (lst1 lst2)
  (cond
    ((null lst1) lst2)
    ((null lst2) lst1)
    ((<= (car lst1) (car lst2))
     (cons (car lst1) (merge-pairs (cdr lst1) lst2)))
    (t (cons (car lst2) (merge-pairs lst1 (cdr lst2))))))

(defun bottom-up-merge (lst)
  (if (null (cdr lst))
      (car lst)
      (bottom-up-merge (mapcar #'(lambda (pair)
                                   (if (and (cdr pair) (cdr (cdr pair)))
                                       (merge-pairs (car pair) (cadr pair))
                                       (car pair)))
                               (make-sorted-pairs lst)))))

;; Testing bottom-up mergesort
(print (bottom-up-merge '(2 3 1 2 5 6 4 3)))
