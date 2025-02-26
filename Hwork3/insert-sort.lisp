(defun insert (x lst)
  "Inserts an element into a sorted list."
  (cond
    ((null lst) (list x))
    ((<= x (car lst)) (cons x lst))
    (t (cons (car lst) (insert x (cdr lst))))))

(defun insertion-sort (lst)
  "Sorts a list using insertion sort."
  (if (null lst)
      nil
      (insert (car lst) (insertion-sort (cdr lst)))))

;; Test
(insertion-sort '(2 3 4 3 1 2 3 2 1))