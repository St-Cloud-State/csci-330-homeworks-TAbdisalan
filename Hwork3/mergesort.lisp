(defun split-list (lst)
  (let ((half (floor (/ (length lst) 2))))
    (list (subseq lst 0 half) (subseq lst half))))

(defun merge-lists (lst1 lst2)
  (cond
    ((null lst1) lst2)
    ((null lst2) lst1)
    ((< (car lst1) (car lst2))
     (cons (car lst1) (merge-lists (cdr lst1) lst2)))
    (t
     (cons (car lst2) (merge-lists lst1 (cdr lst2))))))

(defun mergesort (lst)
  (if (<= (length lst) 1)
      lst
      (let ((halves (split-list lst)))
        (merge-lists (mergesort (first halves))
                     (mergesort (second halves))))))

;; Test
(print (mergesort '(5 1 2 3 5 4 3)))