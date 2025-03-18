(defun match-literal (expected tokens)
  (cond
    ((null tokens)
     (error "Ran out of tokens while expecting ~A" expected))
    ((eql (car tokens) expected)
     (cdr tokens))
    (t
     (error "Expected ~A but found ~A." expected (car tokens)))))

(defun parse-I (tokens)
  (let ((rest tokens))
    (setf rest (match-literal 'i rest))
    (setf rest (parse-E rest))
    (setf rest (match-literal 's rest))
    (setf rest (parse-E rest))
    (setf rest (match-literal 's rest))
    rest))

(defun parse-E (tokens)
  (let ((rest tokens))
    (setf rest (parse-G rest))
    (setf rest (parse-R rest))
    rest))

(defun parse-R (tokens)
  (let ((rest tokens))
    (when (and rest (eql (car rest) 'o))
      (setf rest (cdr rest))
      (setf rest (parse-G rest))
      (setf rest (parse-R rest)))
    rest))

(defun parse-G (tokens)
  (cond
    ((null tokens)
     (error "parse-G: Ran out of tokens, expected x|y|z|w."))
    ((member (car tokens) '(x y z w))
     (cdr tokens))
    (t
     (error "parse-G: Expected x|y|z|w, got ~A" (car tokens)))))

(defun parse-S (tokens)
  (cond
    ((null tokens)
     (error "parse-S: Ran out of tokens, expected s or d."))
    ((eql (car tokens) 's)
     (cdr tokens))
    ((eql (car tokens) 'd)
     (let ((rest (cdr tokens)))
       (setf rest (parse-L rest))
       (setf rest (match-literal 'b rest))
       rest))
    (t
     (error "parse-S: Expected s or d, got ~A" (car tokens)))))

(defun parse-L (tokens)
  (let ((rest tokens))
    (when (or (null rest) (not (eql (car rest) 's)))
      (error "parse-L: Expected s, got ~A" (car rest)))
    (setf rest (cdr rest))
    (when (and rest (eql (car rest) 's))
      (setf rest (parse-L rest)))
    rest))

(defun parse-start (tokens)
  (let ((remaining (parse-I tokens)))
    (if (null remaining)
        (format t "Parse succeeded!~%")
        (format t "Parse succeeded, leftover tokens: ~A~%" remaining))))
