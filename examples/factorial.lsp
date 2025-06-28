(defun fact (n acc)
  (if n
    (fact (- n 1) (* acc n))
    acc))

(write-char (fact 6 1) 1)
