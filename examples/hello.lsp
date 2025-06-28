(defun print-string (addr i)
  (if (setq c (load-byte (+ addr i)))
    (progn
      (write-char c 1)
      (print-string addr (+ i 1)))
    (write-char c 1)))

(setq str "Hello")
(print-string str 0)
