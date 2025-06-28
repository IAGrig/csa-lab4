(defun rev-num (x acc)
  (if x
    (rev-num (/ x 10)
             (+ (* acc 10) (% x 10)))
    acc))

(defun is-pal (x)
  (= x (rev-num x 0)))

(defun find-max (i j)
  (if (> i 999)
    max
    (if (> j 999)
      (find-max (+ i 1) i)
        (progn
          (setq p (* i j))
          (if (and (> p max) (is-pal p))
            (setq max p))
          (find-max i (+ j 1))))))

(setq max 0)
(write-char (find-max 100 100) 1)
