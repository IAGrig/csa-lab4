(defun read-loop ()
  (setq c (read-char 1))
  (store-byte c (+ buf i))
  (setq i (+ i 1))
  (if (= c 0)
      0
      (read-loop)))

(defun write-loop ()
  (setq c (load-byte (+ buf i)))
  (if (= c 0)
    0
    (progn
      (write-char c 1)
      (setq i (+ i 1))
      (write-loop))))

(defun inner (ii ij imin)
  (if (= ij n)
    imin
    (progn
      (setq b1 (load-byte (+ buf ij)))
      (setq b2 (load-byte (+ buf imin)))
      (inner ii
        (+ ij 1)
        (if (< b1 b2) ij imin)))))

(defun outer (i)
  (if (>= i (- n 1))
    0
    (progn
      (setq min (inner i (+ i 1) i))
      (setq tmp (load-byte (+ buf i)))
      (store-byte (load-byte (+ buf min)) (+ buf i))
      (store-byte tmp (+ buf min))
      (outer (+ i 1)))))

(defun sort-bytes (address length)
  (setq buf address)
  (setq n length)
  (setq i 0)
  (outer 0)
  n)

(setq buf (allocate 30))
(setq i 0)
(read-loop)
(setq n (- i 1))
(sort-bytes buf n)
(setq i 0)
(write-loop)
