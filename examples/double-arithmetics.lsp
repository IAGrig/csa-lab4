(defun +! (ahi alo
           bhi blo)
  (setq ro_lo (+ alo blo))
  (setq ro_hi (+ ahi bhi))
  0)

(defun ++ (ahi alo
           bhi blo)
  (enable-eam)
  (setq ro_lo (+ alo blo))
  (setq ro_hi (+ ahi bhi))
  (disable-eam)
  0)


(+! 0 8388607
    0       1)
(write-char ro_hi 1)
(write-char ro_lo 1)

(++ 0 8388607
    0       1)
(write-char ro_hi 2)
(write-char ro_lo 2)

(write-char (*^ 4096 4096) 3)
(write-char (* 4096 4096) 3)
