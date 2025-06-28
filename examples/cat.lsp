(defun isr1 ()
  (enable-interrupts)
  (setq c1 (read-char 1))
  (write-char c1 1))

(defun isr2 ()
  (disable-interrupts)
  (setq c2 (read-char 2))
  (write-char c2 2)
  (enable-interrupts))

(defun loop ()
  (loop))

(enable-interrupts)
(loop)
