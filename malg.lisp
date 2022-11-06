(in-package :malg)

(defun insertion-sort (A comp)
  (defun  while (i key)
    (if (and (>= i 0) (funcall comp (aref A i) key))
        (progn (setf (aref A (+ i 1)) (aref A i))
               (while (- i 1) key)) (+ i 1)))
  (loop for j from 1 to (- (length A) 1) do
        (let
            ((key (aref A j))
             (i (- j 1)))
          (setf (aref A (while i key)) key))))
