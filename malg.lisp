(in-package :malg)

(defconstant e 2.71828182845904523536028747135266249775724709369995)

(defun insertion-sort (arr &optional comp-foo)
  (if (not comp-foo) (setf comp-foo #'>))
  (defun  while (i key)
    (if (and (>= i 0) (funcall comp-foo (elt arr i) key))
        (progn (setf (elt arr (+ i 1)) (elt arr i))
               (while (- i 1) key)) (+ i 1)))
  (loop for j from 1 to (- (length arr) 1) do
        (let
            ((key (elt arr j))
             (i (- j 1)))
          (setf (elt arr (while i key)) key))))

(defun my-merge (arr1 arr2 &optional destination bot comp-foo)
  (if (not bot) (setf bot 0))
  (let ((i 0) (j 0) (len1 (length arr1)) (len2 (length arr2)) (len-dest (+ (length arr1) (length arr2))))
    (if (not destination)
        (setf destination
              (make-array len-dest :initial-element nil)))
    (if (not comp-foo) (setf comp-foo #'<))
    (loop while (and (< i len1) (< j len2)) do
          (if (funcall comp-foo (elt arr1 i) (elt arr2 j))
              (progn (setf (elt destination bot) (elt arr1 i)) (incf i) (incf bot))
              (progn (setf (elt destination bot) (elt arr2 j)) (incf j) (incf bot))))
    (if (= i len1)
        (loop for j from j to (- len2 1) do
              (setf (elt destination bot) (elt arr2 j)) (incf bot))
        (loop for i from i to (- len1 1) do
              (setf (elt destination bot) (elt arr1 i)) (incf bot)))
    (return-from my-merge destination)))

(defun merge-sort (arr &optional bot top)
  (if (not bot) (setf bot 0))
  (if (not top) (setf top (length arr)))
  (if (> (- top bot) 1)
      (let ((mid (floor (/ (+ top bot) 2))))
        (merge-sort arr bot mid)
        (merge-sort arr mid top)
        (my-merge (subseq arr bot  mid) (subseq arr mid top) arr bot))))

(defun random-from-range (bot top)
  (return-from random-from-range (+ bot (random (+ 1 (- top bot))))))

(defun randomize-in-place (arr)
  (let ((n (- (length arr) 1)))
    (loop for i from 0 to n do
          (rotatef (elt arr i) (elt arr (random-from-range i n))))
    (return-from randomize-in-place arr)))

(defun bin-search (arr search-el)
  (let ((bot 0) (top (- (length arr) 1)))
    (if (or (> (elt arr bot) search-el) (< (elt arr top) search-el))
        (return-from bin-search NIL)
        (let  ((mid (floor (/ (+ bot top) 2))))
          (loop while (and (/= (elt arr mid) search-el) (> top bot)) do
                (if (> (elt arr mid) search-el)
                    (progn (setf top mid) (setf mid (floor (/ (+ bot top) 2))))
                    (progn (setf bot mid) (setf mid (ceiling (/ (+ bot top) 2))))))
          (return-from bin-search
            (if (= (elt arr mid) search-el)
                mid
                NIL))))))

(defun on-line-maximum (array)
  (let ((n (length array)) (k (ceiling (/ (- (length array) 1) e)))
        (maximum (elt array (random-from-range 0 (- (ceiling (/ (- (length array) 1) e)) 1)))))
    (loop for i from 0 to (- k 1) do
          (if (> (elt array i) maximum)
              (setf maximum (elt array i))))
    (loop for i from k to (- n 1) do
          (if (> (elt array i) maximum)
              (return-from on-line-maximum i)))
    (return-from on-line-maximum (- n 1))))
