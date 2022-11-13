(in-package :malg)

(defun insertion-sort (A &optional comp-foo)
  (if (not comp-foo) (setf comp-foo #'>))
  (defun  while (i key)
    (if (and (>= i 0) (funcall comp-foo (aref A i) key))
        (progn (setf (aref A (+ i 1)) (aref A i))
               (while (- i 1) key)) (+ i 1)))
  (loop for j from 1 to (- (length A) 1) do
        (let
            ((key (aref A j))
             (i (- j 1)))
          (setf (aref A (while i key)) key))))

(defun my-merge (arr1 arr2 &optional destination comp-foo)
  (let ((i 0) (j 0) (len1 (length arr1)) (len2 (length arr2)) (k 0) (len-dest (+ (length arr1) (length arr2))))
    (if (not destination)
        (setf destination
              (make-array len-dest :initial-element nil)))
    (if (not comp-foo) (setf comp-foo #'<))
    (loop while (and (< i len1) (< j len2)) do
          (if (funcall comp-foo (elt arr1 i) (elt arr2 j))
              (progn (setf (elt destination k) (elt arr1 i)) (incf i) (incf k))
              (progn (setf (elt destination k) (elt arr2 j)) (incf j) (incf k))))
    (if (= i len1)
        (loop for k from k  to (- len-dest 1) do
              (setf (elt destination k) (elt arr2 j)) (incf j))
        (loop for k from k  to (- len-dest 1) do
              (setf (elt destination k) (elt arr1 i)) (incf i)))
    (return-from my-merge destination)))
