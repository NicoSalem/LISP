(setq ext:*warn-on-redefinition* nil)

;;list 1 append
(defun MyAppend (l1a l1b)
    (setq l1 nil)
    (defun inner1 (a b)
        (if (not (eq (car a) nil))
            (progn
            (push (car a) l1)
            (inner1 (cdr a) b)
            )
            (progn ;;else
                (if (not (eq (car b) nil))
                    (progn
                    (push (car b) l1)
                    (inner1 '(nil) (cdr b))
                    )
                )
            )   
        )
    )
    (inner1 l1a l1b)
    (return-from MyAppend (reverse l1))
)
(format t "~a~%" (append '(0 1 2) '(3 4 5)))


;; list 2 reverse
(defun reverse (list)
    (setq l2 nil)
    (setq input list)
    (defun inner2 (l)
        (if (not (eq (car l) nil))
            (progn
            (push (car l) l2)
            (inner2 (cdr l))
            )
        )
    )  
    (inner2 input)
    (return-from reverse l2)  
)
(format t "~a~%" (reverse '(0 1 2 3 4 5)))


;; list 3 map function
(defun add3(x)
    (return-from add3 (+ x 3))
)

(defun map (func list)
    (setq l3 nil)

    (defun inner3 (f l)
        (if (not (eq (car l) nil))
            (progn
            (setq x (funcall f (car l)))
            (push x l3)
            (inner3 f (cdr l))
            )
        )
    )
    (inner3 func list)
    (return-from map (reverse l3))
)
(format t "~a~%" (map #'add3 '(0 1 2 3 4 5)))


;; list 4 remove duplicates
(defun removeDuplicates (list)
    (setq l4 nil)
    (defun inner4 (l)
        (if (not (eq (car l) nil))
            (progn
                (setq count 0)
                (defun loopList (li)
                    (if (and (eq (car l) (car li)) (not (eq (car li) nil)))
                        (setq count (+ count 1))
                    )
                    (unless (eq (cdr li) nil)
                    (loopList (cdr li))
                    )
                )
                (loopList l4)
                (if (= count 0)
                    (push (car l) l4)
                ) 
                (inner4 (cdr l)) 
            )
        ) 
    )
    (inner4 list)
    (return-from removeDuplicates (reverse l4))
)
(format t "~A~%" (removeDuplicates '(0 0 1 0 2 3 4 2 5)))

;;list q 5
(defun fold (v0 func list)
    (setq l5 nil)
    (setq value v0)
    (defun inner5 (f l)
        (if (not (eq (car l) nil))
            (progn
            (setq value (funcall f value (car l)))
            (inner5 f (cdr l))
            )
        )
    )
    (inner5 func list)
    (return-from fold value)
)
(format t "~a~%" (fold 10 #'- '(5 6)))

;;list q 6 
(defun lessthan3 (x)
    (if (< x 3) 
        (return-from lessthan3 T) 
        (return-from lessthan3 nil))
)

(defun filter (func list)
    (setq l6 nil)
    (defun inner6 (f l)
        (if (not (eq (car l) nil))
            (progn
                (if (funcall f (car l))
                    (push (car l) l6)
                )
                (inner6 f (cdr l))
            )
        )
    )
    (inner6 func list)
    (return-from filter (reverse l6))
)
(format t "~a~%" (filter #'lessthan3 '(0 1 2 3 5)))

;;list q 7 got the insertion sort from https://gist.github.com/zard1989/999290
(defun insert (item lst &optional (key #'<))
  (if (null lst)
    (list item)
    (if (funcall key item (car lst))
          (cons item lst) 
          (cons (car lst) (insert item (cdr lst) key)))))

(defun insertion-sort (lst &optional (key #'<))
  (if (null lst)
    lst
    (insert (car lst) (insertion-sort (cdr lst) key) key)))

(defun addSorted (list7a list7b)
    (setq result7 (MyAppend list7a list7b))
    (return-from addSorted (insertion-sort result7))
)
(format t "q7) ~a~%" (addSorted '(0 2 4 6 8) '(1 3 5 7 9)))

;;list q 8
(defun add_to_end (numb list)
    (setq l8 nil)
    (push numb l8)
    (defun innr8 (n l)
        (if (not (eq (car l) nil))
            (progn
                (push (car l) l8)
                (innr8 n (cdr l))
            )
        )
    )
    (setq i (reverse list))
    (innr8 numb i)
    (return-from add_to_end l8)
)
(format t "~a~%" (add_to_end 6 '(0 1 2 3 5)))

;;list q9

(defun indexOf (element list)
    (setq l9 nil)
    (setq index 0)
    (defun inner9 (e l)
        (if (not (eq (car l) nil))
            (progn
                (if (eq (car l) e)
                    (return-from inner9 index)
                    (setq index (+ index 1));;else
                )
                (inner9 e (cdr l))
            )
            (return-from inner9 -1)
        )
    )
    (setq i (inner9 element list))
    (return-from indexOf i)
)
(format t "~a~%" (indexOf 3 '(0 1 2 3 5)))

;; list q10

(defun removeAll (element list)
 (setq l10 nil)
    (defun inner10 (e l)
        (if (not (eq (car l) nil))
            (progn
                (if (eq (car l) e)
                    (progn);;do nothing
                    (push (car l) l10);;else
                )
                (inner10 e (cdr l))
            )
        )
    )
    (inner10 element list)
    (return-from removeAll (reverse l10))
)
(format t "~a~%" (removeAll 0 '(0 1 2 0 3 4 5)))








































