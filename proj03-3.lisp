(setq ext:*warn-on-redefinition* nil)

;;list 1 append

(defun MyAppend (l1a l1b)
    (setq list1r nil)
    (defun inner1 (a b)
        (if (not (equal (car a) nil))
            (progn
                (setq list1r (cons (car a) list1r))
                (inner1 (cdr a) b)
            )
            (progn ;;else
                (if (not (equal (car b) nil))
                    (progn
                        (setq list1r (cons (car b) list1r))
                        (inner1 a (cdr b))
                    )
                    (return-from inner1 list1r)
                )  
            )  
        )
    )
    (inner1 l1a l1b)
    (return-from MyAppend (reverse list1r))
)

(format t "~a~%" (MyAppend '(0 1 2) '(3 4 5)))


;; list 2 reverse
(defun reverse (list)
    (setq l2 nil)
    (setq input list)
    (defun inner2 (l)
        (if (not (equal (car l) nil))
            (progn
            (setq l2 (cons (car l) l2))
            (inner2 (cdr l))
            )
        )
    )  
    (inner2 input)
    (return-from reverse l2)  
)
(format t "~a~%" (reverse '(0 1 2 3 4 5 )))


;; list 3 map function
(defun add3(x)
    (return-from add3 (+ x 3))
)

(defun map (func list)
    (setq l3 nil)

    (defun inner3 (f l)
        (if (not (equal (car l) nil))
            (progn
            (setq x (funcall f (car l)))
            ;;(push x l3)
            (setq l3 (cons x l3))
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
        (if (not (equal (car l) nil))
            (progn
                (setq count 0)
                (defun loopList (li)
                    (if (and (equal (car l) (car li)) (not (equal (car li) nil)))
                        (setq count (+ count 1))
                    )
                    (unless (equal (cdr li) nil)
                    (loopList (cdr li))
                    )
                )
                (loopList l4)
                (if (= count 0)
                    ;;(push (car l) l4)
                    (setq l4 (cons (car l) l4))
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
        (if (not (equal (car l) nil))
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
        (if (not (equal (car l) nil))
            (progn
                (if (funcall f (car l))
                    ;;(push (car l) l6)
                    (setq l6 (cons (car l) l6))
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
(format t "q7) ~a~%" (addSorted '(0 999 4 6 8) '(1 3 5 7 9)))

;;list q 8
(defun add_to_end (numb list)
    (setq l8 nil)
    (push numb l8)
    (defun innr8 (n l)
        (if (not (equal (car l) nil))
            (progn
                ;;(push (car l) l8)
                (setq l8 (cons (car l) l8))
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
        (if (not (equal (car l) nil))
            (progn
                (if (equal (car l) e)
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
        (if (not (equal (car l) nil))
            (progn
                (if (equal (car l) e)
                    (progn);;do nothing
                    ;;(push (car l) l10);;else
                    (setq l10 (cons (car l) l10))
                )
                (inner10 e (cdr l))
            )
        )
    )
    (inner10 element list)
    (return-from removeAll (reverse l10))
)
(format t "~a~%" (removeAll 0 '(0 1 2 0 3 4 5)))

;; ------------------ SET METHODS ----------------------------------------------

;; set q 1

;; (defun template (element set)
;;     (setq set1 nil)
;;     (defun innerst (e s)
;;         (if (not (equal (car s) nil))
;;             (progn
;;                 (innerst e (cdr s))
;;             )
;;         )
;;     ) 
;;     (innerst element set)
;; )

(defun isMember (element set)
    (setq count 0)
    (defun s1 (e s)
        (if (not (equal (car s) nil))
            (progn
                (if (equal (car s) e)
                    (progn
                        (setq count (+ count 1))
                    )
                )
                (s1 e (cdr s))
            )
        )
    )
    (s1 element set)
    (if (equal count 0)
        (return-from isMember nil)
        (return-from isMember T)
    )
)
(format t "set Q1: ~A~%" (isMember 1 '(1 2 3)))

;; set q2

(defun insert (element set)
    (setq results2 nil)
    (if (isMember element set)
        (progn
            (return-from insert set)
        )
        (progn
            ;;(push element set)
            (setq set (cons element set))
            (return-from insert set)
        )
    )
)
(format t "~A~%" (insert "a" '("a" "b" "c")))

;; set q3 

(defun intersection (set3a set3b)
    (setq i3 nil)
    (defun innerI (s3a s3b)
        (if (not (equal (car s3a) nil))
            (progn
                (if (isMember (car s3a) s3b)
                    (if (not (isMember (car s3a) i3))
                        ;;(push (car s3a) i3)
                        (setq i3 (cons (car s3a) i3))
                    )
                )
                (innerI (cdr s3a) s3b)
            )
        )
    ) 
    (innerI set3a set3b)
    (innerI set3b set3a)
    (return-from intersection (reverse i3))
)
(format t "~A~%" (intersection '(1 4) '(1 2 3 4)))

;; set q4
(defun union (set3a set3b)
    (setq u4 nil)
    (defun innerU (s3a s3b)
        (if (not (equal (car s3a) nil))
            (progn
                (if (not (isMember (car s3a) u4))
                    ;;(push (car s3a) u4)
                    (setq u4 (cons (car s3a) u4))
                )
                (innerU (cdr s3a) s3b)
            )
        )
    ) 
    (innerU set3a set3b)
    (innerU set3b set3a)
    (return-from union u4)
)
(format t "~A~%" (union '(4 99) '(1 2 3 4)))

;; ;;set q5

(defun difference (set5a set5b)
    (setq d5 nil)
    (setq count 0)
    (defun innerD (s3a s3b)
        (if (not (equal (car s3a) nil))
            (progn
                (if (not (isMember (car s3a) s3b))
                    ;;(push (car s3a) d5)
                    (setq d5 (cons (car s3a) d5))
                )
                (innerD (cdr s3a) s3b)
            )
        )
    ) 
    (innerD set5a set5b)
    (return-from difference d5)
)
(format t "~A~%" (difference '("a" "b" "c") '("a" "c" "d")))

;;set q6

(defun Sdifference (set5a set5b)
    (setq d5 nil)
    (setq count 0)
    (defun innerD (s3a s3b)
        (if (not (equal (car s3a) nil))
            (progn
                (if (not (isMember (car s3a) s3b))
                    ;; (push (car s3a) d5)
                    (setq d5 (cons (car s3a) d5))
                )
                (innerD (cdr s3a) s3b)
            )
        )
    ) 
    (innerD set5b set5a)
    (innerD set5a set5b)
    (return-from Sdifference d5)
)
(format t "~A~%" (Sdifference '("a" "b" "c") '("a" "c" "d")))

;;set q7

(defun sub (set5a set5b)
    (defun innerD (s3a s3b)
        (if (not (equal (car s3a) nil))
            (progn
                (if (not (isMember (car s3a) s3b))
                    (return-from innerD nil)
                )
                (innerD (cdr s3a) s3b)
            )
            (return-from innerD T)
        )
    ) 
    ;; (innerD set5b set5a)
    (setq bool (innerD set5a set5b))
    (return-from sub bool)
)
(format t "~A~%" (sub '("a" "c") '("a" "c" "d")))

;;set q8

(defun sup (set5a set5b)
    (defun innerD (s3a s3b)
        (if (not (equal (car s3a) nil))
            (progn
                (if (not (isMember (car s3a) s3b))
                    (return-from innerD nil)
                )
                (innerD (cdr s3a) s3b)
            )
            (return-from innerD T)
        )
    ) 
    ;; (innerD set5b set5a)
    (setq bool (innerD set5b set5a))
    (return-from sup bool)
)
(format t "~A~%" (sup '("a" "c" "d" "e") '("a" "c" "j")))

;; set q 9

(defun sq9 (set)
    (setq sets9 nil)
    (setq count 0)
    (defun inners9 (s)
        (if (not (equal (car s) nil))
            (progn
                (setq count (+ count 1))
                (inners9 (cdr s))
            )
        )
    ) 
    (inners9 set)
    (return-from sq9 count)
)
(format t "~A~%" (sq9 '("a" "b" "c" "d")))

;; set q 10

;;----------------------------------------MATH FUNCTIONS--------------------------------------------

;; math q 1

(defun absoluteValue (numb)
    (if (< numb 0)
        (return-from absoluteValue (* numb -1))
        (return-from absoluteValue numb)
    )
)
(format t "~a~%" (absoluteValue -7))

;;math q 2

(defun factorial (n)
  (if (= n 1)              
    (return-from factorial 1)                           
    (return-from factorial (* n (factorial (- n 1))))
  )
) 

(format t "~A~%" (factorial 5))

;; math q3

(defun rightTri (n1 n2 n3)
    (if (equal (* n3 n3) (+ (* n1 n1) (* n2 n2)))
        (return-from rightTri T)
        (return-from rightTri nil)
    )
)
(format t "~A~%" (rightTri 5 12 13))

;; math q4

(defun gdc (n1 n2)
    (if (= n1 0) 
        (return-from gcd n2)
    ) 

    (if (= n2 0) 
        (return-from gcd n1)
    ) 
       
    (if (= n1 n2) 
        (return-from gcd n1)
    ) 
       
    (if (> n1 n2) 
        (return-from gcd (gcd(n1-n2 n2)))
    ) 

    (return-from gcd (gcd(n1 n2-n1))) 
)
(format t "gcd 8 12 ~A~%" (gcd 8 12))

;; math q 5

(defun lcd (n1 n2)
    (return-from lcd (/ (* n1 n2) (gcd n1 n2)))
)
(format t "lcm 4 and 6 => ~A~%" (lcd 4 6))

;; math 6

(defun fibonacci (numb)
    (if (<= numb 1)
        (return-from fibonacci numb)
    )
    (return-from fibonacci (+ (fibonacci (- numb 1)) (fibonacci (- numb 2))))
)
(format t "fibonacci => ~A~%" (fibonacci 4))

;; math 7 note: do it without modulo operator

(defun isDivisible (nmerator divisor)
)

(defun isPrime (number)
    (setq devisor 2)
    (defun innerP (n c)
        (if (equal (- n 1) c)
            (return-from innerP T)
        )
        (if (equal (mod n c) 0)
            (return-from innerP nil)
            (progn
            (setq c (+ c 1))
            (innerp n c)
            )
        )
    )
    (setq answ (innerP number devisor))
    (return-from isPrime answ)
)
(format t "is prime? ~A~%" (isPrime 5))

;; math q8

(defun primeNumber(index)
    (setq count 1)
    (setq currentNumber 2)

    (defun innerq8 (i)
        (if (equal i count)
            (return-from innerq8 (- currentNumber 1))
        )
        (if (isPrime currentNumber)
            (progn
                (setq count (+ count 1))
                (setq currentNumber (+ 1 currentNumber))
                (innerq8 i) 
            )
            (progn
                (setq currentNumber (+ 1 currentNumber))
                (innerq8 i) 
            )
        )
    ) 
    (setq result (innerq8 index))
    (return-from primeNumber result)
)
(setq pnq8 14)
(format t "the ~A prime is: ~A~%" pnq8 (primeNumber pnq8))

;; ;; -----------------------------------REQUIRED FUNCTIONS----------------------------------------------------------

;; required 1

(defun isperfect (number)
    (setq sum 0)
    (setq current 1)
    (defun innerPerf (i)
        (if (equal i current)
            (return-from innerPerf "done")
        )
        (if (equal (mod number current) 0)
            (progn
                (setq sum (+ sum current))
                (setq current (+ 1 current))
                (innerPerf i) 
            )
            (progn
                (setq current (+ 1 current))
                (innerPerf i) 
            )
        )
    ) 
    (innerPerf number)
    (if (equal sum number)
        (return-from isperfect T)
        (return-from isperfect nil)
    )
)
(format t "is perfect? ~A~% " (isperfect 5))

;; required q 2

(defun isAbundant (number)
    (setq sum 0)
    (setq current 1)
    (defun innerAb (i)
        (if (equal i current)
            (return-from innerAb "done")
        )
        (if (equal (mod number current) 0)
            (progn
                (setq sum (+ sum current))
                (setq current (+ 1 current))
                (innerAb i) 
            )
            (progn
                (setq current (+ 1 current))
                (innerAb i) 
            )
        )
    ) 
    (innerAb number)
    (if (> sum number)
        (return-from isAbundant T)
        (return-from isAbundant nil)
    )
)
(format t "is abundant? ~A~% " (isAbundant 12))

;; required 3

(defun isDeficient (number)
    (setq sum 0)
    (setq current 1)
    (defun innerDef (i)
        (if (equal i current)
            (return-from innerDef "done")
        )
        (if (equal (mod number current) 0)
            (progn
                (setq sum (+ sum current))
                (setq current (+ 1 current))
                (innerDef i) 
            )
            (progn
                (setq current (+ 1 current))
                (innerDef i) 
            )
        )
    ) 
    (innerDef number)
    (if (< sum number)
        (return-from isDeficient T)
        (return-from isDeficient nil)
    )
)
(format t "is deficient? ~A~% " (isDeficient 5))