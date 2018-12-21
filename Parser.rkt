#lang racket
;;to run type: (isAssign list)
;;to run with a different list: (isAssign "A = ( A + A )")
;;tester set in place.
;;please make sure when typing a new list for checking
;;that you have spaces before and after every variable, except next
;;to the quotes. Heres an Example: "A = ( A + A )"
(define list "A = C * ( A + C )")

;;splits the string into a list for comparison methods.

(define (stringToList x)
  (regexp-split #px" " x))

;; below are a list of methods to check if the variable passed in is equal to the expected variable.

(define (isID st)
  (printf"isID ran\n")
  (case st
    (("A") #t)
    (("B") #t)
    (("C") #t)
    (else #f))
  )

(define (isEqualSign st)
  (printf"isEqualSign ran\n")
;;  (printf "~a \n" st)
  (if (equal? st "=") #t
  #f))

(define (isMultiplySign lst)
  (printf"isMultiplySign ran\n")
   (define char(car lst))
  (if (equal? char "*") #t
      #f)
  )

(define (isPlusSign lst)
  (printf"isPlusSign ran\n")
  (printf"List:~a\n" lst)
  (define char(car lst))
  (if (equal? char "+") #t
      #f)
  )
(define (isFirstBracket lst)
  (printf"isFirstBraket ran\n")
  (printf"List:~a\n" lst)
  (define char(car lst))
  (if (equal? char "(") #t
      #f)
)

(define (isLastBracket lst)
  (printf"isLastBraket ran\n")
  (printf"List:~a\n" lst)
  (if(equal? lst ")") #t
  #f)
)

(define (isLengthOneExpr st)
  (printf"isLengthOneExpr ran\n")
   (if (and (= (length st) 1) (isID (car st)))
      #t
      #f
  ))

;;below are methods to check if the expression is valid within the given grammer


(define (isIDplusExpr lst)
  (printf"isIDplsExpr ran\n")
  (printf"List:~a\n" lst)
   (define newList(cdr lst))
;;if the first thing is not an id, then its not valid
  (if (not (isID (car lst)))
           #f
;;if it was an id then check if the first thing in the cdr is a plus sign
     (if (isPlusSign (cdr lst))
         (isExpr(cdr newList)) #f
         
           )))


(define (isIDTimesExpr lst)
  (printf"isIDTimesExpr ran\n")
  (printf"List:~a\n" lst)
  (define newList(cdr lst))
;;if the first thing is not an id, then its not valid
  (if (not (isID (car lst)))
           #f
;;if it was an id then check if the first thing in the cdr is a plus sign
           (if (isMultiplySign (cdr lst))
                (isExpr(cdr newList)) #f
         
           )))

(define (isExprInParens lst)
  (printf"isExprInParens ran\n")
  (printf"List:~a\n" lst)
  
  (if (isFirstBracket lst)
      (findLastBracket(cdr lst))
      #f))

;;below method is from the resource listed above, gets the last element
;;in the list, and is used for checking if the parenthesis matches.
(define (last_element l)
  (printf"last_element ran\n")
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))

(define (all-but-last l)
  (printf"all-but-last ran\n")
  (cond [(empty? l) (error 'all-but-last "empty list")]
        [(empty? (rest l)) empty]
        [else (cons (first l) (all-but-last (rest l)))]))

(define (findLastBracket lst)
  (printf "THe list~a\n" lst)
  
  (define newList lst)
(printf "THe new list~a\n" newList)
  (printf "THe list~a\n" lst)
  
  (printf"findLastBracket ran\n")
  (define char(last_element newList))
   (if(isLastBracket char) (isExpr ( all-but-last lst))
     ;; (if (isID (last_element newList)) not sure why i was checking for this.. xD
     ;;     (findLastBracket (all-but-last lst))
      #f))

;;below are the "running" methods, that basically calls the above methods to verify the passed in grammar.

(define (isAssign st)
  (define vals (stringToList st))
  ;<assign> -> id = expr
  ;first thing needs to be an id
  (printf "~a \n" vals)
  (if (isID (car vals))
     (step2 (cdr vals))
 #F))

(define (step2 lst)
 (printf "Looking for the first '=' \n")
 (if (isEqualSign (car lst))
   (step3 (cdr lst)) ; step three we look for the expression
   #F ; no second equal, game over
))

(define (step3 lst)
(printf "Must have found first '=' Start looking at expressions \n")
 (if (isExpr lst)
 #t
#f))

(define (isExpr lst)
 (printf "Looking for an expression \n")
  (printf"List:~a\n" lst)
 ;first check for length 1 expression with an id
  (if (isLengthOneExpr lst)
  #t
   (if (isIDplusExpr lst)
    #t
      (if (isIDTimesExpr lst)
         #t
           (if (isExprInParens lst)
             #t
             #f)
           )
        )
     )
 )
