(load "lispy.ss")

(define (display-all . args)
  (for-each display args))

(define (escape expr)
  (list expr
        (if (procedure? (eval expr))
            "<proc>"
            (eval expr))))

(define (assert exprs)
  (if (eval exprs)
      #t
      (display-all
        exprs
        " expected to be true "
        (map escape exprs)
        "\n")))

(assert '(eq? @t #t))
(assert '(eq? @f #f))

(assert '(@null? '()))
(assert '(not (@null? '(1 2))))
(assert '(not (@null? @t)))
(assert '(not (@null? @f)))

(assert '(@list? '()))
(assert '(@list? '(1)))
(assert '(@list? '(1 2 3)))
(assert '(not (@list? @f)))

(assert '(equal? '(1) (@list 1)))
(assert '(equal? '(1 2) (@list 1 2)))
(assert '(equal? '(1 2 3) (@list 1 2 3)))

(assert '(equal? '() (@reverse '())))
(assert '(equal? '(1) (@reverse '(1))))
(assert '(equal? '(1 2) (@reverse '(2 1))))
(assert '(equal? '(1 2 3) (@reverse '(3 2 1))))

(assert '(equal? '(1 2 3) (@append '(1) '(2 3))))
(assert '(equal? '(1 2 3) (@append '(1 2 3) '())))
(assert '(equal? '(1 2 3) (@append '() '(1 2 3))))
(assert '(equal? '(1 2 2 3) (@append '(1) '(2) '(2 3))))

(assert '(eq? @t (@not @f)))
(assert '(eq? @f (@not @t)))
(assert '(eq? @f (@not '(1 2 3))))

(assert '(equal? 2 (@and 1 2)))
(assert '(equal? 3 (@and 1 2 3)))
(assert '(equal? @f (@and @f 2)))
(assert '(equal? @f (@and 2 @f)))

(assert '(equal? 1 (@or 1 2)))
(assert '(equal? 1 (@or 1 2 3)))
(assert '(equal? 2 (@or @f 2)))
(assert '(equal? 5 (@or @f @f 5 @f)))
(assert '(equal? @f (@or @f @f @f)))

(assert '(equal? '(#t #f #t) (@map odd? '(1 2 3))))
(assert '(equal? '(1 4 9) (@map (lambda (x) (* x x)) '(1 2 3))))

(assert '(equal? '(1 3) (@filter odd? '(1 2 3 4))))

(assert '(equal? '() (@flatten '())))
(assert '(equal? '() (@flatten '((((((())))))))))
(assert '(equal? '(1 2 3) (@flatten '((1) 2 (3)))))

(assert '(equal? 7 (@reduce + '(1 2 4))))
(assert '(equal? 8 (@reduce * '(1 2 4))))

(assert '(equal? '(1 2) (@uniq '(1 1 1 2 1 1 1 2))))

(assert '(equal? 5 (@find odd? '(2 4 5 6))))

(assert '(@member? '(1 2 3) 1))
(assert '(@member? '(1 2 3) 2))
(assert '(@member? '(1 2 3) 3))
(assert '(not (@member? '(1 2 3) 4)))

(assert '(equal? '(1 9)
                 (@difference '(1 3 9 4) '(3 8 4))))

(assert '(equal? '(1 2 3 4 5 6)
                 (@qsort <= '(6 4 2 1 3 5))))
