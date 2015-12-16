; define
; cond
; lambda
; quote

; eq?
; cons
; car
; cdr
; atom?
; apply

(define @eq? eq?)
(define @cons cons)
(define @car car)
(define @cdr cdr)
(define @atom? atom?)
(define @apply apply)

(define @null '())
(define @t (@eq? @null @null))
(define @f (@eq? @null @t))

(define (@cddr lst)
  (@cdr (@cdr lst)))

(define (@cadr lst)
  (@car (@cdr lst)))

(define (@null? val)
  (@eq? val @null))

(define (@list? val)
  (cond
    ((@null? val) @t)
    ((@atom? val) @f)
    (@t (@cdr val))))

(define (@list . args)
  (if (@null? args)
      @null
      (@cons
        (@car args)
        (@apply @list (@cdr args)))))

(define (@reverse-recur lst accu)
  (if (@null? lst)
      accu
      (@reverse-recur (@cdr lst)
                      (@cons (@car lst) accu))))

(define (@reverse lst)
  (@reverse-recur lst @null))

(define (@append-recur l1-reversed accu)
  (if (@null? l1-reversed)
      accu
      (@append-recur (@cdr l1-reversed)
                     (@cons (@car l1-reversed) accu))))

(define (@append2 l1 l2)
  (@append-recur (@reverse l1) l2))

(define (@append . lsts)
  (cond
    ((@null? lsts) lsts)
    ((@null? (@cdr lsts)) (@car lsts))
    (@t (@apply @append
                (@cons (@append2 (@car lsts) (@cadr lsts))
                       (@cddr lsts))))))

(define (@not val)
  (if val @f @t))

(define (@and x . args)
  (cond
    ((@null? args) x)
    ((@not x) @f)
    (@t (@apply @and args))))

(define (@or x . args)
  (cond
    (x x)
    ((@null? args) @f)
    (@t (@apply @or args))))

(define (@map-recur func lst accu)
  (if (@null? lst)
      (@reverse accu)
      (@map-recur func
                  (@cdr lst)
                  (@cons (func (@car lst)) accu))))

(define (@map func lst)
  (@map-recur func lst @null))

(define (@filter-recur func lst accu)
  (if (@null? lst)
      (@reverse accu)
      (@filter-recur func
                     (@cdr lst)
                     (if (func (@car lst))
                         (@cons (@car lst) accu)
                         accu))))

(define (@filter func lst)
  (@filter-recur func lst @null))

(define (@flatten lst)
  (cond
    ((@null? lst) lst)
    ((@list? (@car lst))
     (@append (@flatten (@car lst)) (@flatten (@cdr lst))))
    (@t (@cons (@car lst) (@flatten (@cdr lst))))))

(define (@reduce-recur func lst accu)
  (if (@null? lst)
      accu
      (@reduce-recur func (@cdr lst) (func accu (@car lst)))))

(define (@reduce func lst)
  (@reduce-recur func (@cdr lst) (@car lst)))

(define (@uniq-recur lst accu)
  (cond
    ((@null? lst) (@reverse accu))
    ((@member? accu (@car lst))
      (@uniq-recur (@cdr lst) accu))
    (@t (@uniq-recur (@cdr lst) (@cons (@car lst) accu)))))

(define (@uniq lst)
  (@uniq-recur lst @null))

(define (@find func lst)
  (cond
    ((@null? lst) @f)
    ((func (@car lst)) (@car lst))
    (@t (@find func (@cdr lst)))))

(define (@member? lst needle)
  (@find (lambda (x) (eqv? x needle)) lst))

(define (@difference-recur l1 l2 accu)
  (cond
    ((@null? l1) (@reverse accu))
    ((@member? l2 (@car l1))
      (@difference-recur (@cdr l1) l2 accu))
    (@t (@difference-recur (@cdr l1) l2 (@cons (@car l1) accu)))))

(define (@difference l1 l2)
  (@difference-recur l1 l2 @null))

(define (@qsort <= lst)
  (cond
    ((@null? lst) lst)
    ((@null? (@cdr lst)) lst)
    ((@null? (@cddr lst))
      (if (<= (@car lst) (@cadr lst))
          lst
          (@reverse lst)))
    (@t (@append
          (@qsort <= (@filter
                       (lambda (x) (<= x (@car lst)))
                       (@cdr lst)))
          (@list (@car lst))
          (@qsort <= (@filter
                       (lambda (x) (@not (<= x (@car lst))))
                       (@cdr lst)))))))
