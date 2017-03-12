(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)
(define (id obj) obj)

(define (flip func) (lambda (x y) (func y x)))

(define (curry func x)
  (lambda (y) (apply func (cons x (list y)))))

(define (compose f g)
  (lambda (x) (f (apply g x))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr func end lst)
  (if (null? lst)
    end
    (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func acc lst)
  (if (null? lst)
    acc
    (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred))))

(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))

(define (max first . rest)
  (fold (lambda (old new) (if (> old new) old new) first rest)))

(define (min first . rest)
  (fold (lambda (old new) (if (< old new) old new) first rest)))

(define (length lst)
  (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst)
  (fold (flip cons) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alst) (fold (mem-helper (curry eq? obj) car) #f lst))
(define (assv obj alst) (fold (mem-helper (curry eqv? obj) car) #f lst))
(define (assoc obj alst) (fold (mem-helper (curry equal? obj) id) #f lst))

(define (map f lst) (foldr (lambda (x y) (cons (f x) y)) '() lst))
(define (filter p lst) (foldr (lambda (x y) (if (p x) (cons x y) y)) '() lst))
