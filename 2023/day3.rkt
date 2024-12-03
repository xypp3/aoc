#lang racket

; ? Do chains of numbers count or not for a tool
(define part-1-str "............
.467..114...
....*.......
...35..633..
.......#....
.617*.......
......+.58..
...592......
.......755..
....$.*.....
..664.598...
............
")

(define (make-rectangle num-pos-list first?)
  (cond ((null? (cdr num-pos-list))
          (let ((y (cdar num-pos-list))
                (x (caar num-pos-list)))
              (list
                (cons x (- y 1))
                (cons x (+ 1 y))
                (cons (+ x 1) (- y 1))
                (cons (+ x 1) y)
                (cons (+ x 1) (+ 1 y)))))
        ((equal? first? #t)
          (cons
            (let ((y (cdar num-pos-list))
                  (x (caar num-pos-list)))
                (list
                  (cons (- x 1) (- y 1))
                  (cons (- x 1) y)
                  (cons (- x 1) (+ 1 y))
                  (cons x (- y 1))
                  (cons x (+ 1 y))))
            (make-rectangle (cdr num-pos-list) #f)))
        (else
          (cons
            (let ((y (cdar num-pos-list))
                  (x (caar num-pos-list)))
                (list
                  (cons x (- y 1))
                  (cons x (+ 1 y))))
            (make-rectangle (cdr num-pos-list) #f)))))

(regexp-split #rx"\n" part-1-str)

(define (make-2d row col-lst)
  (cond ((empty? col-lst) '())
        (else
          (cons
            (cons (car col-lst) row)
            (make-2d row (cdr col-lst))))))

(define test-lst (list (cons 1 2) (cons 3 4) (cons 9 0)))
test-lst
(make-rectangle test-lst #t)
(car '(1 2))
(make-2d 3 '(1 2 3))
