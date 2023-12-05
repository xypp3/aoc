#lang racket


(define str-1
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(define part-1-colors (list "red" "green" "blue"))
(define ruleset-1 (list (cons "red" 12) (cons "green" 13) (cons "blue" 14)))

(define (make-game line)
  (cons 
    (string->number (car (regexp-match #rx"[0-9]+" line)))
    (make-cube-set-list 
      (regexp-split 
        #rx";"
        (string-join (cdr (regexp-split #rx":" line)))))))

(define (make-cube-set-list lst)
  (cond ((empty? lst) '())
        (else
          (cons
            (make-cube-set (car lst) part-1-colors)
            (make-cube-set-list (cdr lst))))))

; this function adds up the same coloured cubes in one cube subset (i.e. a comma separated list that ends with a semi-colom in the challenge
(define (make-cube-set cubes colors)
  (cond ((empty? colors) '())
        (else
          (cons
            (cons 
              (car colors)
              (foldl + 0
                (map 
                  string->number 
                  (regexp-match* 
                    #rx"[0-9]+" 
                    (string-join (regexp-match* (string-append "[0-9]+ " (car colors)) cubes))))))
            (make-cube-set cubes (cdr colors))))))

(define (valid-game? cube-list ruleset)
  (cond ((empty? cube-list) #t)
        ((or
           (> (get-red-num (car cube-list)) (get-red-num ruleset))
           (> (get-green-num (car cube-list)) (get-green-num ruleset))
           (> (get-blue-num (car cube-list)) (get-blue-num ruleset))) #f)
        (else
          (valid-game? (cdr cube-list) ruleset))))




; make game for each line afterwards
;(make-game (car (regexp-split #rx"\n" str-1)))
;(define tmp-str "3 blue, 4 red 1 red, 2 green, 6 blue")
;(make-cube-set tmp-str part-1-colors)
;(define game-1 (make-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

(define (get-red-num lst) (cdar lst))
(define (get-green-num lst) (cdadr lst))
(define (get-blue-num lst)(cdaddr lst))

;(valid-game? (cdr game-1) ruleset-1)
(define (part-one lst)
  (cond ((empty? lst) '())
        ((string=? "" (car lst)) '())
        (else
          (cons (make-game (car lst))
                (part-one (cdr lst))))))


(define (my-filter lst)
  (cond ((empty? lst) '())
        ((not (valid-game? (cdr (car lst)) ruleset-1)) (my-filter (cdr lst)))
        (else
          (cons
            (caar lst)
            (my-filter (cdr lst))))))

(define game-list-test (part-one (regexp-split #rx"\n" str-1)))
(foldl + 0 (my-filter game-list-test))

(define game-list (part-one (regexp-split #rx"\n" (file->string "day2-data.txt"))))
(foldl + 0 (my-filter game-list))


; PART 2
; find min cube in each game
; get power set of each game
; sum all powersets

(define (get-all-of-one-color color-proc cube-set)
  (cond ((empty? cube-set) '())
        (else
          (cons
            (color-proc (car cube-set))
            (get-all-of-one-color color-proc (cdr cube-set))))))

;<https://stackoverflow.com/questions/27128960/getting-the-largest-number-in-a-list-in-scheme>
(define (getlargest lst)
  (foldl (lambda (e r) (if (or (not r) (> e r)) e r))
         #f
         lst))

; testing get-all-of-one-color
#| (getlargest (get-all-of-one-color get-red-num (cdar game-list-test))) |#
#| (getlargest (get-all-of-one-color get-green-num (cdar game-list-test))) |#
#| (getlargest (get-all-of-one-color get-blue-num (cdar game-list-test))) |#

(define (part-two lst)
  (cond ((empty? lst) 0)
        (else
          (+
            (*
              (getlargest (get-all-of-one-color get-red-num (cdar lst)))
              (getlargest (get-all-of-one-color get-green-num (cdar lst)))
              (getlargest (get-all-of-one-color get-blue-num (cdar lst))))
            (part-two (cdr lst))))))

(part-two game-list-test)
(part-two game-list)
