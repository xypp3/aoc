#lang racket

(define str-1 
"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(define (split_str str)
  (regexp-split #rx"\n" str))

(define (last-element lst)
  (if (null? (cdr lst))
    (car lst)
    (last-element (cdr lst))))

(define (part-one prev lst index)
  (cond ((= 0 (length lst)) prev)
        ((string=? (car lst) "") (part-one prev (cdr lst) index))
        ((= 0 (modulo index 2))
         (part-one
           (+
             prev
             (* 10 (string->number (car (regexp-match* #rx"[0-9]" (car lst))))))
           lst
           (+ 1 index)))
        ((= 1 (modulo index 2))
         (part-one
           (+
             prev
             (string->number (last-element (regexp-match* #rx"[0-9]" (car lst)))))
           (cdr lst)
           (+ 1 index)))))

(part-one 0 (regexp-split #rx"\n" str-1) 0)
(part-one 0 (regexp-split #rx"\n" (file->string "day1-data.txt")) 0)

(define str-2
"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(define regstr #rx"[0-9]|zero|one|two|three|four|five|six|seven|eight|nine")
(define (word-to-num word)
  (cond ((string=? "one" word) 1)
        ((string=? "two" word) 2)
        ((string=? "three" word) 3)
        ((string=? "four" word) 4)
        ((string=? "five" word) 5)
        ((string=? "six" word) 6)
        ((string=? "seven" word) 7)
        ((string=? "eight" word) 8)
        ((string=? "nine" word) 9)
        (else (string->number word))))

(define regstr-last #rx"[0-9]|eightwo|zerone|twone|oneight|threeight|fiveight|sevenine|zero|one|two|three|four|five|six|seven|eight|nine")
(define (word-to-num-last word)
  (cond ((string=? "one" word) 1)
        ((string=? "two" word) 2)
        ((string=? "three" word) 3)
        ((string=? "four" word) 4)
        ((string=? "five" word) 5)
        ((string=? "six" word) 6)
        ((string=? "seven" word) 7)
        ((string=? "eight" word) 8)
        ((string=? "nine" word) 9)
        ((string=? "eightwo" word) 2)
        ((string=? "zerone" word) 1)
        ((string=? "twone" word) 1)
        ((string=? "oneight" word) 8)
        ((string=? "threeight" word) 8)
        ((string=? "fiveight" word) 8)
        (else (string->number word))))


(define (part-two prev lst index)
  (cond ((= 0 (length lst)) prev)
        ((string=? (car lst) "") (part-two prev (cdr lst) index))
        ((= 0 (modulo index 2))
         (part-two
           (+
             prev
             (* 10 (word-to-num (car (regexp-match* regstr (car lst))))))
           lst
           (+ 1 index)))
        ((= 1 (modulo index 2))
         (part-two
           (+
             prev
             (word-to-num-last (last-element (regexp-match* regstr-last (car lst)))))
           (cdr lst)
           (+ 1 index)))))


(part-two 0 (regexp-split #rx"\n" str-2) 0)
(part-two 0 (regexp-split #rx"\n" str-1) 0)
(part-two 0 (regexp-split #rx"\n" (file->string "day1-data.txt")) 0)
