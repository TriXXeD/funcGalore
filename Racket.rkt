#lang scheme

;Day and Date validation

(define (leap-year? year)
  (cond ((eq? (modulo year 400) 0) #t)
        ((eq? (modulo year 100) 0) #f)
        ((eq? (modulo year 4) 0) #t)
        (else #f)))

;needs helpers
(define (day year month date)
  (day-name (modulo 
             (+ (quotient (modulo year 100) 4)
                (+ (modulo year 100)
                (+ date
                   (+ (month-table month year)
                      (century-table (find-century year))
                      ))))
             7)))

;helpers
(define (month-table month year)
  (cond [(leap-year? year) (month-table-leap month)]
        [else (month-table-nonleap month)]))
 
(define (month-table-leap month)
  (cond [(ormap (lambda (x) (equal? month x))
                '("JanuaryNon" "October" 10)) 0]
        [(ormap (lambda (x) (equal? month x))
                '("August" 8 "February" 2)) 2]
        [(ormap (lambda (x) (equal? month x))
                '("March" 3 "FebruaryNon" "November" 11)) 3]
        [(ormap (lambda (x) (equal? month x))
                '("September" 9 "December" 12)) 5]
        [(ormap (lambda (x) (equal? month x))
                '("June" 6)) 4]
        [(ormap (lambda (x) (equal? month x))
                '("May" 5)) 1]
        [(ormap (lambda (x) (equal? month x))
                '("January" 1 "April" 4)) 6]
        [else (error "not a month")]))

(define (month-table-nonleap month)
  (cond [(ormap (lambda (x) (equal? month x))
                '("January" 1 "October" 10)) 0]
        [(ormap (lambda (x) (equal? month x))
                '("August" 8)) 2]
        [(ormap (lambda (x) (equal? month x))
                '("March" 3 "February" 2 "November" 11)) 3]
        [(ormap (lambda (x) (equal? month x))
                '("September" 9 "December" 12)) 5]
        [(ormap (lambda (x) (equal? month x))
                '("June" 6)) 4]
        [(ormap (lambda (x) (equal? month x))
                '("May" 5)) 1]
        [(ormap (lambda (x) (equal? month x))
                '("April" 4)) 6]
        [else (error "not a month")]))
                                  
(define (century-table century)
  (modulo century 4))

(define (find-century year)
  (quotient (modulo year 1000) 100))
                                                 
(define (day-name day)
  (list-ref '("Sat" "Sun" "Mon" "Tue" "Wed" "Thu" "Fri") day))

;SanityTests
(quotient (modulo 2016 1000) 100)
(day-name 0)
(day-name 6)
(leap-year? 2000)
(leap-year? 100)
(month-table "October" 2005)
(find-century 1400)
(century-table (find-century 2000))
(day 2000 "January" 1)
(day 2000 1 1)
(day 2016 "October" 28)
(day 2016 "September" 1)
(day 2016 "November" 1)
(day 2016 "December" 1)