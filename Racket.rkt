#lang scheme
(require racket/trace)
;Cal Layout
;	Calender:
;	'(
;		<Name>
;		<Descrip>
;		'(
;				<Appointment>*
;		 )
;		'(
;				<Cal>*
;		 )
;	 )
;
;	App:
;	'(
;                <descri>
;                <timeframe>
;			
;	 )
;
;
;	Timeframe:
;	'(
;			<date> (FROM)
;			<date> (TO)
;	 )
;
;       Date:
;       '(
;			<Year>
;			<Month>
;			<Day>
;			<Hour>
;			<Minute>
;	 )
;Calender

(define (cal1)
  (build-cal "Some Calender Name" "Some Calender Desc"
             (list (build-appointment "Some Appointment Descript"
                                (build-timeframe
                                   (build-datetime 2016 10 28 13 37)
                                   (build-datetime 2016 10 28 16 30)))
                   (build-appointment "Some Second Appointment"
                                (build-timeframe
                                   (build-datetime 2016 10 28 17 00)
                                   (build-datetime 2016 10 30 12 00)))
                   )
             (list (build-cal "Some Other Calender" "A calender within a calender"
                        (list (build-appointment "Some secret appointment"
                                           (build-timeframe
                                               (build-datetime 2001 9 11 11 37)
                                               (build-datetime 2001 9 11 13 37)))
                         )
                        )
             )
  )
)     

;required functionality
(define (appointments-overlap? ap1 ap2)
  (if (and (before? (from (timeframe ap1)) (to (timeframe ap2)))
           (before? (from (timeframe ap2)) (to (timeframe ap1))))
      #t
      #f))

(define (calendars-overlap? cal1 cal2)
  (with-calendar cal1
    (lambda (cal1)
      (with-calendar cal2
        (lambda (cal2)
          (let* ([apl1 (applist cal1)]
                 [apl2 (applist cal2)])
            (ormap 
               (lambda (cartplist)
                 (apply appointments-overlap? cartplist)
               )
               (cartesian-product apl1 apl2)
            )))))))
               
             ;  (with-appointment apl2
              ;   (ormap
               ;   (lambda (appoint2)
                ;    (appointments-overlap? appoint1 appoint2)
                 ;   )))))))))))

(define (flatten-calendar cal)
  (with-calendar cal (lambda (cal)
        (build-cal
         (cal-name cal)
         (cal-descript cal)
         (apply append
                (applist cal)
                (map (lambda (x)
                     (applist (flatten-calendar x)))
                (subcal cal))
         )
         ))))
                                
            
                                       

                        
   ;o,l,o,l,ao


;Builders
(define (build-datetime year month day hour minute)
  (if (datetime? (list year month day hour minute))
      (list year month day hour minute)
      (error "Please use numerals to describe ur datetime")))

(define (build-timeframe from-datetime to-datetime)
  (if (and (datetime? from-datetime)
           (datetime? to-datetime)
           (before? from-datetime to-datetime))                      
      (list from-datetime to-datetime)
      (error "timeframe build fail")))

(define (build-appointment descript timeframe)
  (if (and (string? descript)
           (timeframe? timeframe))
      (list descript timeframe)
      (error "appointment build fail")))

(define (build-cal title descript [appointments '()] [subcals '()])
  (if (and (string? title)
           (string? descript)
           (andmap appointment? appointments)
           (if (not (eq? subcals '())) (andmap calendar? subcals) #t))
      (list title descript appointments subcals)
      (error "cal build fail")))

;Data Constructs Validation
(define (with-calendar cal func)
  (if (calendar? cal)
	(func cal)
	(error "Not a calendar")
	)
)
                                         
(define (with-appointment appointment func)
  (if (appointment? appointment)
        (func appointment)
        (error "not an appointment")
      )
  )
                                         
(define (with-timeframe timeframe func)
  (if (timeframe? timeframe)
        (func timeframe)
        (error "not a timeframe")
      )
  )
                                         
(define (with-datetime datetime func)
  (if (datetime? datetime)
        (func datetime)
        (error "not a datetime")
      )
  )                                           
(define (datetime? datetime)
  (match datetime
    [(list year month day hour minute) (and (date? year month day)
                                            (time? hour minute))]
    [_ #f]))

(define (timeframe? timeframe)
  (match timeframe
          [(list from-datetime to-datetime) (and (datetime? from-datetime)
                                                 (datetime? to-datetime))]
          [_ #f]))

(define (appointment? appointment)
  (match appointment
          [(list ap-desc timeframe) (and (string? ap-desc)
                                         (timeframe? timeframe))]
          [_ #f]))

(define (calendar? cal)
  (match cal
    [(list cal-name cal-desc appoint cal) (and (string? cal-name)
                                               (string? cal-desc)
                                               (andmap appointment? appoint)
                                               (andmap calendar? cal))]
    [_ #f]))

(define (before? from to)
  (if ( <= (year from) (year to))
      ( <=
        (+ (day-to-min (day from))
           (+ (hour-to-min (hour from))
              (day-to-min (days-in-month (year from) (month from)))))  
        (+ (day-to-min (day to))
           (+ (hour-to-min (hour to))
              (day-to-min (days-in-month (year to) (month to)))))
        )
      #f))
;convert to minutes
(define (hour-to-min hour)
  (* hour 60))
(define (day-to-min day)
  (* day 1440))
;getters
(define (minute datetime)
  (with-datetime datetime (lambda (x) 
                            (list-ref x 4))))

(define (hour datetime)
  (with-datetime datetime (lambda (x)
                            (list-ref x 3))))

(define (day datetime)
  (with-datetime datetime (lambda (x)
                            (list-ref x 2))))

(define (month datetime)
  (with-datetime datetime (lambda (x)
                            (list-ref x 1))))

(define (year datetime)
  (with-datetime datetime (lambda (x)
                                  (list-ref x 0))))

(define (from timeframe)
  (with-timeframe timeframe (lambda (x)
                              (list-ref x 0))))

(define (to timeframe)
  (with-timeframe timeframe (lambda (x)
                              (list-ref x 1))))

(define (applist cal)
  (with-calendar cal (lambda (x)
                       (list-ref x 2))))

(define (timeframe appointment)
  (with-appointment appointment (lambda (x)
                                  (list-ref x 1))))

(define (app-descript appointment)
  (with-appointment appointment (lambda (x)
                                  (list-ref x 0))))

(define (cal-name cal)
  (with-calendar cal (lambda (x)
                       (list-ref x 0))))

(define (cal-descript cal)
  (with-calendar cal (lambda (x)
                       (list-ref x 1))))

(define (subcal cal)
  (with-calendar cal (lambda (x)
                       (list-ref x 3))))
;Validation helpers
(define (hour? hour)
  (and (>= hour 0) (<= hour 24)))

(define (minute? minute)
  (and (>= minute 0) (<= minute 60)))

(define (time? hour minute)
  (if (and (number? hour) (number? minute))
      (and (hour? hour) (minute? minute))
      #f
      ))
(define (month? month)
  (if (and (<= month 12) (> month 0)) #t #f))

(define (date? year month day)
  (if  (and (number? month)
            (eq? (month? month) #t)
            (number? year)
            (<= day (days-in-month year month))
            (> day 0))
       #t
       #f))



;Checks leap year rules
(define (leap-year? year)
  (cond ((eq? (modulo year 400) 0) #t)
        ((eq? (modulo year 100) 0) #f)
        ((eq? (modulo year 4) 0) #t)
        (else #f)))

;Uses a formula to determine which day a given date is,
;works on gregorian calender its use (approx 1750)
(define (find-day year month date)
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
  (list-ref '("Saturday" "Sunday" "Monday" "Tueday" "Wednesday" "Thursday" "Friday") day))

(define (days-in-month year month)
  (if (leap-year? year)
      (list-ref '("not a month" 31 29 31 30 31 30 31 31 30 31 30 31) month)
      (list-ref '("not a month" 31 28 31 30 31 30 31 31 30 31 30 31) month)))

;HTML Generation
;body, head, div, br, p, tb, ul, li, h1, h2, h3, ol

;SanityTests
(day-name 0)
(day-name 6)
(leap-year? 2000)
(leap-year? 100)
(month-table "October" 2005)
(find-century 1400)
(century-table (find-century 2000))
(find-day 2000 "January" 1)
(find-day 2000 1 1)
(find-day 2016 10 28)
(find-day 2016 9 1)
(find-day 2016 11 1)
(find-day 2016 12 1)
(hour? 20)
(hour? 25)
(days-in-month 2016 2)
(days-in-month 2015 2)
(time? 6 50)
(time? 's 66)
(time? 13 37)
(eq? (month? 2) #t)
(date? 2016 2 29)
(date? 2001 2 29)
(datetime? (list 2016 2 29 13 37))
(datetime? (list 2011 2 25 13 37))
(timeframe? (list (list 2016 2 29 13 37) (list 2011 2 25 13 37)))
;(build-datetime 2016 "october" 28 16 30)
(build-datetime 2016 10 28 16 30)
(before? (list 2016 2 29 13 37) (list 2011 2 25 13 37))
(before? (list 2016 2 25 13 37) (list 2016 2 25 13 37))
(cal1)
(applist (cal1))
(appointments-overlap? (list-ref (applist (cal1)) 0) (list-ref (applist (cal1)) 0))
(appointments-overlap? (list-ref (applist (cal1)) 1) (list-ref (applist (cal1)) 0))
(calendars-overlap? (cal1) (cal1))
(flatten-calendar (cal1))