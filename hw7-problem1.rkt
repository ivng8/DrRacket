;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw7-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This problem asks you to design several functions that employ the
; following data definitions. The functions that you design *must* use
; list abstraction(s) when appropriate; you MAY NOT use recursion: doing
; so will lead you to get no code credit for the function :(
;
; NOTE #1: Part of the credit for each problem will be based on the choice
; of list abstractions, so make sure that they are a good match for the
; problem.
;
; NOTE #2: For certain problems, you will have to design helper functions
; that do not use list abstractions. You should follow the full design
; recipe (including appropriate use of templates) for all problems. Be sure
; to do this, even if it feels a bit tedious - listen to your templates!!
;
; Data Definitions (do not modify these)


; A Weekday is one of:
; - "Monday"
; - "Tuesday"
; - "Wednesday"
; - "Thursday"
; - "Friday"
; Interpretation: a day that excludes the weekend

(define WEEKDAY-M "Monday")
(define WEEKDAY-T "Tuesday")
(define WEEKDAY-W "Wednesday")
(define WEEKDAY-R "Thursday")
(define WEEKDAY-F "Friday")

(define (weekday-temp w)
  (...
   (cond
     [(string=? w WEEKDAY-M) ...]
     [(string=? w WEEKDAY-T) ...]
     [(string=? w WEEKDAY-W) ...]
     [(string=? w WEEKDAY-R) ...]
     [(string=? w WEEKDAY-F) ...])))


(define-struct meeting [day bname rnum hstart mstart duration])

; A ClassMeeting is a (make-meeting Weekday String String PosInt[8, 18] NonNegInt[0, 59] PosInt)
; Interpretation: when a class is scheduled to meet weekly
; - day: which day of the week
; - bname: name of the building
; - rnum: room number
; - hstart: starting hour (24hr)
; - mstart: starting minute
; - duration: length of the class (in minutes)

(define CM-FUNDIES-M (make-meeting WEEKDAY-M "WVH" "210A" 10 30 65))
(define CM-FUNDIES-W (make-meeting WEEKDAY-W "WVH" "210A" 10 30 65))
(define CM-FUNDIES-R (make-meeting WEEKDAY-R "WVH" "210A" 10 30 65))
;
(define CM-FUNDIES-LAB (make-meeting WEEKDAY-T "WVH" "212" 8 0 100))
;
(define CM-DISCRETE-T (make-meeting WEEKDAY-T "ISEC" "102" 13 35 100))
(define CM-DISCRETE-F (make-meeting WEEKDAY-F "ISEC" "102" 13 35 100))
;
(define CM-DISCRETE-SEM (make-meeting WEEKDAY-W "Hastings" "110" 16 35 65))
;
(define CM-CREATURES-T (make-meeting WEEKDAY-T "Forbidden Forest" "Hut" 13 0 200))
(define CM-POTIONS-R (make-meeting WEEKDAY-R "Hogwarts" "Dungeon" 13 0 200))

(define (classmeeting-temp cm)
  (... (weekday-temp (meeting-day cm)) ...
       (meeting-bname cm) ...
       (meeting-rnum cm) ...
       (meeting-hstart cm) ...
       (meeting-mstart cm) ...
       (meeting-duration cm) ...))


(define-struct course [prefix num name prof meetings])

; A Course is a (make-course String String String String [List-of ClassMeeting])
; Interpretation: a weekly class
; - prefix: the course prefix
; - num: the course number
; - name: the course name
; - prof: name of the professor
; - meetings: weekly meeting times

(define COURSE-EASY-A
  (make-course "SCHED" "101" "Easy A" "Lazy"
               '()))

(define COURSE-FUNDIES-LECTURE
  (make-course "CS" "2500" "Fundies" "Howdy"
               (list CM-FUNDIES-M CM-FUNDIES-W CM-FUNDIES-R)))

(define COURSE-FUNDIES-LAB
  (make-course "CS" "2501" "Fundies Lab" "Awesome TAs"
               (list CM-FUNDIES-LAB)))

(define COURSE-DISCRETE-LECTURE
  (make-course "CS" "1800" "Discrete Structures" "Dr Strange"
               (list CM-DISCRETE-T CM-DISCRETE-F)))

(define COURSE-DISCRETE-SEM
  (make-course "CS" "1802" "Seminar for CS 1800" "Park"
               (list CM-DISCRETE-SEM)))

(define COURSE-CREATURES
  (make-course "HPTR" "2000" "Care of Magical Creatures" "Hagrid"
               (list CM-CREATURES-T)))

(define COURSE-POTIONS
  (make-course "HPTR" "2650" "Potions" "Snape"
               (list CM-POTIONS-R)))

(define (course-temp c)
  (... (course-prefix c) ...
       (course-num c) ...
       (course-name c) ...
       (course-prof c) ...
       (locm-temp (course-meetings c)) ...))


; A CourseSchedule is a [List-of Course]
; Interpretation: a list of weekly courses!

(define SCHEDULE-OOPS '())

(define SCHEDULE-KHOURY
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-DISCRETE-LECTURE
        COURSE-DISCRETE-SEM))

(define SCHEDULE-MAGIC
  (list COURSE-CREATURES
        COURSE-POTIONS))

(define SCHEDULE-CS+MAGIC
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-CREATURES
        COURSE-POTIONS))


; TODO 1/8: Part of healthy course scheduling is making sure to build in time for
;           food, and so you are to design the function lunch-course that produces
;           a "Lunch" course!
;
;           The function should take in a prefix & number (e.g., "FOOD" "101"),
;           a name & professor (e.g., "Exciting Baking" with "Alderton"), as
;           well as a list of weekdays. The function will then makes sure that
;           déjeuner occurs on all of those days at noon (for one hour) in a
;           single location of your choice (e.g., Hogwarts Great Hall).
;
;           Note: make sure to test your function on at least two sets of inputs!

; lunch-course : String String String String [List-of Weekday]) -> Course
; Creates a "Lunch" course
; using an inputted prefix, number, name, and professor
; and a list of weekdays

(check-expect (lunch-course
               "FOOD"
               "101"
               "Exciting Baking"
               "Alderton"
               (list))
              (make-course "FOOD" "101" "Exciting Baking" "Alderton"
                           '()))
(check-expect (lunch-course
               "FOODS"
               "101"
               "Exciting Baking"
               "Alderton"
               (list WEEKDAY-M))
              (make-course "FOODS" "101" "Exciting Baking" "Alderton"
                           (list (make-meeting WEEKDAY-M "Devon House" "126A" 12 0 60))))
(check-expect (lunch-course
               "FOOD"
               "105"
               "Exciting Baking"
               "Alderton"
               (list WEEKDAY-M WEEKDAY-T))
              (make-course "FOOD" "105" "Exciting Baking" "Alderton"
                           (list (make-meeting WEEKDAY-M "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-T "Devon House" "126A" 12 0 60))))
(check-expect (lunch-course
               "FOOD"
               "101"
               "Cooking"
               "Alderton"
               (list WEEKDAY-M WEEKDAY-T WEEKDAY-W))
              (make-course "FOOD" "101" "Cooking" "Alderton"
                           (list (make-meeting WEEKDAY-M "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-T "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-W "Devon House" "126A" 12 0 60))))
(check-expect (lunch-course
               "FOOD"
               "101"
               "Exciting Baking"
               "Pancake"
               (list WEEKDAY-M WEEKDAY-T WEEKDAY-W WEEKDAY-R))
              (make-course "FOOD" "101" "Exciting Baking" "Pancake"
                           (list (make-meeting WEEKDAY-M "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-T "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-W "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-R "Devon House" "126A" 12 0 60))))
(check-expect (lunch-course
               "FOOD"
               "101"
               "Exciting Baking"
               "Alderton"
               (list WEEKDAY-M WEEKDAY-T WEEKDAY-W WEEKDAY-R WEEKDAY-F))
              (make-course "FOOD" "101" "Exciting Baking" "Alderton"
                           (list (make-meeting WEEKDAY-M "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-T "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-W "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-R "Devon House" "126A" 12 0 60)
                                 (make-meeting WEEKDAY-F "Devon House" "126A" 12 0 60))))

(define (lunch-course prefix num name prof low)
  (make-course
   prefix
   num
   name
   prof
   (map lunch-meeting low)))

; lunch-meeting : Weekday -> ClassMeeting
; takes a weekday
; and creates a meeting
; with the weekday for an hour at noon
; located at Devon House Room 126A

(check-expect (lunch-meeting WEEKDAY-M)
              (make-meeting WEEKDAY-M "Devon House" "126A" 12 0 60))
(check-expect (lunch-meeting WEEKDAY-T)
              (make-meeting WEEKDAY-T "Devon House" "126A" 12 0 60))
(check-expect (lunch-meeting WEEKDAY-W)
              (make-meeting WEEKDAY-W "Devon House" "126A" 12 0 60))
(check-expect (lunch-meeting WEEKDAY-R)
              (make-meeting WEEKDAY-R "Devon House" "126A" 12 0 60))
(check-expect (lunch-meeting WEEKDAY-F)
              (make-meeting WEEKDAY-F "Devon House" "126A" 12 0 60))

(define (lunch-meeting w)
  (make-meeting w "Devon House" "126A" 12 0 60))

; TODO 2/8: Design the function long-weekend? that determines if a
;           course schedule avoids all classes on Mondays & Fridays.
;           In the examples above, this is true of OOPS and MAGIC.
;           Note: make sure to follow all the templates and
;           sufficiently test all your functions!

; long-weekend? : CourseSchedule -> Boolean
; determines if a course schedule avoids classes
; on Mondays and Fridays

(check-expect (long-weekend? SCHEDULE-OOPS) #t)
(check-expect (long-weekend? SCHEDULE-KHOURY) #f)
(check-expect (long-weekend? SCHEDULE-MAGIC) #t)
(check-expect (long-weekend? SCHEDULE-CS+MAGIC) #f)

(define (long-weekend? cs)
  (andmap mf? cs))

; mf? : Course -> Boolean
; takes inputted course and returns false
; if it detects monday or friday in
; the class meetings of the course

(check-expect (mf? COURSE-EASY-A) #t)
(check-expect (mf? COURSE-FUNDIES-LECTURE) #f)
(check-expect (mf? COURSE-FUNDIES-LAB) #t)
(check-expect (mf? COURSE-DISCRETE-LECTURE) #f)
(check-expect (mf? COURSE-DISCRETE-SEM) #t)
(check-expect (mf? COURSE-CREATURES) #t)
(check-expect (mf? COURSE-POTIONS) #t)

(define (mf? c)
  (andmap meeting-mf (course-meetings c)))

; meeting-mf : ClassMeeting -> Boolean
; takes a class meeting and returns false
; if it takes place on monday or friday

(check-expect (meeting-mf CM-FUNDIES-M) #f)
(check-expect (meeting-mf CM-FUNDIES-W) #t)
(check-expect (meeting-mf CM-FUNDIES-R) #t)
(check-expect (meeting-mf CM-FUNDIES-LAB) #t)
(check-expect (meeting-mf CM-DISCRETE-T) #t)
(check-expect (meeting-mf CM-DISCRETE-F) #f)
(check-expect (meeting-mf CM-DISCRETE-SEM) #t)
(check-expect (meeting-mf CM-CREATURES-T) #t)
(check-expect (meeting-mf CM-POTIONS-R) #t)

(define (meeting-mf m)
  (not
   (or
    (string=? (meeting-day m) WEEKDAY-M)
    (string=? (meeting-day m) WEEKDAY-F))))
          

; TODO 3/8: Design the function only-khoury that takes a course schedule
;           and produces a new schedule only containing classes that
;           have the prefix "CS", "DS", or "CY". So supplying OOPS and
;           KHOURY would result in unaffected schedules, but MAGIC would
;           result in an empty schedule and CS+MAGIC would result in a
;           schedule with only Fundies :)
;
;           Note: since we didn't include any DS/CY courses in the
;           examples, make may need to create example courses to properly
;           test your helper function(s)! Some course suggestions include
;           DS2000 (Programming with Data) and CY2550 (Foundations of
;           Cybersecurity).

(define COURSE-DS
  (make-course "DS" "2000" "Programming with Data" "Snape"
               (list CM-POTIONS-R)))
(define COURSE-CY
  (make-course "CY" "2550" "Foundations" "Cyber"
               (list CM-POTIONS-R)))

; only-khoury : CourseSchedule -> CourseSchedule
; takes a course schedule and produces a new
; schedule only containing classes that have the
; prefix "CS", "DS", or "CY"

(check-expect (only-khoury SCHEDULE-OOPS) SCHEDULE-OOPS)
(check-expect (only-khoury SCHEDULE-KHOURY) SCHEDULE-KHOURY)
(check-expect (only-khoury SCHEDULE-MAGIC) '())
(check-expect (only-khoury SCHEDULE-CS+MAGIC)
              (list COURSE-FUNDIES-LECTURE
                    COURSE-FUNDIES-LAB))
(check-expect (only-khoury (list COURSE-CREATURES COURSE-DS))
              (list COURSE-DS))
(check-expect (only-khoury (list COURSE-CREATURES COURSE-CY))
              (list COURSE-CY))

(define (only-khoury cs)
  (filter prefix? cs))

; prefix? : Course -> Boolean
; takes a course and sees if it has
; prefix "CS", "DS", or "CY"

(check-expect (prefix? COURSE-EASY-A) #f)
(check-expect (prefix? COURSE-FUNDIES-LECTURE) #t)
(check-expect (prefix? COURSE-FUNDIES-LAB) #t)
(check-expect (prefix? COURSE-DISCRETE-LECTURE) #t)
(check-expect (prefix? COURSE-DISCRETE-SEM) #t)
(check-expect (prefix? COURSE-CREATURES) #f)
(check-expect (prefix? COURSE-POTIONS) #f)
(check-expect (prefix? COURSE-DS) #t)
(check-expect (prefix? COURSE-CY) #t)

(define (prefix? c)
  (cond
    [(string=? (course-prefix c) "CS") #t]
    [(string=? (course-prefix c) "DS") #t]
    [(string=? (course-prefix c) "CY") #t]
    [else #f]))

; TODO 4/8: Design the function time-in-class that calculates total
;           time spent in class (in minutes each week) for a supplied
;           course schedule. For example, OOPS requires 0 minutes and
;           KHOURY is 560.

; time-in-class : CourseSchedule -> PosInt
; calculates the total time spent in class
; for a supplied course schedule
 
(check-expect (time-in-class SCHEDULE-OOPS) 0)
(check-expect (time-in-class SCHEDULE-KHOURY) 560)
(check-expect (time-in-class SCHEDULE-MAGIC) 400)
(check-expect (time-in-class SCHEDULE-CS+MAGIC) 695)

(define (time-in-class cs)
  (foldr + 0 (map time-per-course cs)))

; time-per-course : Course -> PosInt
; calculates the time spent in a course

(check-expect (time-per-course COURSE-EASY-A) 0)
(check-expect (time-per-course COURSE-FUNDIES-LECTURE) 195)
(check-expect (time-per-course COURSE-FUNDIES-LAB) 100)
(check-expect (time-per-course COURSE-DISCRETE-LECTURE) 200)
(check-expect (time-per-course COURSE-DISCRETE-SEM) 65)
(check-expect (time-per-course COURSE-CREATURES) 200)
(check-expect (time-per-course COURSE-POTIONS) 200)

(define (time-per-course c)
  (foldr + 0 (map meeting-duration (course-meetings c))))

; TODO 5/8: Design the function bring-water? that takes a course schedule
;           and determines if any course has even a single meeting that
;           lasts for longer than two hours. For example, this is true
;           for either of the magic schedules, but none of the others.

; bring-water? : CourseSchedule -> Boolean
; determines if any course in a schedule has
; even a single meeting that
; lasts for longer than two hours

(check-expect (bring-water? SCHEDULE-OOPS) #f)
(check-expect (bring-water? SCHEDULE-KHOURY) #f)
(check-expect (bring-water? SCHEDULE-MAGIC) #t)
(check-expect (bring-water? SCHEDULE-CS+MAGIC) #t)

(define (bring-water? cs)
  (ormap >120? cs))

; <120? : Course -> Boolean
; determines if the course has a meeting
; that lasts for longer than two hours

(check-expect (>120? COURSE-EASY-A) #f)
(check-expect (>120? COURSE-FUNDIES-LECTURE) #f)
(check-expect (>120? COURSE-FUNDIES-LAB) #f)
(check-expect (>120? COURSE-DISCRETE-LECTURE) #f)
(check-expect (>120? COURSE-DISCRETE-SEM) #f)
(check-expect (>120? COURSE-CREATURES) #t)
(check-expect (>120? COURSE-POTIONS) #t)

(define (>120? c)
  (ormap meeting-120? (course-meetings c)))

; meeting-120? : ClassMeeting -> Boolean
; determines if the meeting
; lasts longer than two hours

(check-expect (meeting-120? CM-FUNDIES-M) #f)
(check-expect (meeting-120? CM-FUNDIES-W) #f)
(check-expect (meeting-120? CM-FUNDIES-R) #f)
(check-expect (meeting-120? CM-FUNDIES-LAB) #f)
(check-expect (meeting-120? CM-DISCRETE-T) #f)
(check-expect (meeting-120? CM-DISCRETE-F) #f)
(check-expect (meeting-120? CM-DISCRETE-SEM) #f)
(check-expect (meeting-120? CM-CREATURES-T) #t)
(check-expect (meeting-120? CM-POTIONS-R) #t)

(define (meeting-120? cm)
  (> (meeting-duration cm) 120))

; TODO 6/8: Design the function course->days-abbrev that takes a course
;           and produces a single string that has abbreviations of all
;           days of the week that course meets. For instance, Fundies
;           lecture would produce "MWR", Fundies lab would produce "T",
;           Discrete lecture would be "TF", and the "easy A" class would
;           produce "" (since the lazy prof never wants to meet!).

; course->days-abbrev : Course -> String
; takes a course and produces a single
; string that has abbreviations of all
; days of the week that course meets

(check-expect (course->days-abbrev COURSE-EASY-A) "")
(check-expect (course->days-abbrev COURSE-FUNDIES-LECTURE) "MWR")
(check-expect (course->days-abbrev COURSE-FUNDIES-LAB) "T")
(check-expect (course->days-abbrev COURSE-DISCRETE-LECTURE) "TF")
(check-expect (course->days-abbrev COURSE-DISCRETE-SEM) "W")
(check-expect (course->days-abbrev COURSE-CREATURES) "T")
(check-expect (course->days-abbrev COURSE-POTIONS) "R")

(define (course->days-abbrev c)
  (foldr string-append "" (map day-letter (course-meetings c))))

; letter-collapse : ClassMeeting -> String
; takes the weekday that the class meeting
; occurs on and gives the string that represents the day

(check-expect (day-letter CM-FUNDIES-M) "M")
(check-expect (day-letter CM-FUNDIES-W) "W")
(check-expect (day-letter CM-FUNDIES-R) "R")
(check-expect (day-letter CM-FUNDIES-LAB) "T")
(check-expect (day-letter CM-DISCRETE-T) "T")
(check-expect (day-letter CM-DISCRETE-F) "F")
(check-expect (day-letter CM-DISCRETE-SEM) "W")
(check-expect (day-letter CM-CREATURES-T) "T")
(check-expect (day-letter CM-POTIONS-R) "R")

(define (day-letter cm)
  (cond
    [(string=? (meeting-day cm) WEEKDAY-M) "M"]
    [(string=? (meeting-day cm) WEEKDAY-T) "T"]
    [(string=? (meeting-day cm) WEEKDAY-W) "W"]
    [(string=? (meeting-day cm) WEEKDAY-R) "R"]
    [(string=? (meeting-day cm) WEEKDAY-F) "F"]))

; TODO 7/8: Design the functions stack/h and stack/v, to stack a supplied
;           list of images horizontally and vertically, with a bit of buffer
;           between each image (see the GAP we've defined for you). You have
;           been supplied tests for clarity.

(define GAP (square 5 "solid" "white"))

; stack/h : [List-of Image] -> Image
; takes a list of images and stacks them horizontally
; into another image

(check-expect
 (stack/h '())
 GAP)

(check-expect
 (stack/h
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/h loi)
  (foldr beside GAP (map add-gap-beside loi)))

; add-gap-beside : Image -> Image
; adds a gap on the left of the image

(check-expect (add-gap-beside (text "A" 5 "black"))
              (beside GAP (text "A" 5 "black")))
(check-expect (add-gap-beside (text "B" 10 "black"))
              (beside GAP (text "B" 10 "black")))
(check-expect (add-gap-beside (text "C" 50 "black"))
              (beside GAP (text "C" 50 "black")))

(define (add-gap-beside i)
  (beside GAP i))

; stack/v : [List-of Image] -> Image
; takes a list of images and stacks them vertically
; into another image

(check-expect
 (stack/v '())
 GAP)

(check-expect
 (stack/v
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/v loi)
  (foldr above GAP (map add-gap-above loi)))

; add-gap-above : Image -> Image
; adds a gap on the left of the image

(check-expect (add-gap-above (text "A" 5 "black"))
              (above GAP (text "A" 5 "black")))
(check-expect (add-gap-above (text "B" 10 "black"))
              (above GAP (text "B" 10 "black")))
(check-expect (add-gap-above (text "C" 50 "black"))
              (above GAP (text "C" 50 "black")))

(define (add-gap-above i)
  (above GAP i))

; TODO 8/8: Now using your solutions to the previous two parts, design the
;           function viz-schedule, which produces a visual representation
;           of a supplied course schedule, such that each course is a row
;           (with the prefix, num, name, prof, and day abbreviations) and
;           the rows are vertically stacked. You have been supplied tests
;           for clarity.

; viz-schedule : CourseSchedule -> Image
; creates a visual representation of a supplied course schedule
; in which each course is a row
; and the rows are stacked vertically stacked

(check-expect (viz-schedule SCHEDULE-OOPS) GAP)

(check-expect (viz-schedule SCHEDULE-KHOURY)
              (above GAP
                     (text "CS 2500 (Fundies, Howdy): MWR" 50 "black")
                     GAP
                     (text "CS 2501 (Fundies Lab, Awesome TAs): T" 50 "black")
                     GAP
                     (text "CS 1800 (Discrete Structures, Dr Strange): TF" 50 "black")
                     GAP
                     (text "CS 1802 (Seminar for CS 1800, Park): W" 50 "black")
                     GAP))

(define (viz-schedule cs)
  (foldr above GAP (map add-gap-above  (map 50-black cs))))

; 50-black : Course -> Image
; takes an inputted course and makes
; it an image of texxt with the prefix, num
; name, prof, and day abbreviation

(check-expect (50-black COURSE-EASY-A)
              (text "SCHED 101 (Easy A, Lazy): " 50 "black"))
(check-expect (50-black COURSE-FUNDIES-LECTURE)
              (text "CS 2500 (Fundies, Howdy): MWR" 50 "black"))
(check-expect (50-black COURSE-FUNDIES-LAB)
              (text "CS 2501 (Fundies Lab, Awesome TAs): T" 50 "black"))
(check-expect (50-black COURSE-DISCRETE-LECTURE)
              (text "CS 1800 (Discrete Structures, Dr Strange): TF" 50 "black"))
(check-expect (50-black COURSE-DISCRETE-SEM)
              (text "CS 1802 (Seminar for CS 1800, Park): W" 50 "black"))
(check-expect (50-black COURSE-CREATURES)
              (text "HPTR 2000 (Care of Magical Creatures, Hagrid): T" 50 "black"))
(check-expect (50-black COURSE-POTIONS)
              (text "HPTR 2650 (Potions, Snape): R" 50 "black"))

(define (50-black c)
  (text
   (string-append
    (course-prefix c) " "
    (course-num c) " ("
    (course-name c) ", "
    (course-prof c) "): "
    (course->days-abbrev c))
   50
   "black"))
