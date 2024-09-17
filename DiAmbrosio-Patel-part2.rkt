;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Patel-DiAmbrosio.part2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; The starter file will NOT interpret as-is
;; It provides test cases as additional documentation
;; Tests are not exhaustive. You should add more tests covering edge cases. 

; Data Definition

(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number (listof widget))
;; interp.
;;        name is the name of the item
;;        quantity are how many are in stock
;;        time is how long it takes to produce.  a value of 0 means this is a base component and
;;           cannot be made by our factory
;;        price is how much it sells for
;;        parts are the subwidgets needed to construct this item
;; ---------------------------------------------------------------------

;; here is one hierarchy of Widgets used to make a Telephone
(define Wire (make-widget "Wire" 3 0 5 empty))
(define Cord (make-widget "Cord" 7 0 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 0 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 11 0 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))

;; a separate hierarchy of Widgets used to make a Jewelry set
(define Glass (make-widget "Glass" 6 0 4 empty))
(define Beads (make-widget "Beads" 25 6 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 4 5 (list Beads)))
(define Chain (make-widget "Chain" 7 0 1 empty))
(define Pendant (make-widget "Pendant" 3 0 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3 (list Chain Pendant)))
(define Ring (make-widget "Ring" 15 0 10 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30 (list Ring Necklace Bracelet)))

;; test cases for Part 1
(check-expect (find-name-longer-than--widget Wire 3) (list Wire))
(check-expect (find-quantity-over--widget Wire 2) (list Wire))
(check-expect (find-cheaper-than--widget Wire 6) (list Wire))
(check-expect (find-hard-make--widget Jewelry 6 4)
              (list Jewelry Ring Pendant Bracelet Beads))
;;                                 ^^^ should be 0, not 5

;; test cases for Part 2
(check-expect (find-good-make--widget Jewelry 10 3)
              (list Ring Bracelet Beads Glass))

(check-expect (find-tough-advertise--widget Jewelry 6 2)
              (list Jewelry)) 

;; Question 1
;; Encapsulated mutual recursion template
#;
(define (fn-for-widget w)
  (local [(define (fn-fo-list-of-widget low)
            (cond [(empty? low) empty]
                  [else (append (find-name-longer-than--widget (first low)) (fn-fo-list-of-widget (rest low)))]))]
    (cond [(empty? (widget-parts w)) (... (widget-quantity w) (widget-time w) (widget-parts))])
    [else (fn-fo-list-of-widget (widget-parts w))]))

;; Question 2
;; Function definition for find-good-make--widget
;; find-good-make--widget: Widget Natural Number -> ListOfWidget
;; Purpose: returns all (sub)widgets that require less than the
;; specified Natural’s amount of time to make and whose price is more than the Number’s

;(define Wire (make-widget "Wire" 3 0 5 empty))
;(define Cord (make-widget "Cord" 7 0 5 (list Wire)))
;(define Numbers (make-widget "Numbers" 9 0 5 empty))
;(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
;(define Receiver (make-widget "Receiver" 11 0 7 empty))
;(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))

(check-expect (find-good-make--widget (make-widget "" 0 0 0 empty) 0 0) empty)
(check-expect (find-good-make--widget Telephone 7 4) (list Receiver Buttons Numbers Cord Wire))

;; (define (find-good-make--widget w 0 0) empty) ;the stub

(define (find-good-make--widget w n m)
  (filter--widget w (lambda (w) (and (< (widget-time w) n) (> (widget-price w) m)))))

;; Function definition for find-tough-advertise--widget 
;; find-tough-advertise--widget: Widget Natural Number -> ListOfWidget 
;; Purpose: returns all (sub)widgets whose name is longer than the 
;; first Natural and whose number of Parts is larger than the second Natural

(check-expect (find-tough-advertise--widget (make-widget "" 0 0 0 empty) 0 0) empty)
(check-expect (find-tough-advertise--widget Telephone 5 2) (list Telephone))
(check-expect (find-tough-advertise--widget Jewelry 7 1) (list Jewelry Necklace))

;; (define (find-tough-advertise--widget w n m) empty) ;the stub

(define (find-tough-advertise--widget w n m)
  (filter--widget w (lambda (w) (and (> (string-length (widget-name w)) n) (> (length (widget-parts w)) m)))))

;; Question 3

;; Function definition for filter--widget
;; Signature: Widget ((Widget) -> Boolean) -> ListOfWidget
;; Purpose: Given widget and a function, the widget and its
;; sub-widgets will be fintered based on the function provided
;; into a list of widgets

(define (filter--widget w fn)
  (local [(define (fn-for--low low)
            (cond [(empty? low) empty]
                  [else (append (filter--widget (first low) fn) (fn-for--low (rest low)))]))]
    (cond [(fn w) (cons w (fn-for--low (widget-parts w)))]
    [else (fn-for--low (widget-parts w))])))

;; Function definition for find-name-longer-than--widget: 
;; Signature: Widget Natural -> ListOfWidget
;; Purpose: Take a widget and a string length, returns a list 
;; that can include that widget and or its subwidget, with names 
;; longer than the spesified string length

(check-expect (find-name-longer-than--widget (make-widget "" 0 0 0 empty) 0) empty)
(check-expect (find-name-longer-than--widget Telephone 5) (list Telephone Receiver Buttons Numbers))

;; (define (find-name-longer-than--widget (make-widget "" 0 0 0 empty) 0) empty) ;the stub

(define (find-name-longer-than--widget w n)
  (filter--widget w (lambda (w) (> (string-length (widget-name w)) n))))

;; Function definition for find-quantity-over--widget: 
;; Signature: Widget Natural -> ListOfWidget
;; Purpose: Given a widget and a natural number, examine the widget, as well as all of 
;; the subwidgets used to manufacture it, and return those whose 
;; quantity in stock is greater than the given natural number.

(check-expect (find-quantity-over--widget (make-widget "" 0 0 0 empty) 0) empty)
(check-expect (find-quantity-over--widget Telephone 7) (list Receiver Buttons Numbers))

;; (define (find-quantity-over--widget (make-widget "" 0 0 0 empty) 0) empty)) ; Stub

(define (find-quantity-over--widget w n)
  (filter--widget w (lambda (w) (> (widget-quantity w) n))))

;; Function definition for find-cheaper-than--widget: 
;; Signature: Widget Natural -> ListOfWidgets
;; Purpose: Take a widget and a price, returns a list 
;; that can include that widget or its subwidgets, with prices
;; cheaper than that spesified by the minimum price

(check-expect (find-cheaper-than--widget (make-widget "" 0 0 1000 empty) 0) empty)
(check-expect (find-cheaper-than--widget Telephone 6) (list Buttons Numbers Cord Wire))

;; (define (find-cheaper-than--widget (make-widget "" 0 0 0 empty) 0) empty)

(define (find-cheaper-than--widget w n)
  (filter--widget w (lambda (w) (< (widget-price w) n))))

;; Function definition for find-hard-make--widget: 
;; Signature: Widget Natural Number ->  ListOfWidget
;; Purpose: Given a widget, a natural number, and a number, 
;; examine the widget, as well as all of the subwidgets used to manufacture it,
;; and return those whose quantity in stock is less than the given natural number 
;; or whose cost is greater than the given number.

(check-expect (find-hard-make--widget (make-widget "" 0 0 0 empty) 0 0) empty)
(check-expect (find-hard-make--widget Telephone 6 6) (list Telephone Receiver Wire))

;(define (find-hard-make--widget (make-widget "" 0 0 0 empty) 0 0) empty) ; Stub

(define (find-hard-make--widget w q p)
  (filter--widget w (lambda (w) (or (< (widget-quantity w) q) (> (widget-price w) p)))))