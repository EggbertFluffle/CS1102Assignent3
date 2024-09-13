;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname DiAmbrosio-Patel-part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DiAmbrosio, Patel

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

;; as always, the following tests are *not* exhaustive and simply serve as additional
;; documentation

#;
(define (fn-fo-list-of-widget low)
  (cond [(empty? low) empty]
        [else (append (find-name-longer-than--widget (first low)) (fn-fo-list-of-widget (rest low)))]))
#;
(define (fn-for-widget w)
  (cond [(empty? (widget-parts w)) (... (widget-quantity w) (widget-time w) (widget-parts))])
        [else (fn-fo-list-of-widget (widget-parts w))])

;; test cases for Part 1
(check-expect (find-name-longer-than--widget Wire 3) (list Wire))
(check-expect (find-quantity-over--widget Wire 2) (list Wire))
(check-expect (find-cheaper-than--widget Wire 6) (list Wire))
(check-expect (find-hard-make--widget Jewelry 6 4)
              (list Jewelry Ring Pendant Bracelet Beads))
(check-expect (resupply--widget Wire 6 2)
              (make-widget "Wire" 5 0 5 empty))
;;                                 ^^^ should be 0, not 5

;; Function definition for find-name-longer-than--widget: 
;; Signature: Widget Natural -> ListOfWidget
;; Purpose: Take a widget and a string length, returns a list 
;; that can include that widget or its subwidget, with names 
;; longer than the spesified string length

(check-expect (find-name-longer-than--widget (make-widget "" 0 0 0 empty) 0) empty)
(check-expect (find-name-longer-than--widget Telephone 5) (list Telephone Receiver Buttons Numbers))

;; (define (find-name-longer-than--widget (make-widget "" 0 0 0 empty) 0) empty) ;the stub

(define (find-name-longer-than--low low n)
  (cond [(empty? low) empty]
        [else (append (find-name-longer-than--widget (first low) n) (find-name-longer-than--low (rest low) n))]))

(define (find-name-longer-than--widget w n)
  (cond [(> (string-length (widget-name w)) n) (cons w (find-name-longer-than--low (widget-parts w) n))]
        [else (find-name-longer-than--low (widget-parts w) n)]))

;; Function definition for find-quantity-over--widget: 
;; Signature: Widget Natural -> ListOfWidget
;; Purpose: Given a widget and a natural number, examine the widget, as well as all of 
;; the subwidgets used to manufacture it, and return those whose 
;; quantity in stock is greater than the given natural number.

(check-expect (find-quantity-over--widget (make-widget "" 0 0 0 empty) 0) empty)
(check-expect (find-quantity-over--widget Telephone 7) (list Receiver Buttons Numbers))

;; (define (find-quantity-over--widget (make-widget "" 0 0 0 empty) 0) empty)) ; Stub

(define (find-quantity-over--low low n)
  (cond [(empty? low) empty]
        [else (append (find-quantity-over--widget (first low) n) (find-quantity-over--low (rest low) n))]))

(define (find-quantity-over--widget w n)
  (cond [(> (widget-quantity w) n) (cons w (find-quantity-over--low (widget-parts w) n))]
        [else (find-quantity-over--low (widget-parts w) n)]))

;; Function definition for find-cheaper-than--widget: 
;; Signature: Widget Natural -> ListOfWidgets
;; Purpose: Take a widget and a price, returns a list 
;; that can include that widget or its subwidgets, with prices
;; cheaper than that spesified by the minimum price

(check-expect (find-cheaper-than--widget (make-widget "" 0 0 1000 empty) 0) empty)
(check-expect (find-cheaper-than--widget Telephone 6) (list Buttons Numbers Cord Wire))

;; (define (find-cheaper-than--widget (make-widget "" 0 0 0 empty) 0) empty)

(define (find-cheaper-than--low low n)
  (cond [(empty? low) empty]
        [else (append (find-cheaper-than--widget (first low) n) (find-cheaper-than--low (rest low) n))]))

(define (find-cheaper-than--widget w n)
  (cond [(< (widget-price w) n) (cons w (find-cheaper-than--low (widget-parts w) n))]
        [else (find-cheaper-than--low (widget-parts w) n)]))

;; Function definition for find-hard-make--widget: 
;; Signature: Widget Natural Number ->  ListOfWidget
;; Purpose: Given a widget, a natural number, and a number, 
;; examine the widget, as well as all of the subwidgets used to manufacture it,
;; and return those whose quantity in stock is less than the given natural number 
;; or whose cost is greater than the given number.

(check-expect (find-hard-make--widget (make-widget "" 0 0 0 empty) 0 0) empty)
(check-expect (find-hard-make--widget Telephone 6 6) (list Telephone Receiver Wire))

;(define (find-hard-make--widget (make-widget "" 0 0 0 empty) 0 0) empty) ; Stub

(define (find-hard-make--low low q p)
  (cond [(empty? low) empty]
        [else (append (find-hard-make--widget (first low) q p) (find-hard-make--low (rest low) q p))]))

(define (find-hard-make--widget w q p)
  (cond [(or (< (widget-quantity w) q) (> (widget-price w) p)) (cons w (find-hard-make--low (widget-parts w) q p))]
        [else (find-hard-make--low (widget-parts w) q p)]))

;; Function definition for resupply--widget
;; Signature: Widget Natural Natural -> Widget 
;; Purpose: Takes a widget and a quantity as well as a resupply value.
;; resupply--widget will ensure that the widget as well as its subwidgets have
;; an adequate amount of parts to build the passed in quantity of the parent widget
;; Otherwise, the part that is in lacking will have its quanitity increased by the resupply amount

(define RESUPPLY-TEST (make-widget "Telephone" 10 20 15 (list Receiver 
                                                             	Buttons
                                                             	(make-widget "Cord" 7 0 5 (list 
                                                                                        (make-widget "Wire" 8 0 5 empty))))))
(check-expect (resupply--widget Telephone 7 5) RESUPPLY-TEST)

;; (define (resupply--widget (make-widget "" 0 0 0 empty) 0 0) (make-widget "" 0 0 0 empty)) ;the stub

(define (resupply--low low m i)
  (cond [(empty? low) empty]
        [else (cons (resupply--widget (first low) m i) (resupply--low (rest low) m i))]))

(define (resupply--widget w m i)
  (cond [(< (widget-quantity w) m)
         (make-widget
          (widget-name w)
          (+ i (widget-quantity w))
          (widget-time w)
          (widget-price w)
          (resupply--low (widget-parts w) m i))]
    [else (make-widget
          (widget-name w)
          (widget-quantity w)
          (widget-time w)
          (widget-price w)
          (resupply--low (widget-parts w) m i))]))