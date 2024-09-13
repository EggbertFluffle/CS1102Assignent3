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

;; test cases for Part 1
(check-expect (find-name-longer-than--widget Wire 3) (list Wire))
(check-expect (find-quantity-over--widget Wire 2) (list Wire))
(check-expect (find-cheaper-than--widget Wire 6) (list Wire))
(check-expect (find-hard-make--widget Jewelry 6 4)
              (list Jewelry Ring Pendant Bracelet Beads))
(check-expect (resupply--widget Wire 6 2)
              (make-widget "Wire" 5 0 5 empty))
;;                                 ^^^ should be 0, not 5

;; DELETE THIS WHEN DONE
;#
(define (sum-children lon)
        (local ([(define (sum-arb-tree node)
                         (+ (node-data node)
                            (sum-children (node-children node))))])
               (cond [(empty? lon) 0]
                 [else
                  (+ (sub-arb-tree (first lon))
                     (sum-children (rest lon)))])))


;; Function definition for find-name-longer-than--widget: 
;; Signature: Widget Natural -> ListOfWidget
;; Purpose: Take a widget and a string length, returns a list 
;; that can include that widget or its subwidget, with names 
;; longer than the spesified string length

;; (define (find-name-longer-than--widget (make-widget "" 0 0 0 empty) 0) empty) ;the stub

;#
(define (sum-children lon)
        (local ([(define (sum-arb-tree node)
                         (+ (node-data node)
                            (sum-children (node-children node))))])
               (cond [(empty? lon) 0]
                 [else
                  (+ (sub-arb-tree (first lon))
                     (sum-children (rest lon)))])))

;; Function definition for find-quantity-over--widget: 
;; Signature: Widget Natural -> ListOfWidget
;; Purpose: Given a widget and a natural number, examine the widget, as well as all of 
;; the subwidgets used to manufacture it, and return those whose 
;; quantity in stock is greater than the given natural number. 

;; (define (find-quantity-over--widget (make-widget "" 0 0 0 empty) 0) empty)) ; Stub

;; Function definition for find-cheaper-than--widget: 
;; Signature: Widget Natural -> ListOfWidgets
;; Purpose: Take a widget and a price, returns a list 
;; that can include that widget or its subwidgets, with prices
;; cheaper than that spesified by the minimum price

;; (define (find-cheaper-than--widget (make-widget "" 0 0 0 empty) 0) empty)

;; Function definition for find-hard-make--widget: 
;; Signature: Widget Natural Number ->  ListOfWidget
;; Purpose: Given a widget, a natural number, and a number, 
;; examine the widget, as well as all of the subwidgets used to manufacture it,
;; and return those whose quantity in stock is less than the given natural number 
;; or whose cost is greater than the given number. 

;(define (find-hard-make--widget (make-widget "" 0 0 0 empty) 0 0) empty) ; Stub

;; Function definition for resupply--widget
;; Signature: Widget Natural Natural -> Widget 
;; Purpose: Takes a widget and a quantity as well as a resupply value.
;; resupply--widget will ensure that the widget as well as its subwidgets have
;; an adequate amount of parts to build the passed in quantity of the parent widget
;; Otherwise, the part that is in lacking will have its quanitity increased by the resupply amount

;; (define (resupply--widget (make-widget "" 0 0 0 empty) 0 0) (make-widget "" 0 0 0 empty)) ;the stub