#lang racket

(require racket/mpair)

(provide (all-defined-out))

(define (mcddr x) (mcdr (mcdr x)))
(define (mcadr x) (mcar (mcdr x)))
(define (mcdar x) (mcdr (mcar x)))
(define (mcaddr x) (mcar (mcdr (mcdr x))))
(define (mcadar x) (mcar (mcdr (mcar x))))

(define (set-mcadr! x val) (set-mcar! (mcdr x) val))
(define (set-mcddr! x val) (set-mcdr! (mcdr x) val))
