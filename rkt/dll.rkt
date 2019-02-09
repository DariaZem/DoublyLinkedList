#lang racket

(require racket/mpair)
(require "mpair-ext.rkt")

(provide (all-defined-out))

(define (dll . args)
  (let* ((head #f)
        (tail #f))
    (define (make-dll . args)
      (cond
        ((and (not head) (not tail) (empty? args))
          #f)
        ((and (not head) (not tail)) ; when first element added
          (let ((new-node (make-new-node head tail (car args))))
            (set! head new-node)
            (set! tail new-node)
            (apply make-dll (cdr args))))
        ((empty? args)
          dispatch)
        (else
          (let ((new-value (car args)))
            (add-to-tail! new-value)
            (apply make-dll (cdr args))))))
    (define (make-new-node prev next el)
      (mcons prev (mcons next el)))
    (define (init-dll el)
      (let ((new-node (make-new-node #f #f el)))
        (set! head new-node)
        (set! tail new-node)
        dispatch))
    (define (get-prev node)
      (mcar node))
    (define (set-prev! node p)
      (set-mcar! node p))
    (define (get-next node)
      (mcadr node))
    (define (set-next! node p)
      (set-mcadr! node p))
    (define (get-value node)
      (and (not (empty? node)) (mcddr node)))
    (define (set-value! node el)
      (set-mcddr! node el))
    (define (add-to-tail! el)
      (if (not head)
        (init-dll el)
        (let* ((old-node tail)
              (new-node (make-new-node old-node #f el)))
          (set! tail new-node)
          (set-next! old-node new-node)
          dispatch)))
    (define (add-to-head! el)
      (if (not head)
        (init-dll el)
        (let* ((old-node head)
              (new-node (make-new-node #f old-node el)))
          (set! head new-node)
          (set-prev! old-node new-node)
          dispatch)))
    (define (add-in-position! el index)
      (define (add-in-position-iter el index curnode)
        (define new-node (make-new-node #f #f el))
        (cond
          ((= index 0)
            (let ((prev-node (get-prev curnode)))
              (if (not prev-node)
                (begin
                  ; add to the head:
                  (set-next! new-node curnode)
                  (set-prev! curnode new-node)
                  (set! head new-node))
                (begin
                  (set-prev! curnode new-node)
                  (set-next! new-node curnode)
                  (set-prev! new-node prev-node)
                  (set-next! prev-node new-node)))
              #t))
          ((and (> index 0) (not curnode))
            ; add to tail:
            (let ((old-node tail))
              (set-next! old-node new-node)
              (set-prev! new-node old-node)
              (set! tail new-node)))
          (else
            (add-in-position-iter el (- index 1) (get-next curnode)))))
      (add-in-position-iter el index head)
      dispatch)
    (define (to-string)
      (define (to-string-iter node)
        (cond
          ((not node) (error "list is empty"))
          ((not (get-next node)) (get-value node))
          (else (format "~a->~a" (get-value node) (to-string-iter (get-next node))))))
      (to-string-iter head))
    (define (dispatch m)
      (cond
        ((equal? m 'add-last) add-to-tail!)
        ((equal? m 'add-in-position) add-in-position!)
        ((equal? m 'to-string) to-string)
        ))
    (apply make-dll args)
    dispatch))

(define (add-last dll el) ((dll 'add-last) el))
(define (add-in-position dll el index) ((dll 'add-in-position) el index))
(define (->string dll) ((dll 'to-string)))

(module+ test

  (require rackunit)

  (define-simple-check (check-dll-equal? dll1 dll2)
    (check-equal? (->string dll1) (->string dll2)))

  (check-dll-equal? (add-last (dll) 1) (dll 1))
  (check-dll-equal? (add-last (dll 1 2 3) 4) (dll 1 2 3 4))

  (check-dll-equal? (add-in-position (dll 1 2 3 4 5) "foo" 2) (dll 1 2 "foo" 3 4 5))
  (check-dll-equal? (add-in-position (dll 1 2 3 4 5) (cons 1 2) 0) (dll (cons 1 2) 1 2 3 4 5))
  (check-dll-equal? (add-in-position (dll 1 2 3 4 5) 10 5) (dll 1 2 3 4 5 10))

  (check-equal? (->string (dll 1 2 3)) "1->2->3")
)