(define-module pattern.pattern
  (use chase_evade.bresenham)
  (use gauche.array)
  (use gauche.collection)
  (use srfi-1)
  (use srfi-42)
  (export-all)
  )



(select-module pattern.pattern)

(define-class <pattern> ()
  ((pattern :init-value '()
            :accessor pattern-of)
   (offset :init-value '(0 . 0)
           :accessor offset-of)))


;; (load-pattern "Sample")

;; Sample
;; ((0 . 0) (4 . 10))
;; ((4 . 10) (15 . 3))
;; ((15 . 3) (0 . 0))
(define (load-pattern file)
  (let1 p (make <pattern>)
        (with-input-from-file file
          (lambda () (port-for-each
                      (lambda (cord) (build-path-segment! p cord))
                      read)))        
        p))

(define-method build-path-segment! ((p <pattern>) sx sy ex ey)
  (set! (pattern-of p)
        (append (pattern-of p)
                (if (null? (pattern-of p))
                    (bresenham sx sy ex ey)
                    (cdr (bresenham sx sy ex ey))))))

(define-method build-path-segment! ((p <pattern>) (cp <pair>))
  (build-path-segment!
   p
   (car (car cp)) (cdr (car cp))
   (car (cadr cp)) (cdr (cadr cp))))

(define-method normalize! ((p <pattern>))
  (let ((sx (car (find-min (pattern-of p) :key car)))        
        (sy (cdr (find-min (pattern-of p) :key cdr))))
    (map! (lambda (x)
            (cons (- (car x) sx)
                  (- (cdr x) sy)))
          (pattern-of p))))

(define-method set-offset! ((p <pattern>) ofs)
  (set! (offset-of p) ofs))

(define-method get-range ((p <pattern>))
  (define (range acs)
    (values (call-with-values
                (lambda () (find-min&max (pattern-of p) :key acs))              
              (lambda (x y) (+ (abs (acs x)) (abs (acs y)) 1)))))
  (values (range car) (range cdr)))

(define-method pattern->matrix ((p <pattern>))
  (let1 ar (call-with-values (cut get-range p)
                   (lambda (x y) (make-array (shape 0 y 0 x) 0)))
        (for-each         
         (lambda (xy) (array-set! ar (cdr xy) (car xy) 1))         
         (pattern-of p))
        ar))

(define (array-print ar)
  (apply print "  " (list-ec (: x 0 (array-length ar 1)) x))
  (print)
  (array-for-each-index
   ar (lambda (i j)
        (when (= j 0) (display i) (display " "))
        (display (array-ref ar i j))
        (when (= (+ j 1) (array-length ar 1))
              (display "\n")))))


;; (print (pattern-of p))
;; (begin (print) (array-print (pattern->matrix p)))
;; (array-for-each-index (pattern->matrix p) print)

;; (define p (make <pattern>))
;; (build-path-segment! p 4 9 10 10)
;; (build-path-segment! p 10 10 2 5)
;; (build-path-segment! p 2 5 4 4)
;; (normalize! p)
;; (print (pattern-of p))
;; (set-offset! p '(2 . 2))
;; (print (offset-of p))

;; (define ar
;;   (call-with-values
;;       (cut get-range p)
;;     (lambda (x y) (make-array(shape 0 x 0 y) 0))))






(provide "pattern/pattern")

