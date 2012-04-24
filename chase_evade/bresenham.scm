(define-module chase_evade.bresenham
  (export bresenham)  
  )
(select-module chase_evade.bresenham)


(define-syntax tmp
    (syntax-rules ()
      ((_ startx starty d1 d2 n1 n2 s1 s2 e)
       (let loop (;; (frac (- (* d2 2) d1))
                  (frac (- d2 (/ d1 2)))
                  (n1 startx)
                  (n2 starty)
                  (xlist '())
                  (ylist '()))
         ;; (print n1 ":" e)
         (if (= n1 e)
             (reverse (map cons (cons n1 xlist) (cons n2 ylist)))
             (loop
              (if (>= frac 0) (+ d2 (- frac d1)) (+ frac d2))
              (+ n1 s1)
              (if (>= frac 0) (+ n2 s2) n2)
              (cons n1 xlist)
              (cons n2 ylist))))
       )))

(define (bresenham startx starty endx endy)
  (let ((stepx (if (> 0 (- endx startx)) -1 1))
        (stepy (if (> 0 (- endy starty)) -1 1))
        (deltax (* (abs (- endx startx)) 2))
        (deltay (* (abs (- endy starty)) 2)))
    (if (> deltax deltay)
        (tmp startx starty deltax deltay nextx nexty stepx stepy endx)
        (map (lambda (x) (cons (cdr x) (car x)))
             (tmp starty startx deltay deltax nexty nextx stepy stepx endy)))))    

;; (bresenham 13 17 15 10)
;; (bresenham 3 7 5 0)
;; (bresenham 10 10 13 5)
;; (bresenham 0 0 3 3)

;; (bresenham 5 5 0 0)
;; (bresenham 0 0 5 4)

;; (bresenham 18 12 18 11)

;; (bresenham 10 17 10 20)

;; (bresenham 44 20 10 14)


(provide "chase_evade/bresenham")