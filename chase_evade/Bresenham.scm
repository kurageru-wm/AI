(define-syntax tmp
    (syntax-rules ()
      ((_ startx starty d1 d2 n1 n2 s1 s2 e)
       (let loop (;; (frac (- (* d2 2) d1))
                  (frac (- d2 (/ d1 2)))
                  (n1 startx)
                  (n2 starty)
                  (step 0)
                  (xlist '())
                  (ylist '()))
         (if (= n1 (+ e 1))
             (values (reverse xlist) (reverse ylist))             
             (loop
              (if (>= frac 0) (+ d2 (- frac d1)) (+ frac d2))
              (+ n1 s1)
              (if (>= frac 0) (+ n2 s2) n2)
              (+ step 1)
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
        (call-with-values
            (lambda () (tmp startx starty deltay deltax nexty nextx stepy stepx endy))
          (lambda (x y) (values y x))))
          
    ))

(bresenham 1 1 8 11)