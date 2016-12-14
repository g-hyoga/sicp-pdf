#lang racket
(require sicp-pict)
(define kirby (load-painter "img/kirby.gif"))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

#;(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (fipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;(paint (square-limit kirby 1))

;;;;;;;;;;;;;;;;;;;;;;;
(define (split 1th-divide 2th-divide)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split 1th-divide 2th-divide) painter (- n 1))))
          (1th-divide painter (2th-divide smaller smaller))))))

(define right-split2 (split beside below))
(define up-split2 (split below beside))
;;;;;;;;;;;;;;;;;;;;;;;

(paint (up-split kirby 2))
(paint (up-split2 kirby 2))

(paint (right-split kirby 2))
(paint (right-split2 kirby 2))

