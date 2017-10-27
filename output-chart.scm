
;; Copyright 2016 John J Foerch. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module output-chart
    *

(import chicken scheme)

(use (srfi 1 13)
     data-structures
     fmt
     imlib2
     matchable
     typeclass)

(import catalog)
(import catalog-hyg-database)

;;
;; Utilities
;;

(define tau (* 4.0 (asin 1.0)))
(define tau/2 (* 2.0 (asin 1.0)))
(define tau/4 (asin 1.0))

(define (square x) (* x x))

(define (distance3 p1 p2)
  (match-let (((x1 y1 z1) p1)
              ((x2 y2 z2) p2))
    (sqrt
     (+ (square (- x1 x2))
        (square (- y1 y2))
        (square (- z1 z2))))))

(define (celestial->cartesian ra dec distance)
  (let ((theta ra)
        (phi (- tau/4 dec)))
    (list
     (* distance (cos theta) (sin phi))
     (* distance (sin theta) (sin phi))
     (* distance (cos phi)))))

(define (cartesian->celestial x y z)
  (let ((r (sqrt (+ (square x) (square y) (square z)))))
    (list
     (atan y x)
     (asin (/ z r))
     r)))

(define (cartesian-center points)
  (map
   (lambda (x) (/ x (length points)))
   (fold (lambda (point sum) (map + point sum))
         '(0 0 0)
         points)))

(define (cartesian2-bounding-box points)
  (let loop ((xmin (caar points))
             (ymin (cadar points))
             (xmax (caar points))
             (ymax (cadar points))
             (points (cdr points)))
    (if (null? points)
        (list xmin ymin xmax ymax)
        (let ((x (caar points))
              (y (cadar points)))
          (loop (min x xmin)
                (min y ymin)
                (max x xmax)
                (max y ymax)
                (cdr points))))))

(define (close-loop coords)
  (append coords (list (first coords))))


;;
;; Chart Spec
;;

(define (parse-catalog-name spec)
  (let ((spec (->string spec)))
    (map string->symbol (string-split spec ":"))))

(define-record-type :chart-spec
  (%make-chart-spec name draw)
  chart-spec?
  (name chart-spec-name)
  (draw chart-spec-draw))

(define (make-chart-spec spec)
  (cond
   ((and (symbol? spec) (string-any #\: (->string spec)))
    (let* ((split (string-split (->string spec) ":"))
           (object-name (string->symbol (second split))))
      (%make-chart-spec object-name (list (parse-catalog-name spec)))))
   ((pair? spec)
    (let* ((fst (first spec))
           (props (cdr spec))
           (draw (alist-ref 'draw props)))
      (cond
       (draw (%make-chart-spec fst (map parse-catalog-name draw)))
       (else
        (let* ((split (string-split (->string fst) ":"))
               (object-name (string->symbol (second split))))
          (%make-chart-spec object-name (list (parse-catalog-name fst))))))))))


;;
;; Draw Charts
;;

(define (boundaries/celestial-center boundaries/celestial)
  (let ((cartesian-points
         (map (match-lambda ((ra dec) (celestial->cartesian ra dec 1.0)))
              (apply append boundaries/celestial))))
    (take
     (apply cartesian->celestial (cartesian-center cartesian-points))
     2)))


(define-class <plotter>
  plotter-new ;; width height
  plotter-fill-rectangle ;; p color x y width height
  plotter-fill-circle ;; p color x y r
  plotter-line ;; p color x1 y1 x2 y2
  plotter-write ;; p path
  plotter-color ;; r g b a
  )

(define-record-type :chart
  (%make-chart plotter canvas projection draw-star)
  chart?
  (plotter chart-plotter)
  (canvas chart-canvas)
  (projection chart-projection)
  (draw-star chart-draw-star))

(define (chart-draw chart commands projection)
  (with-instance ((<plotter> (chart-plotter chart)))
    (let* ((canvas (chart-canvas chart))
           (black (plotter-color 0 0 0 255)))
      (for-each
       (lambda (command)
         (let-values (((instruction attrs points)
                       (draw-command-deconstruct command)))
           (let ((points (if (alist-ref 'closed attrs)
                             (append points (list (first points)))
                             points)))
             (let loop ((prev (apply projection (first points)))
                        (points (cdr points)))
               (unless (null? points)
                 (match-let (((x1 y1) prev)
                             ((x2 y2) (apply projection (first points))))
                   (plotter-line canvas black x1 y1 x2 y2)
                   (loop (list x2 y2) (cdr points))))))))
       commands))))

(define draw-command-deconstruct
  (match-lambda
    ((cmd ('@ . attrs) . data)
     (values cmd attrs data))
    ((cmd . data)
     (values cmd '() data))))

(define draw-command-coords
  (match-lambda
    ((cmd ('@ . attrs) . coords)
     coords)
    ((cmd . coords)
     coords)))

(define (draw-command-replace-coords command new-coords)
  (match command
    ((cmd ('@ . attrs) . coords)
     `(,cmd (@ . ,attrs) . ,new-coords))
    ((cmd . coords)
     `(,cmd . ,new-coords))))


(define (make-chart plotter projection scale fit)
  (with-instance ((<plotter> plotter))
    (let* ((boundaries/celestial (map draw-command-coords fit))
           (center/celestial (boundaries/celestial-center boundaries/celestial))
           (boundaries/projection
            (map (lambda (boundary/celestial)
                   (map (match-lambda ((ra dec) (apply projection ra dec center/celestial)))
                        boundary/celestial))
                 boundaries/celestial)))
      (match-let (((xmin ymin xmax ymax)
                   (cartesian2-bounding-box
                    (apply append boundaries/projection))))
        (let* ((width (inexact->exact (+ 1 (round (* (- xmax xmin) scale)))))
               (height (inexact->exact (+ 1 (round (* (- ymax ymin) scale)))))
               (canvas (plotter-new width height))
               (black (plotter-color 0 0 0 255)))
          (define (celestial->projection ra dec)
            (apply projection ra dec center/celestial))
          (define (projection->plot x y)
            (list (inexact->exact (round (* (- (- x xmax)) scale)))
                  (inexact->exact (round (* (- (- y ymax)) scale)))))
          (define (celestial->plot ra dec)
            (apply projection->plot (celestial->projection ra dec)))
          (define (draw-star ra dec mag)
            (let ((min-r 1)
                  (max-r 8)
                  (min-mag -1.5)
                  (max-mag 4.5))
              (define (star-radius mag)
                (let* ((a (/ (- min-r max-r) (- max-mag min-mag)))
                       (b (- max-r (* a min-mag))))
                  (max 1 (inexact->exact (round (+ b (* a mag)))))))
              (match-let
                  (((x y) (celestial->plot ra dec)))
                (plotter-fill-circle canvas black x y (star-radius mag)))))
          (let ((chart (%make-chart plotter canvas celestial->plot draw-star))
                (command (map (lambda (c d)
                                (draw-command-replace-coords c d))
                              fit boundaries/projection)))
            (values
             chart
             (lambda () (chart-draw chart command projection->plot)))))))))


(define plotter-imlib2
  (make-<plotter>
   image-create            ;; plotter-new
   image-fill-rectangle    ;; plotter-fill-rectangle
   (lambda (p color x y r) ;; plotter-fill-circle
     (image-fill-ellipse p color x y r r))
   image-draw-line ;; plotter-line
   image-save      ;; plotter-write
   color/rgba      ;; plotter-color
   ))


(define (draw-chart chart-spec plotter projection options)
  (let* ((chart-name (chart-spec-name chart-spec))
         (draw-objects (chart-spec-draw chart-spec))
         ;; at the moment, the draw is only constellations, and that is
         ;; what we want to fit to.  however, we need a way to specify
         ;; other objects in the draw, which will not be fit to.
         ;;
         ;; we're drawing a certain amount of stars from each
         ;; constellation shown in the chart.  we'd like to be able to
         ;; specify this with a single config option, not have to add the
         ;; same stars specification to every chart.
         ;;
         ;; the 'draw' spec doesn't really capture what we want to do.
         ;; we're not just drawing these constellations, we're setting
         ;; them as the subject of the chart, and then basing several draw
         ;; commands on them.
         ;;
         (boundaries/celestial (append-map
                                (match-lambda
                                  ((catalog object)
                                   ((catalog-query (catalog-find catalog))
                                    object)))
                                draw-objects))
         (constellations (map second draw-objects)))
    (let-values (((chart draw-constellation-boundary)
                  (make-chart plotter projection (alist-ref 'scale options)
                              (map (lambda (b) (cons* 'path '(@ (closed . #t)) b))
                                   boundaries/celestial))))
      (draw-constellation-boundary)
      ;; drawing stars
      (with-instance ((<plotter> plotter))
        (let* ((image-filename (string-append (->string chart-name) ".png"))
               (black (plotter-color 0 0 0 255)))
          (for-each
           (lambda (constellation)
             (let* ((maxmag 4.0)
                    (got 0)
                    (stars (take-while
                            (lambda (star)
                              (let ((mag (alist-ref 'mag star)))
                                (cond
                                 ((< mag maxmag)
                                  (set! got (add1 got))
                                  #t)
                                 ((< got 10)
                                  (set! got (add1 got))
                                  (set! maxmag (+ maxmag 0.1))
                                  #t)
                                 (else #f))))
                            (sort
                             (hyg-get-records/constellation
                              constellation)
                             (lambda (a b)
                               (< (alist-ref 'mag a) (alist-ref 'mag b)))))))

               #;(chart-draw
               chart
               (map
               (lambda (star)
               (match-let
               (((x y) (chart-projection
               (alist-ref 'ra star)
               (alist-ref 'dec star))))
               `(star (,x ,y ,(alist-ref 'mag star)))))
               stars)
               (cut split-at <> 2)) ;; mag is in the second value...?
               ;; no, this works well enough for the
               ;; optimized drawing case, but not
               ;; for plot-fitting

               (for-each
                (lambda (star)
                  ((chart-draw-star chart)
                   (alist-ref 'ra star)
                   (alist-ref 'dec star)
                   (alist-ref 'mag star)))
                stars)))
           constellations)
          (plotter-write (chart-canvas chart) image-filename)
          (fmt #t "wrote " image-filename nl))))))

)
