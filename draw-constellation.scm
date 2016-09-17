#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

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

(import chicken scheme)

(use (srfi 1 13 99)
     args
     (only data-structures alist-ref)
     fmt
     imlib2
     matchable
     typeclass)

(load "catalog-pbarbier-constellation-boundaries")
(import catalog-pbarbier-constellation-boundaries)

(load "catalog-hyg-database")
(import catalog-hyg-database)

(load "output-skyculture")
(import output-skyculture)

(load "projection")
(import projection)


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
;; Main
;;

(define (chart-name chart-spec)
  (cond
   ((symbol? chart-spec) chart-spec)
   ((pair? chart-spec) (first chart-spec))
   (else
    (fmt #t "bad chart spec: " chart-spec nl)
    (exit 1))))

(define (constellations-to-draw constellation-spec)
  (cond
   ((symbol? constellation-spec) (list constellation-spec))
   ((pair? constellation-spec)
    (or (alist-ref 'draw (cdr constellation-spec))
        (list (first constellation-spec))))
   (else
    (fmt #t "bad constellation spec: " constellation-spec nl)
    (exit 1))))

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
  (let* ((chart-name (chart-name chart-spec))
         (constellations (constellations-to-draw chart-spec))
         ;;XXX: read-boundary comes from a catalog
         (boundaries/celestial (map read-boundary constellations)))
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
             (let* ((maxmag 4.5)
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

(define (main options)
  (let* ((projection-name (alist-ref 'projection options))
         (projection (alist-ref projection-name (projections))))
    (unless projection
      (fmt #t "Unknown projection: " projection-name nl)
      (exit 1))

    ;; (output-skyculture (cdr output))

    (for-each
     (lambda (chart-spec)
       (draw-chart chart-spec plotter-imlib2
                   (projection-fn projection)
                   options))
     (alist-ref 'charts options))))

(define (usage-header)
  (fmt #f "usage: draw-constellation [options] [const] ..." nl nl
       " const: IAU abbreviation of a constellation (ori)" nl))

(define opts
  (list
   (args:make-option
       (h help) #:none "help"
     (fmt #t (usage-header) nl (args:usage opts) nl)
     (exit 1))

   (args:make-option
       (o options-file) #:required
       "load additional options alist from file")

   (args:make-option
       (projection) #:required
       "azimuthal-equidistant, gnomonic, stereographic"
     (set! arg (string->symbol arg)))

   (args:make-option
       (scale) #:required
       ;;XXX: this docstring is incorrect
       "image height and width are radius * ARG, (0 < radius < 1)"
     (set! arg (string->number arg)))))

(receive (options charts)
    (args:parse (command-line-arguments) opts)
  (let* ((options-file (alist-ref 'options-file options))
         (options-file-options
          (if options-file
              (with-input-from-file options-file read)
              '()))
         (options (append options options-file-options))
         (output (alist-ref 'output options))
         (output-options output-skyculture-options)
         (options
          (append options
                  output-options
                  '((projection . azimuthal-equidistant) ;; default options
                    (scale . 1000))))
         (options-charts
          (alist-ref 'charts options eq? '()))
         (charts
          (if (null? charts)
              options-charts
              (map (lambda (chart) (or (assq chart options-charts) chart))
                   (map (o string->symbol string-downcase) charts)))))
    (unless output
      (fmt #t "No output set." nl)
      (exit 1))
    (when (null? charts)
      (fmt #t "No charts requested."
           nl nl (usage-header) nl (args:usage opts) nl)
      (exit 1))
    (main (append `((charts . ,charts)) options))))
