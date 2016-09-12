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
  (%make-chart plotter canvas projection point-to-canvas draw-star)
  chart?
  (plotter chart-plotter)
  (canvas chart-canvas)
  (projection chart-projection)
  (point-to-canvas chart-point-to-canvas)
  (draw-star chart-draw-star))

(define (make-chart plotter projection fit-to-objects scale
                    draw-fit-to-objects)
  (with-instance ((<plotter> plotter))
    (let* ((center/celestial (boundaries/celestial-center fit-to-objects))
           (boundaries/cartesian2
            (map (lambda (boundary/celestial)
                   (map (lambda (point) (projection point center/celestial))
                        boundary/celestial))
                 fit-to-objects))
           (projection-bbox (cartesian2-bounding-box
                             (apply append boundaries/cartesian2))))
      (match-let (((xmin ymin xmax ymax) projection-bbox))
        (let* ((pwidth (- xmax xmin))
               (pheight (- ymax ymin))
               (width (inexact->exact (+ 1 (round (* pwidth scale)))))
               (height (inexact->exact (+ 1 (round (* pheight scale))))))
          (define (point-to-canvas x y)
            (list (inexact->exact (round (* (- x xmin) scale)))
                  (inexact->exact (round (* (- y ymin) scale)))))
          (let* ((canvas (plotter-new width height))
                 (black (plotter-color 0 0 0 255))
                 (p (%make-chart
                     plotter
                     canvas
                     projection
                     point-to-canvas
                     (lambda (ra dec mag) ;; draw-star
                       (let ((min-r 1)
                             (max-r 8)
                             (min-mag -1.5)
                             (max-mag 4.5))
                         (define (star-radius mag)
                           (let* ((a (/ (- min-r max-r) (- max-mag min-mag)))
                                  (b (- max-r (* a min-mag))))
                             (inexact->exact (round (+ b (* a mag))))))
                         (match-let
                             (((x y)
                               (apply point-to-canvas
                                      (projection (list ra dec) center/celestial))))
                           (let* ((r (star-radius mag)))
                             (plotter-fill-circle canvas black x y r))))))))
            (when draw-fit-to-objects
              (for-each
               (lambda (boundary/cartesian2)
                 (let ((points (close-loop boundary/cartesian2)))
                   (let loop ((prev (apply point-to-canvas (first points)))
                              (points (cdr points)))
                     (unless (null? points)
                       (match-let (((x1 y1) prev)
                                   ((x2 y2) (apply point-to-canvas (first points))))
                         (plotter-line canvas black x1 y1 x2 y2)
                         (loop (list x2 y2) (cdr points)))))))
               boundaries/cartesian2))
            p))))))

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
         (boundaries/celestial (map read-boundary constellations))
         (chart (make-chart plotter projection boundaries/celestial
                            (alist-ref 'scale options)
                            #t)))
    ;; drawing stars
    (with-instance ((<plotter> plotter))
      (let* ((image-filename (string-append (->string chart-name) ".png"))
             (black (plotter-color 0 0 0 255)))
        (for-each
         (lambda (constellation)
           (let ((stars (hyg-get-records/constellation
                         constellation
                         (lambda (rec) (< (alist-ref 'mag rec) 4.5)))))
             (for-each
              (lambda (star)
                ((chart-draw-star chart)
                 (alist-ref 'ra star)
                 (alist-ref 'dec star)
                 (alist-ref 'mag star)))
              stars)))
         constellations)
        (plotter-write (chart-canvas chart) image-filename)
        (fmt #t "wrote " image-filename nl)))))

(define (main options)
  (let* ((output (alist-ref 'output options))
         (projection-name (alist-ref 'projection options))
         (projection (alist-ref projection-name (projections))))
    (unless output
      (fmt #t "No output set." nl)
      (exit 1))
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
       "azimuthal-equidistant, stereographic"
     (set! arg (string->symbol arg)))

   (args:make-option
       (scale) #:required
       "image height and width are radius * ARG, (0 < radius < 1)"
     (set! arg (string->number arg)))))

(receive (options charts)
    (args:parse (command-line-arguments) opts)
  (let* ((options-file (alist-ref 'options-file options))
         (options-file-options
          (if options-file
              (with-input-from-file options-file read)
              '()))
         (options-charts
          (alist-ref 'charts options-file-options eq? '()))
         (charts
          (if (null? charts)
              options-charts
              (map (lambda (chart) (or (assq chart options-charts) chart))
                   (map (o string->symbol string-downcase) charts)))))
    (when (null? charts)
      (fmt #t "No charts requested."
           nl nl (usage-header) nl (args:usage opts) nl)
      (exit 1))
    (main (append
           `((charts . ,charts))
           options
           options-file-options
           '((projection . azimuthal-equidistant) ;; default options
             (scale . 1000))))))
