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

(use (srfi 1 13)
     args
     (only data-structures alist-ref)
     fmt
     imlib2
     matchable)

(load "catalog-pbarbier-constellation-boundaries")
(import catalog-pbarbier-constellation-boundaries)


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
;; Projections
;;

;; Azimuthal Equidistant
;;

(define (azimuthal-equidistant point center)
  (match-let (((ra dec) point)
              ((center-ra center-dec) center))
    (let* ((cosc (+ (* (sin center-dec) (sin dec))
                    (* (cos center-dec) (cos dec) (cos (- ra center-ra)))))
           (c (acos cosc)))
      (if (< (abs (- c tau/2)) 0.0001)
          (list #f #f) ;; error or NaN
          (let* ((k (if (zero? c) 1 (/ c (sin c))))
                 (x (* k (cos dec) (sin (- ra center-ra))))
                 (y (* k (- (* (cos center-dec) (sin dec))
                            (* (sin center-dec) (cos dec) (cos (- ra center-ra)))))))
            (list (- x) (- y)))))))


;;
;; Main
;;

(define (main options)
  (let* ((constellation (alist-ref 'constellation options))
         (boundary/celestial (read-boundary constellation))
         (boundary/cartesian
          (map (match-lambda ((ra dec) (celestial->cartesian ra dec 1.0)))
               boundary/celestial))
         (center/cartesian (cartesian-center boundary/cartesian))
         (center/celestial (take (apply cartesian->celestial center/cartesian) 2))
         (boundary/cartesian2 (map (lambda (point)
                                     (azimuthal-equidistant point center/celestial))
                                   boundary/celestial))
         (projection-bbox (cartesian2-bounding-box boundary/cartesian2)))
    (match-let (((xmin ymin xmax ymax) projection-bbox))
      (let* ((scale (alist-ref 'scale options))
             (pwidth (- xmax xmin))
             (pheight (- ymax ymin))
             (aspect (/ pwidth pheight)))
        ;; drawing
        (let* ((width (inexact->exact (+ 1 (round (* pwidth scale)))))
               (height (inexact->exact (+ 1 (round (* pheight scale)))))
               (image (image-create width height))
               (image-filename (string-append (string-downcase (->string constellation)) ".png"))
               (black (color/rgba 0 0 0 255))
               (points (close-loop boundary/cartesian2)))
          (define (point-to-canvas x y)
            (list (inexact->exact (round (* (- x xmin) scale)))
                  (inexact->exact (round (* (- y ymin) scale)))))
          (let loop ((prev (apply point-to-canvas (first points)))
                     (points (cdr points)))
            (unless (null? points)
              (match-let (((x1 y1) prev)
                          ((x2 y2) (apply point-to-canvas (first points))))
                (image-draw-line image black x1 y1 x2 y2)
                (loop (list x2 y2) (cdr points)))))
          (image-save image image-filename))))))

(define (usage-header)
  (fmt #f "usage: draw-constellation [options] <const>" nl nl
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
       (scale) #:required
       "image height and width are radius * ARG, (0 < radius < 1)"
     (set! arg (string->number arg)))))

(call-with-values
    (lambda () (args:parse (command-line-arguments) opts))
  (match-lambda*
    ((options ())
     (fmt #t (usage-header) nl (args:usage opts) nl)
     (exit 1))
    ((options (constellation))
     (let* ((constellation (string->symbol (string-upcase constellation)))
            (options-file (alist-ref 'options-file options))
            (options (append
                      `((constellation . ,constellation))
                      options
                      (if options-file
                          (append options
                                  (with-input-from-file options-file read))
                          '())
                      '((scale . 1000))))) ;; default options
       (main options)))
    ((options (constellation . rest))
     (fmt #t (usage-header) nl (args:usage opts) nl)
     (exit 1))))
