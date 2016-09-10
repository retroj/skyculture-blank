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
     data-structures
     extras
     fmt
     imlib2
     matchable)

(define boundaries-filename "bound_in_20.txt")

(define lines-filename "lines_in_20.txt")

(define verts-filename "verts_18.txt")


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

(define (celestial-units->radians ra dec)
  (list
   (* tau (/ ra 24.0))
   (* tau (/ dec 360.0))))

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
;; Catalog
;;

(define (close-loop coords)
  (append coords (list (first coords))))

(define (read-boundary constellation)
  (define (parse-line line)
    (with-input-from-string line
      (lambda ()
        (let* ((ra (read))
               (dec (read))
               (const (read)))
          (values ra dec const)))))
  (with-input-from-file boundaries-filename
    (lambda ()
      (let loop ((line (read-line))
                 (result (list)))
        (cond
         ((eof-object? line) result)
         (else
          (receive (ra dec const) (parse-line line)
            (cond
             ((eq? const constellation)
              (loop (read-line)
                    (cons (celestial-units->radians ra dec)
                          result)))
             ((null? result)
              (loop (read-line) result))
             (else ;; skip rest of file
              result)))))))))

(define (read-constellation-lines constellations)
  (define (parse-verts-line line)
    (match (with-input-from-string line
             (lambda ()
               (let loop ((token (read))
                          (tokens '()))
                 (cond
                  ((eof-object? token) (reverse! tokens))
                  (else (loop (read) (cons token tokens)))))))
      ((key _ra _dec . constellations) (cons key constellations))
      (else #f))) ;;XXX: error - malformed line
  (define (parse-lines-line line)
    (with-input-from-string line
      (lambda ()
        (let* ((ra (read))
               (dec (read))
               (verts (->string (read))))
          (match (string-split verts ":")
            ((vert-a vert-b) (values (string->number vert-a)
                                     (string->number vert-b)
                                     ra dec))
            (else #f)))))) ;;XXX: error - malformed line
  (let ((vertex-keys (map parse-verts-line
                          (with-input-from-file verts-filename
                            read-lines))))
    (define (care-about-verts? a b)
      (and-let* ((aconst (alist-ref a vertex-keys))
                 (bconst (alist-ref b vertex-keys)))
        (and (any (lambda (x) (member x constellations)) aconst)
             (any (lambda (x) (member x constellations)) bconst))))
    (with-input-from-file lines-filename
      (lambda ()
        (let loop ((line (read-line))
                   (current-segment #f)
                   (result (list)))
          (cond
           ((eof-object? line) result)
           (else
            (receive (vert-a vert-b ra dec) (parse-lines-line line)
              (cond
               ((care-about-verts? vert-a vert-b)
                (let ((segment (list vert-a vert-b)))
                  (if (equal? current-segment segment)
                      (loop (read-line)
                            current-segment
                            (cons (cons (celestial-units->radians ra dec)
                                        (car result))
                                  (cdr result)))
                      (loop (read-line)
                            segment
                            (cons (list (celestial-units->radians ra dec))
                                  result)))))
               (else (loop (read-line) current-segment result)))))))))))


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
               (black (color/rgba 0 0 0 255)))
          (for-each
           (match-lambda
             ((x y)
              (let ((x (inexact->exact (round (* (- x xmin) scale))))
                    (y (inexact->exact (round (* (- y ymin) scale)))))
                (image-draw-pixel image black x y))))
           boundary/cartesian2)
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
