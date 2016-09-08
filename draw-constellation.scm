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

(define tau 6.283185307179586)

(define (square x) (* x x))

(define (distance3 p1 p2)
  (match-let (((x1 y1 z1) p1)
              ((x2 y2 z2) p2))
    (sqrt
     (+ (square (- x1 x2))
        (square (- y1 y2))
        (square (- z1 z2))))))

(define (celestial->cartesian ra dec distance)
  (let ((theta (* tau (/ ra 24.0)))
        (phi (* tau (/ (- 90.0 dec) 360.0))))
    (list
     (* distance (cos theta) (sin phi))
     (* distance (sin theta) (sin phi))
     (* distance (cos phi)))))

(define (celestial->spherical ra dec distance)
  (list
   (* tau (/ ra 24.0))
   (* tau (/ (- 90.0 dec) 360.0))
   distance))

(define (cartesian->celestial x y z)
  (let ((r (sqrt (+ (square x) (square y) (square z)))))
    (list
     (* 24.0 (/ (atan y x) tau))
     (* 360.0 (/ (asin (/ z r)) tau))
     r)))

(define (cartesian-center points)
  (map
   (lambda (x) (/ x (length points)))
   (fold (lambda (point sum) (map + point sum))
         '(0 0 0)
         points)))

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
                    (cons (list ra dec) result)))
             ((null? result)
              (loop (read-line) result))
             (else ;; skip rest of file
              result)))))))))

(define (main options)
  (let* ((constellation (alist-ref 'constellation options))
         (boundary/celestial (read-boundary constellation))
         (boundary/cartesian
          (map (match-lambda ((ra dec) (celestial->cartesian ra dec 1.0)))
               boundary/celestial))
         (center/cartesian (cartesian-center boundary/cartesian))
         (center/celestial (apply cartesian->celestial center/cartesian))
         (center/cartesian (cons 1.0 (cdr center/cartesian)))) ;; we draw from x=1.0
    (fmt #t
         ;; (pretty boundary/cartesian) nl
         "center/cartesian: " center/cartesian nl
         "center/celestial: " center/celestial nl)
    (let* ((center/spherical (apply celestial->spherical center/celestial))
           (center-theta #;0 (car center/spherical))
           (center-phi #;(* 0.25 tau) (cadr center/spherical))
           (boundary/spherical (map (match-lambda ((ra dec) (celestial->spherical ra dec 1.0))) boundary/celestial))
           (boundary/cartesian2
            (map
             (match-lambda
               ((theta phi distance)
                (let* (;; stereographic
                       ;;
                       ;; (k (/ 2.0 (+ 1.0
                       ;;              (* (sin center-phi) (sin phi))
                       ;;              (* (cos center-phi) (cos phi) (cos (- theta center-theta))))))
                       ;; (x (* k (cos phi) (sin (- theta center-theta))))
                       ;; (y (* k (- (* (cos center-phi) (sin phi))
                       ;;            (* (sin center-phi) (cos phi) (cos (- theta center-theta))))))

                       ;; gnomonic
                       ;;
                       ;; (cos-c (+ (* (sin center-phi) (sin phi))
                       ;;           (* (cos center-phi) (cos phi) (cos (- theta center-theta)))))
                       ;; (x (/ (* (cos phi) (sin (- theta center-theta)))
                       ;;       cos-c))
                       ;; (y (/ (- (* (cos center-phi) (sin phi)) (* (sin center-phi) (cos phi) (cos (- theta center-theta))))
                       ;;       cos-c))

                       ;; lambert azimuthal equal area
                       ;;
                       (k (sqrt (/ 2.0 (+ 1.0 (* (sin center-phi) (sin phi)) (* (cos center-phi) (cos phi) (cos (- theta center-theta)))))))
                       (x (* k (cos phi) (sin (- theta center-theta))))
                       (y (* k (- (* (cos center-phi) (sin phi)) (* (sin center-phi) (cos phi) (cos (- theta center-theta))))))
                       )

                  #;(fmt #t theta " " phi " -> " x " " y nl)
                  (list x y))))
             boundary/spherical))
           (radius (fold (lambda (point radius)
                           (max radius (sqrt (apply + (map square point)))))
                         0
                         boundary/cartesian2)))
      (fmt #t "radius: " radius nl)
      ;; drawing
      (let* ((scale (alist-ref 'scale options))
             (wid (inexact->exact (ceiling (* 2 radius scale))))
             (hei (inexact->exact (ceiling (* 2 radius scale))))
             (image (image-create wid hei))
             (image-filename (string-append (string-downcase (->string constellation)) ".png"))
             (black (color/rgba 0 0 0 255)))
        (fmt #t "wid: " wid nl
             "hei: " hei nl)
        (for-each
         (match-lambda
           ((x y)
            (let ((x (inexact->exact (round (* scale (+ radius x)))))
                  (y (inexact->exact (round (* scale (+ radius y))))))
              (fmt #t x " " y nl)
              (image-draw-pixel image black x y))))
         boundary/cartesian2)
        (image-save image image-filename)))

    #;(let* ((scale (alist-ref 'scale options))
           (wid (inexact->exact (ceiling (* radius scale 2))))
           (hei (inexact->exact (ceiling (* radius scale 2))))
           (image (image-create wid hei))
           (image-filename (string-append (string-downcase (->string constellation)) ".png"))
           ;; 'center' the constellation at celestial 0 0.  celestial
           ;; points converted to cartesian will then have a y value that
           ;; maps to x in the image, and a z value that maps to -y
           (boundary/celestial/centered
            (close-loop
             (map (match-lambda ((ra dec)
                                 (list (- ra (car center/celestial))
                                       (- dec (cadr center/celestial)))))
                  boundary/celestial)))
           (boundary/cartesian/centered
            (map (match-lambda ((ra dec) (celestial->cartesian ra dec 1.0)))
                 boundary/celestial/centered))
           (black (color/rgba 0 0 0 255)))
      ;; plot the boundary
      (for-each
       (match-lambda
         ((_ x y)
          (fmt #t x " " y " -> ")
          (let ((x (inexact->exact (round (* scale (- radius x)))))
                (y (inexact->exact (round (* scale (- radius y))))))
            (fmt #t x " " y nl)
            (image-draw-pixel image black x y))))
       boundary/cartesian/centered)
      (image-save image image-filename))))

(define (usage-header)
  (fmt #f "usage: draw-constellation [options] <const>" nl nl
       " const: IAU abbreviation of a constellation (ori)" nl))

(define opts
  (list
   (args:make-option (h help) #:none
                     "help"
     (fmt #t (usage-header) nl (args:usage opts) nl)
     (exit 1))

   (args:make-option (o options-file) #:required
                     "load additional options alist from file")

   (args:make-option (scale) #:required
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
     (fmt #t "Too many operands" nl)
     (exit 1))))
