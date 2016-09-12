
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

(module projection
    (projections
     projection-name
     projection-fn

     azimuthal-equidistant
     stereographic)

(import chicken scheme)

(use (srfi 99)
     cells
     matchable)

(define tau (* 4.0 (asin 1.0)))
(define tau/2 (* 2.0 (asin 1.0)))
(define tau/4 (asin 1.0))

(define projections (cell (list)))

(define-record-type :projection
  (make-projection name fn)
  projection?
  (name projection-name)
  (fn projection-fn))

(define (define-projection name fn)
  (projections
   (cons
    (cons name (make-projection name fn))
    (projections))))


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

(define-projection 'azimuthal-equidistant azimuthal-equidistant)


;; Stereographic
;;

(define (stereographic point center)
  (match-let (((ra dec) point)
              ((center-ra center-dec) center))
    (let* ((lam ra)
           (phi dec)
           (lam0 center-ra)
           (phi1 center-dec)
           (R 1.0)
           (dlam (- lam lam0))
           (k (/ (* R 2.0)
                 (+ 1
                    (* (sin phi1) (sin phi))
                    (* (cos phi1) (cos phi) (cos dlam)))))
           (x (* k (cos phi) (sin dlam)))
           (y (* k (- (* (cos phi1) (sin phi))
                      (* (sin phi1) (cos phi) (cos dlam))))))
      (list (- x) (- y)))))

(define-projection 'stereographic stereographic)

)
