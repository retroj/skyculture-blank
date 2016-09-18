
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

(module catalog-pbarbier-constellation-boundaries
    (read-boundary
     read-constellation-lines)

(import chicken scheme)

(use (srfi 1 13)
     data-structures
     extras
     fmt
     matchable
     ports)

(define boundaries-filename "data/constellations-pbarbier/bound_in_20.txt")

(define lines-filename "data/constellations-pbarbier/lines_in_20.txt")

(define verts-filename "data/constellations-pbarbier/verts_18.txt")

(define tau (* 4.0 (asin 1.0)))

(define (celestial-units->radians ra dec)
  (list
   (* tau (/ ra 24.0))
   (* tau (/ dec 360.0))))

(define (constellation-normalize const)
  (string->symbol (string-downcase (symbol->string const))))

(define (%read-boundary constellation)
  (define (parse-line line)
    (with-input-from-string line
      (lambda ()
        (let* ((ra (read))
               (dec (read))
               (const (read)))
          (values ra dec (constellation-normalize const))))))
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

(define (read-boundary constellation)
  (map
   (lambda (constellation)
     (let ((boundary (%read-boundary constellation)))
       (cond
        ((null? boundary)
         (fmt #t "Unknown constellation: " constellation nl)
         (exit 1))
        (else boundary))))
   (if (eq? 'ser constellation)
       (list 'ser1 'ser2)
       (list constellation))))

(define (read-constellation-lines constellations)
  (define (parse-verts-line line)
    (match (with-input-from-string line
             (lambda ()
               (let loop ((token (read))
                          (tokens '()))
                 (cond
                  ((eof-object? token) (reverse! tokens))
                  (else (loop (read) (cons token tokens)))))))
      ((key _ra _dec . constellations)
       (cons key (map constellation-normalize constellations)))
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

)
