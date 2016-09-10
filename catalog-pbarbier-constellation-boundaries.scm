
(module catalog-pbarbier-constellation-boundaries
    *

(import chicken scheme)

(use (srfi 1)
     data-structures
     extras
     matchable
     ports)

(define boundaries-filename "bound_in_20.txt")

(define lines-filename "lines_in_20.txt")

(define verts-filename "verts_18.txt")

(define tau (* 4.0 (asin 1.0)))

(define (celestial-units->radians ra dec)
  (list
   (* tau (/ ra 24.0))
   (* tau (/ dec 360.0))))

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



)
