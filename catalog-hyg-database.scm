
(module catalog-hyg-database
    *

(import chicken scheme)

(use (srfi 1 13)
     data-structures
     extras
     fmt
     (only list-utils zip-alist)
     matchable
     ports
     posix
     regex
     utils)  

(define tau (* 4.0 (asin 1.0)))

(define (degrees->radians d)
  (* tau (/ d 360.0)))

(define (hours->radians h)
  (* tau (/ h 24.0)))

;; Options
;;

(define hyg-database-field-types
  `((absmag . ,string->number)
    #;(bayerflamsteed)
    (ci . ,string->number)
    (colorindex . ,string->number)
    (comp . ,string->number)
    (comp_primary . ,string->number)
    (con . ,(o string->symbol string-downcase))
    (dec . ,(o degrees->radians string->number))
    (decrad . ,string->number)
    (dist . ,string->number)
    (distance . ,string->number)
    (hip . ,string->number)
    (hd . ,string->number)
    #;(hr)
    #;(gliese)
    (id . ,string->number)
    (lum . ,string->number)
    (mag . ,string->number)
    (pmdec . ,string->number)
    (pmdecrad . ,string->number)
    (pmra . ,string->number)
    (pmrarad . ,string->number)
    #;(propername)
    (ra . ,(o hours->radians string->number))
    (rarad . ,string->number)
    (rv . ,string->number)
    (starid . ,string->number)
    #;(spectrum)
    (var_min . ,string->number)
    (var_max . ,string->number)
    (vx . ,string->number)
    (vy . ,string->number)
    (vz . ,string->number)
    (x . ,string->number)
    (y . ,string->number)
    (z . ,string->number)))

(define hyg-database-fields #f)
(define hyg-database-field-converters #f)

(define hyg-database (make-parameter "hygdata_v3.csv"))
;;(define hyg-database (make-parameter "hygfull.csv"))


;; Utils
;;

(define (abort-program msg #!optional (status 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (fmt #t "error: " msg nl)
      (exit status))))

(define (file-readable? file)
  (and (file-exists? file)
       (regular-file? file)
       (file-read-access? file)))


;; HYG Database
;;

(define (hyg-get-records/pattern pattern)
  (with-input-from-pipe
   (string-join
    (map qs (list "grep" pattern (hyg-database)))
    " ")
   read-lines))

;;XXX: this implementation isn't thorough about the checking the
;;     designator.  it just grabs the first line that matches the
;;     designator from a grep.
(define (hyg-get-records/designator designator)
  (zip-alist
   hyg-database-fields
   (map
    (match-lambda ((proc data) (proc data)))
    (zip
     hyg-database-field-converters
     (string-split
      (first ;;XXX: if no lines were found, this gives an error (car)
       (let ((des (->string designator)))
         (or
          (cond
           ;; Hipparcos designator
           ((number? designator)
            ;;XXX: assumes it's the second field
            (hyg-get-records/pattern (string-append "^[^,]\\+," des ",")))

           ;; Bayer designator (AlpHer, Alp2Her)
           ((string-match '(: ($ upper (+ lower))
                              ($ (? numeric))
                              ($ (+ alpha)))
                          des)
            => (lambda (m)
                 (let ((greekletter (list-ref m 1))
                       (superscript (list-ref m 2))
                       (constellation (list-ref m 3)))
                   (hyg-get-records/pattern
                    (string-append ",[0-9]*" greekletter
                                   (if (not (string-null? superscript))
                                       (string-append " *" superscript " *")
                                       "[0-9]\\? *[0-9]\\?")
                                   constellation)))))

           ;; Flamsteed designator (95Her)
           ((string-match '(: ($ (+ numeric))
                              ($ (+ any)))
                          des)
            => (lambda (m)
                 (let ((num (list-ref m 1))
                       (constellation (list-ref m 2)))
                   (hyg-get-records/pattern
                    (string-append "," num "[A-Za-z ]\\+" constellation)))))

           (else
            (abort-program (fmt #f "failed to parse star designator: " des))))
          (abort-program (fmt #f "star designator not found in database: " des)))))
      "," #t)))))

(define (hyg-get-records/constellation constellation #!optional (pred identity))
  (let* ((pattern (fmt #f "," (string-titlecase (->string constellation)) ","))
         (records-unfiltered (hyg-get-records/pattern pattern)))
    (filter
     (lambda (rec)
       (and (eq? constellation (alist-ref 'con rec))
            (pred rec)))
     (map
      (lambda (rec)
        (zip-alist
         hyg-database-fields
         (map
          (match-lambda ((proc data) (proc data)))
          (zip hyg-database-field-converters
               (string-split rec "," #t)))))
      records-unfiltered))))

(unless (file-readable? (hyg-database))
  (abort-program
   (fmt #f "HYG database (" (hyg-database) ") not found or not readable.\n"
        "  Obtain hygfull.csv from https://github.com/astronexus/HYG-Database/")))
(let ((hyg-database-fields-spec
       (with-input-from-file (hyg-database) read-line)))
  (set! hyg-database-fields
    (map (o string->symbol string-downcase) (string-split hyg-database-fields-spec ",")))
  (set! hyg-database-field-converters
    (map (lambda (field)
           (or (alist-ref field hyg-database-field-types)
               identity))
         hyg-database-fields)))

)
