
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

(module output-skyculture
    *

(import chicken scheme)

(use (srfi 1 13)
     data-structures
     filepath
     fmt
     matchable
     posix)

(import output-chart)
(import projection)
(import catalog-hyg-database)

(define output-skyculture-options
  '((charts
     (constellation:and)
     (constellation:ant)
     (constellation:aps)
     (constellation:aqr)
     (constellation:aql)
     (constellation:ara)
     (constellation:ari)
     (constellation:aur)
     (constellation:boo)
     (constellation:cae)
     (constellation:cam)
     (constellation:cnc)
     (constellation:cvn)
     (constellation:cma)
     (constellation:cmi)
     (constellation:cap)
     (argonavis
      (draw . (constellation:car
               constellation:pup
               constellation:vel)))
     (constellation:cas)
     (constellation:cen)
     (constellation:cep)
     (constellation:cet)
     (constellation:cha)
     (constellation:cir)
     (constellation:col)
     (constellation:com)
     (constellation:cra)
     (constellation:crb)
     (constellation:crv)
     (constellation:crt)
     (constellation:cru)
     (constellation:cyg)
     (constellation:del)
     (constellation:dor)
     (constellation:dra)
     (constellation:equ)
     (constellation:eri)
     (constellation:for)
     (constellation:gem)
     (constellation:gru)
     (constellation:her)
     (constellation:hor)
     (constellation:hya)
     (constellation:hyi)
     (constellation:ind)
     (constellation:lac)
     (constellation:leo)
     (constellation:lmi)
     (constellation:lep)
     (constellation:lib)
     (constellation:lup)
     (constellation:lyn)
     (constellation:lyr)
     (constellation:men)
     (constellation:mic)
     (constellation:mon)
     (constellation:mus)
     (constellation:nor)
     (constellation:oct)
     (ophiuchus-serpens
      (draw . (constellation:oph constellation:ser)))
     (constellation:ori)
     (constellation:pav)
     (constellation:peg)
     (constellation:per)
     (constellation:phe)
     (constellation:pic)
     (constellation:psc)
     (constellation:psa)
     (constellation:pyx)
     (constellation:ret)
     (constellation:sge)
     (constellation:sgr)
     (constellation:sco)
     (constellation:scl)
     (constellation:sct)
     (constellation:sex)
     (constellation:tau)
     (constellation:tel)
     (constellation:tri)
     (constellation:tra)
     (constellation:tuc)
     (constellation:uma)
     (constellation:umi)
     (constellation:vir)
     (constellation:vol)
     (constellation:vul))))

(define (output-skyculture output-options options)
  (let* ((projection-name (alist-ref 'projection options))
         (projection (alist-ref projection-name (projections)))
         (output-path (alist-ref 'path output-options)))
    (unless projection
      (fmt #t "Unknown projection: " projection-name nl)
      (exit 1))
    (unless output-path
      (fmt #t "No output path specified" nl)
      (exit 1))
    (cond
     ((file-exists? output-path)
      (cond
       ((directory output-path)
        ;;XXX: we should require a command line option to overwrite
        (fmt #t "Warning: writing to existing directory: " output-path nl))
       (else
        (fmt #t output-path " exists and is not a skyculture directory." nl)
        (exit 1))))
     (else
      (create-directory output-path #t)))

    (let* ((constellationsart.fab-path (filepath:join-path
                                        (list output-path "constellationsart.fab")))
           (constellationsart.fab-port (open-output-file constellationsart.fab-path)))
      (for-each
       (lambda (chart-spec)
         (let* ((chart-spec (make-chart-spec chart-spec))
                (chart-name (chart-spec-name chart-spec))
                (chart-filename (string-append (->string chart-name) ".png"))
                (draw-objects (chart-spec-draw chart-spec))
                (constellations (map second draw-objects))
                (constellation-stars (sort (append-map
                                            (lambda (constellation)
                                              (hyg-get-records/constellation constellation))
                                            constellations)
                                           (lambda (a b)
                                             (< (alist-ref 'mag a) (alist-ref 'mag b)))))
                (stars (append-map
                        (lambda (constellation)
                          (let* ((maxmag 4.0)
                                 (got 0))
                            (take-while
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
                             (sort (hyg-get-records/constellation constellation)
                                   (lambda (a b)
                                     (< (alist-ref 'mag a) (alist-ref 'mag b)))))))
                        constellations)))
           (chart-spec-draw-set! chart-spec
                                 (map (lambda (ob) (cons 'fit ob))
                                      (chart-spec-draw chart-spec)))
           (chart-spec-draw-set! chart-spec (append (chart-spec-draw chart-spec)
                                                    (map (lambda (star) (cons 'star star))
                                                         stars)))
           (chart-spec-path-set! chart-spec (filepath:join-path
                                             (list
                                              output-path
                                              chart-filename)))
           (let* ((projection (projection-fn projection))
                  (chart (draw-chart chart-spec plotter-imlib2 projection options))
                  (celestial->plot (chart-projection chart)))
             (fmt constellationsart.fab-port chart-name " " chart-filename)
             (for-each
              (lambda (star)
                (let ((hip (alist-ref 'hip star))
                      (ra (alist-ref 'ra star))
                      (dec (alist-ref 'dec star)))
                  (match-let
                      (((x y) (celestial->plot ra dec)))
                    (fmt constellationsart.fab-port " " x " " y " " hip))))
              (take constellation-stars 3))
             (fmt constellationsart.fab-port nl))))
       (alist-ref 'charts options)))))

)
