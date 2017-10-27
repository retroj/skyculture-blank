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
     matchable)

(load "catalog.scm")
(load "catalog-pbarbier-constellation-boundaries")
(load "catalog-hyg-database")
(load "projection")
(load "output-chart")
(load "output-skyculture")

(import output-skyculture)


(define (draw-charts options)
  (let* ((output-spec (alist-ref 'output options)))
    (match output-spec
      ((output-scheme . output-options)
       (case output-scheme
         ((skyculture)
          (output-skyculture output-options options))
         (else
          (fmt #t "Unknown output-scheme " output-scheme "." nl)
          (exit 1))))
      (_
       (fmt #t "No output scheme given." nl)
       (exit 1)))))


;;
;; Main
;;

(define (main command-line-arguments)
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
      (args:parse command-line-arguments opts)
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
      (draw-charts (append `((charts . ,charts)) options)))))

(main (command-line-arguments))
