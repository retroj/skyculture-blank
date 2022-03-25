
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

(module catalog-constellation-names
    (constellation-name-lookup)

(import scheme)
(import (chicken base))
(import (chicken io))
(import (chicken process))
(import (chicken string))
(import (srfi 1))
(import (srfi 13))
(import fmt)
(import matchable)

(import catalog)

(define catalog-filename "data/constellation-names/iau.csv")

(define (constellation-normalize const)
  (string->symbol (string-downcase (symbol->string const))))


(define (constellation-name-lookup spec)
  (let ((grep-results
         (with-input-from-pipe
          (string-join
           (map qs (list "grep" "-i" (string-append "^" (->string spec)) catalog-filename))
           " ")
          read-lines)))
    (if (null? grep-results)
        #f
        (map cons (list 'abbreviation 'name)
             (map
              string-trim-both
              (string-split (first grep-results)
                            ","))))))

(define-catalog 'constellation-names
  '(constellation-names) constellation-name-lookup)

)
