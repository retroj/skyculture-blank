
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
     )

(define output-skyculture-options
  '((charts
     (and)
     (ant)
     (aps)
     (aqr)
     (aql)
     (ara)
     (ari)
     (aur)
     (boo)
     (cae)
     (cam)
     (cnc)
     (cvn)
     (cma)
     (cmi)
     (cap)
     (argonavis
      (draw . (car pup vel)))
     (cas)
     (cen)
     (cep)
     (cet)
     (cha)
     (cir)
     (col)
     (com)
     (cra)
     (crb)
     (crv)
     (crt)
     (cru)
     (cyg)
     (del)
     (dor)
     (dra)
     (equ)
     (eri)
     (for)
     (gem)
     (gru)
     (her)
     (hor)
     (hya)
     (hyi)
     (ind)
     (lac)
     (leo)
     (lmi)
     (lep)
     (lib)
     (lup)
     (lyn)
     (lyr)
     (men)
     (mic)
     (mon)
     (mus)
     (nor)
     (oct)
     (ophiuchus-serpens
      (draw . (oph ser1 ser2)))
     (ori)
     (pav)
     (peg)
     (per)
     (phe)
     (pic)
     (psc)
     (psa)
     (pyx)
     (ret)
     (sge)
     (sgr)
     (sco)
     (scl)
     (sct)
     (sex)
     (tau)
     (tel)
     (tri)
     (tra)
     (tuc)
     (uma)
     (umi)
     (vir)
     (vol)
     (vul))))

(define (output-skyculture options)
  #f)

)
