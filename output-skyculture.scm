
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

(define (output-skyculture options)
  #f)

)
