
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
(import catalog-constellation-names)
(import catalog-hyg-database)

(define output-skyculture-options
  '((charts
     (constellation:and
      (reference-stars . (#;hip:3881 #;hip:3092 hip:9640 hip:677 hip:116584)))
     (constellation:ant
      (reference-stars . (hip:51172 hip:47758 hip:48926)))
     (constellation:aps
      (reference-stars . (hip:81852 hip:81065 hip:72370)))
     (constellation:aql
      (reference-stars . (hip:97649 hip:93244 hip:93805)))
     (constellation:aqr
      (reference-stars . (hip:115438 #;hip:109074 #;hip:102618 hip:110395 hip:104459)))
     (constellation:ara
      (reference-stars . (hip:83081 hip:85727 hip:88714)))
     (constellation:ari
      (reference-stars . (hip:13209 hip:13914 hip:8832)))
     (constellation:aur
      (reference-stars . (hip:28380 hip:24608 hip:23015)))
     (constellation:boo
      (reference-stars . (hip:72105 hip:71075 hip:67927)))
     (constellation:cae
      (reference-stars . (hip:21060 hip:21770 hip:21861)))
     (constellation:cam
      (reference-stars . (hip:25110 hip:22783 hip:16228)))
     (constellation:cnc
      (reference-stars . (hip:44066 hip:40526 hip:40843)))
     (constellation:cvn
      (reference-stars . (hip:63901 hip:61317 hip:61309)))
     (constellation:cma
      (reference-stars . (hip:35904 hip:33160 hip:30122)))
     (constellation:cmi
      (reference-stars . (hip:37921 hip:37279 hip:36188)))
     (constellation:cap
      (reference-stars . (hip:107556 hip:100064 hip:102978)))
     (car
      (draw . (constellation:car
               constellation:pup
               constellation:vel))
      (reference-stars . (hip:45238 hip:50191 hip:39757)))
     (constellation:cas
      (reference-stars . (hip:8886 hip:3179 hip:0746)))
     (constellation:cen
      (reference-stars . (hip:68933 hip:71683 hip:56561)))
     (constellation:cep
      (reference-stars . (hip:116727 hip:106032 hip:109492)))
     (constellation:cet
      (reference-stars . (hip:14143 hip:12770 hip:1562)))
     (constellation:cha
      (reference-stars . (hip:60000 hip:51839 hip:40702)))
     (constellation:cir
      (reference-stars . (hip:75323 hip:74824 hip:71908)))
     (constellation:col
      (reference-stars . (hip:30277 hip:28328 hip:25859)))
     (constellation:com
      (reference-stars . (hip:64241 hip:64394 hip:60742)))
     (constellation:cra
      (reference-stars . (hip:93825 hip:90887 hip:92953)))
     (constellation:crb
      (reference-stars . (hip:78493 hip:76952 hip:76127)))
     (constellation:crv
      (reference-stars . (hip:60965 hip:61359 hip:59316)))
     (constellation:crt
      (reference-stars . (hip:58188 hip:55282 hip:54682)))
     (constellation:cru
      (reference-stars . (hip:61084 hip:62434 hip:60718)))
     (constellation:cyg
      (reference-stars . (hip:107310 hip:94779 hip:95947)))
     (constellation:del
      (reference-stars . (hip:102532 hip:102805 hip:101421)))
     (constellation:dor
      (reference-stars . (hip:27890 hip:27100 hip:19893)))
     (constellation:dra
      (reference-stars . (hip:56211 hip:85670 hip:97433)))
     (constellation:equ
      (reference-stars . (hip:104521 hip:104987 hip:105570)))
     (constellation:eri
      (reference-stars . (hip:22109 hip:13701 hip:7588)))
     (constellation:for
      (reference-stars . (hip:14879 hip:13202 hip:13147)))
     (constellation:gem
      (reference-stars . (hip:37740 hip:32362 hip:28734)))
     (constellation:gru
      (reference-stars . (hip:114131 hip:112623 hip:108085)))
     (constellation:her
      (reference-stars . (hip:80170 hip:79992 hip:88794)))
     (constellation:hor
      (reference-stars . (hip:19747 hip:12484 hip:14240)))
     (constellation:hya
      (reference-stars . (hip:64962 hip:55434 hip:43813)))
     (constellation:hyi
      (reference-stars . (hip:9236 hip:2021 hip:17678)))
     (constellation:ind
      (reference-stars . (hip:101772 hip:103227 hip:105319)))
     (constellation:lac
      (reference-stars . (hip:110538 hip:111104 hip:109937)))
     (constellation:leo
      (reference-stars . (hip:57632 hip:49669 hip:47908)))
     (constellation:lmi
      (reference-stars . (hip:53229 hip:46952 hip:50303)))
     (constellation:lep
      (reference-stars . (hip:28910 hip:24244 hip:23685)))
     (constellation:lib
      (reference-stars . (hip:74785 hip:77853 hip:73714)))
     (constellation:lup
      (reference-stars . (hip:74395 hip:70576 hip:75177)))
     (constellation:lyn
      (reference-stars . (hip:44248 hip:36145 hip:30060)))
     (constellation:lyr
      (reference-stars . (hip:92791 hip:91262 hip:92420)))
     (constellation:men
      (reference-stars . (hip:21949 hip:25918 hip:29134)))
     (constellation:mic
      (reference-stars . (hip:105140 hip:103738 hip:102831)))
     (constellation:mon
      (reference-stars . (hip:39863 hip:31978 hip:29651)))
     (constellation:mus
      (reference-stars . (hip:61199 hip:62322 hip:57363)))
     (constellation:nor
      (reference-stars . (hip:78639 hip:79509 hip:80582)))
     (constellation:oct
      (reference-stars . (hip:70638 hip:107089 hip:112405)))
     (oph
      (draw . (constellation:oph constellation:ser))
      (reference-stars . (hip:92946 hip:77233 hip:85755)))
     (constellation:ori
      (reference-stars . (hip:27913 hip:27366 hip:22449)))
     (constellation:pav
      (reference-stars . (hip:100751 hip:98495 hip:86929)))
     (constellation:peg
      (reference-stars . (hip:107315 hip:109410 hip:1067)))
     (constellation:per
      (reference-stars . (hip:18532 hip:15863 hip:13254)))
     (constellation:phe
      (reference-stars . (hip:8837 hip:0765 hip:5348)))
     (constellation:pic
      (reference-stars . (hip:32607 hip:27530 hip:27321)))
     (constellation:psc
      (reference-stars . (hip:4889 hip:9487 hip:114971)))
     (constellation:psa
      (reference-stars . (hip:113246 hip:107608 hip:111954)))
     (constellation:pup
      (no-art . #t))
     (constellation:pyx
      (reference-stars . (hip:41723 hip:42828 hip:42515)))
     (constellation:ret
      (reference-stars . (hip:18597 hip:19780 hip:19921)))
     (constellation:ser
      (no-art . #t))
     (constellation:sge
      (reference-stars . (hip:98920 hip:96757 hip:96837)))
     (constellation:sgr
      (reference-stars . (hip:95168 hip:95294 hip:87072)))
     (constellation:sco
      (reference-stars . (hip:78820 hip:85927 hip:82729)))
     (constellation:scl
      (reference-stars . (hip:115102 hip:116231 hip:4577)))
     (constellation:sct
      (reference-stars . (hip:92175 hip:90595 hip:92814)))
     (constellation:sex
      (reference-stars . (hip:51437 hip:49641 #;hip:48437 hip:50414)))
     (constellation:tau
      (reference-stars . (hip:26451 hip:15900 hip:17999)))
     (constellation:tel
      (reference-stars . (#;hip:91589 #;hip:90422 hip:90568 hip:96341 hip:99120)))
     (constellation:tri
      (reference-stars . (hip:10064 hip:10559 hip:8796)))
     (constellation:tra
      (reference-stars . (hip:77952 hip:74946 hip:82273)))
     (constellation:tuc
      (reference-stars . (hip:2484 hip:114996 hip:110130)))
     (constellation:uma
      (reference-stars . (hip:67301 hip:41704 hip:50372)))
     (constellation:umi
      (reference-stars . (hip:11767 hip:59504 hip:79822)))
     (constellation:vel
      (no-art . #t))
     (constellation:vir
      (reference-stars . (hip:72220 hip:57380 hip:65474)))
     (constellation:vol
      (reference-stars . (hip:44382 hip:35228 hip:34481)))
     (constellation:vul
      (reference-stars . (hip:98543 hip:95771 hip:94703))))))

(define (get-reference-star sym)
  (let* ((s (->string sym))
         (hip (string->number (second (string-split s ":")))))
    (hyg-get-records/designator hip)))

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
      (create-directory output-path #t)
      (fmt #t "Created directory " output-path nl)))

    ;; create info.ini
    ;;
    (with-output-to-file (filepath:join-path (list output-path "info.ini"))
      (lambda ()
        (fmt #t "[info]" nl
             "name = Blank" nl
             "author = Skyculture-blank" nl
             "boundaries = generic" nl
             "descriptionSource = https://github.com/retroj/skyculture-blank/" nl
             )))

    (with-output-to-file (filepath:join-path (list output-path "description.en.utf8"))
      (lambda () (fmt #t "Blank Sky Culture" nl)))

    (let* ((constellationsart.fab-path (filepath:join-path
                                        (list output-path "constellationsart.fab")))
           (constellationsart.fab-port (open-output-file constellationsart.fab-path))
           (constellationship.fab-path (filepath:join-path
                                        (list output-path "constellationship.fab")))
           (constellationship.fab-port (open-output-file constellationship.fab-path))
           (constellation_names.eng.fab-path (filepath:join-path
                                              (list output-path
                                                    "constellation_names.eng.fab")))
           (constellation_names.eng.fab-port (open-output-file
                                              constellation_names.eng.fab-path)))
      (for-each
       (lambda (chart-spec)
         (let* ((chart-spec (make-chart-spec chart-spec))
                (chart-name (chart-spec-name chart-spec))
                (chart-filename (string-append (->string chart-name) ".png"))
                (draw-objects (chart-spec-draw chart-spec))
                (constellations (map second draw-objects))
                (constellation-iau (or (constellation-name-lookup chart-name)
                                       `((abbreviation . ,chart-name) (name . ,chart-name))))
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

           (fmt constellationship.fab-port
                (alist-ref 'abbreviation constellation-iau)
                " 1 "
                (alist-ref 'hip (first constellation-stars))
                " "
                (alist-ref 'hip (first constellation-stars))
                nl)

           (fmt constellation_names.eng.fab-port
                (alist-ref 'abbreviation constellation-iau)
                "\t"
                (wrt (->string (alist-ref 'name constellation-iau)))
                "\t"
                "_(" (wrt (->string (alist-ref 'name constellation-iau))) ")"
                nl)

           (unless (chart-spec-get-property chart-spec 'no-art)
             (chart-spec-draw-set! chart-spec
                                   (map (lambda (ob) (cons 'fit ob))
                                        (chart-spec-draw chart-spec)))
             (chart-spec-draw-set! chart-spec (append (chart-spec-draw chart-spec)
                                                      (map (lambda (star) (cons 'star star))
                                                           stars)))
             (chart-spec-path-set! chart-spec (filepath:join-path
                                               (list output-path chart-filename)))
             (let* ((projection (projection-fn projection))
                    (chart (draw-chart chart-spec plotter-imlib2 projection options))
                    (celestial->plot (chart-projection chart)))
               (fmt constellationsart.fab-port (alist-ref 'abbreviation constellation-iau)
                    " " chart-filename)
               (for-each
                (lambda (des)
                  (let* ((star (get-reference-star des))
                         (hip (alist-ref 'hip star))
                         (ra (alist-ref 'ra star))
                         (dec (alist-ref 'dec star)))
                    (match-let
                        (((x y) (celestial->plot ra dec)))
                      (fmt constellationsart.fab-port " " x " " y " " hip))))
                (chart-spec-get-property chart-spec 'reference-stars))
               (fmt constellationsart.fab-port nl)))))
       (alist-ref 'charts options)))))

)
