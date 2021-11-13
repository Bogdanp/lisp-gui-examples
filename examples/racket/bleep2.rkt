#lang racket

(require racket/gui/easy
         racket/gui/easy/operator
         rsound)

(define notes
  `(("A" . 440.00)
    ("B" . 493.88)
    ("C" . 261.63)
    ("D" . 293.66)
    ("E" . 329.63)
    ("F" . 349.23)
    ("G" . 292.00)))

(define min-duration 1)
(define max-duration 600000)
(define min-position 0)
(define max-position 2000)
(define min-frequency 20)
(define max-frequency 2000)
(define-values (position->frequency frequency->position)
  (let* ([min-frequency (log min-frequency)]
         [max-frequency (log max-frequency)]
         [scale (/ (- max-frequency min-frequency)
                   (- max-position min-position))])
    (define (position->frequency pos)
      (exact-round (exp (+ min-frequency (* scale (- pos min-position))))))
    (define (frequency->position freq)
      (exact-round (/ (- (log freq) min-frequency) (+ scale min-position))))
    (values position->frequency frequency->position)))

(define/obs @frequency 440.0)
(define/obs @duration-ms 200)
(define/obs @tone
  (obs-combine
   (λ (frequency duration-ms)
     (make-tone frequency 0.5 (exact-round (* 44.1 duration-ms))))
   @frequency @duration-ms))

(define (play-tone)
  (parameterize ([default-sample-rate 44100])
    (play (obs-peek @tone))))

(define ((adjust-freq by) freq)
  (max min-frequency (min max-frequency (* freq by))))

(define ((make-text->number [valid? values]) text)
  (define maybe-num
    (and (not (string-suffix? text "."))
         (string->number text)))
  (and maybe-num (valid? maybe-num) maybe-num))

(define frequency-text->frequency
  (make-text->number
   (λ (freq)
     (and (>= freq min-frequency)
          (<= freq max-frequency)))))

(define duration-text->duration
  (make-text->number
   (λ (duration)
     (and (>= duration min-duration)
          (<= duration max-duration)))))

(define (frequency-button label adjust-by)
  (button label (@frequency . λ<~ . (adjust-freq adjust-by))))

(render
 (window
  #:title "Bleep"
  (vpanel
   (hpanel
    #:margin '(10 15)
    (slider
     #:style '(horizontal plain)
     #:min-value min-position
     #:max-value max-position
     (@frequency . ~> . frequency->position)
     (λ (pos) (@frequency . := . (position->frequency pos)))))
   (hpanel
    #:margin '(10 15)
    (spacer)
    (frequency-button "<" 1/2)
    (hpanel
     (input
      @frequency
      #:value=? =
      #:value->text ~a
      (λ (_event text)
        (cond
          [(frequency-text->frequency text) => (λ:= @frequency)]
          [else (void)])))
     (text "Hz"))
    (frequency-button ">" 2.0)
    (spacer))
   (hpanel
    #:margin '(10 15)
    (hpanel
     (input
      #:label "Duration: "
      @duration-ms
      #:value=? =
      #:value->text ~a
      (λ (_event text)
        (cond
          [(duration-text->duration text) => (λ:= @duration-ms)]
          [else (void)])))
     (text "ms"))
    (button "Play" play-tone)
    (choice
     #:label "♪ "
     (map car notes)
     (λ (choice)
       (@frequency . := . (cdr (assoc choice notes)))))))))
