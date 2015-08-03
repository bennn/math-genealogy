#lang racket/base

;; Simple scraper for the Math Genealogy project
;;  genealogy.math.ndsu.nodak.edu
;; Finds all ancestors of a given mathematician

(provide
  ancestors
  ;; (->* [url] [#:verbose? Boolean] (Setof String))
  ;; Given a start URL, return that mathematician's ancestors as an unsorted set
  ;; When `verbose?` is true, print logging information to `(current-output-port)`.
)

(require
  racket/match
  (only-in "rkt-util/html.rkt" html->xexp)
  (only-in net/url call/input-url string->url get-impure-port url->string)
  (only-in racket/list last empty?)
  (only-in racket/set in-set set-add set-count set set-member?)
  (only-in racket/string string-split string-trim)
  (only-in sxml car-sxpath sxpath)
)

;; =============================================================================

(define BASE_URL "http://genealogy.math.ndsu.nodak.edu")

;; Show a debug message, the UI is like printf
(define-syntax-rule (debug msg arg* ...)
  (displayln (string-append "[INFO] " (format msg arg* ...))))

;; =============================================================================

;; Convert an argument (of unknown type) to a URL
;; (: string->start-url (-> Any url))
(define (string->start-url arg)
  (cond
    [(string->number arg)
     => id->math-url]
    [(string->url arg)
     => (lambda (x) x)]
    [else
     (name->math-url arg)]))

;; Convert a mathematician's id to his/her url
;; (: id->math-url (-> Natural url))
(define (id->math-url n)
  (string->url (format "~a/id.php?id=~a" BASE_URL n)))

;; Convert a mathematician's name to a math genealogy URL
;; Should be implemented using quickSearch.php
;; (: name->math-url (-> String url))
(define (name->math-url str)
  (error 'name->math-url "Not implemented"))

;; Collect all ancestors of the mathematician at `url`.
;; (: ancestors (->* [url] [#:verbose Boolean] (Setof String)))
(define (ancestors url #:verbose? [verbose? #f])
  (let loop ([unvisited (url->advisor-id* url)] ;; (Listof Natural)
             [visited   (set)]                  ;; (Setof Natural)
             [acc       (set)])                 ;; (Setof String)
    (match unvisited
     ['()           ;; Nothing else to visit
      acc]
     [(cons id id*) ;; Do nothing if we've seen this part of the tree
      #:when (set-member? visited id)
      (loop id* visited acc)]
     [(cons id id*) ;; Collect the advisors for `id` & loop
      (when verbose? (debug "Visiting '~a'" id))
      (define a-id* (url->advisor-id* (id->math-url id)))
      (loop (append a-id* id*)
            (set-add visited id)
            (add-name* acc a-id*))])))

;; Convert a list of id's to names, add to the set `acc`.
;; (Very special-purpose)
;; (: add-name* (-> (Setof String) (Listof Natural) (Setof String)))
(define (add-name* acc id*)
  (for/fold ([acc acc]) ([id (in-list id*)])
    (set-add acc (url->name (id->math-url id)))))

;; Convert the html at `url` to sxml format
;; (: url->sxml (-> url sxml))
(define (url->sxml url)
  (call/input-url url get-impure-port html->xexp))

;; Get the ids of all advisors at `url`
;; (: url->advisor-id* (-> url (Listof Natural)))
(define (url->advisor-id* url)
  (sxml->advisor-id* (url->sxml url)))

;; Get the ids of all advisors within the sxml
;; (: sxml->advisor-id* (-> sxml (Listof Natural)))
(define (sxml->advisor-id* sx)
  ;; Find <p> with text "Advisor", scrape all a@href for new ids
  (define sx* ((sxpath `(// p ,(contains-text? "Advisor") a @ href *text*)) sx))
  (map href->id sx*))

;; Parse a math genealogy id from a special href
;; (: href->id (-> String Natural))
(define (href->id str)
  (cond
   [(regexp-match "^id\\.php\\?id=([0-9]+)$" str)
    => (lambda (match*) (string->number (cadr match*)))]
   [else #f]))

;; Scrape a mathematician's name from a URL
;; (: url->name (-> url String))
(define (url->name url)
  (sxml->name (url->sxml url)))

;; Scrape a mathematician's name from an sxml document
;; (: sxml->name (-> sxml String))
(define (sxml->name sx)
  (string-trim (last (string-split ((car-sxpath '(// title *text*)) sx) " - "))))

;; Pretty-print a set of ancestors
;; (: display-ancestors (-> String (Setof String) Void))
(define (display-ancestors name a-set)
  (define title (format "Ancestors of ~a:" name))
  (displayln title)
  (displayln (make-string (string-length title) #\=))
  (for ([a (in-set a-set)])
    (displayln a)))

;; Write a set of ancestors as data
;; Should ideally keep the tree structure
;; (: write-ancestors (-> String (Setof String) String))
(define (write-ancestors name a-set)
  (format "~a" (cons name a-set)))

;; sxml filter
;; Return all elements with text matching `part`
;; (2015-08-03: I have no idea what the second argument is)
;; (: contains-text? (-> String (-> (Listof sxml) ??? (Listof sxml))))
(define ((contains-text? part) elem* ???)
  ;; Order of arguments is for my selfish purposes
  ;; (: contains-part? (-> sxml Boolean))
  (define (contains-part? elem)
    (regexp-match? part ((car-sxpath '(*text*)) elem)))
  (filter contains-part? elem*))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  ;; -- parameters
  (define output-file (make-parameter #f))
  (define verbose? (make-parameter #f))
  ;; -- main
  (command-line
    #:program "Math Genealogy scraper"
    #:once-any
    [("-o" "--output") o-param "Location to save results" (output-file o-param)]
    [("-v" "--verbose") "Print debugging information" (verbose? #t)]
    #:args (ID-or-URL)
    (begin
      (define url (string->start-url ID-or-URL))
      (when (verbose?) (debug "Got start URL '~a'" (url->string url)))
      (define name (sxml->name (url->sxml url)))
      (when (verbose?) (debug "Searching for ancestors of '~a'" name))
      (define a-set (ancestors url #:verbose? (verbose?)))
      (when (verbose?) (debug "Finished scraping ~a ancestors." (set-count a-set)))
      (if (output-file)
        (with-output-to-file (output-file)
          (lambda () (displayln (write-ancestors name a-set))))
        (display-ancestors name a-set)))))


;; =============================================================================

(module+ test
  (require rackunit)

  (define EULER-ID 38586)
  (define BERNOULLI-ID 53410)
  (define EGLINGER-ID 129628)
  (define PACIOLI-ID 126888)
  (define ZASIUS-ID 126659)

  ;; -- href->id
  (check-equal? (href->id "id.php?id=69") 69)

  ;; -- id->math-url
  (define-syntax-rule (check-url=? [u1 u2] ...)
    (begin (check-equal? (url->string u1) (url->string u2)) ...))
  (check-url=?
    [(id->math-url EULER-ID) (string->url "http://genealogy.math.ndsu.nodak.edu/id.php?id=38586")]
    [(id->math-url BERNOULLI-ID) (string->url "http://genealogy.math.ndsu.nodak.edu/id.php?id=53410")]
    [(id->math-url EGLINGER-ID) (string->url "http://genealogy.math.ndsu.nodak.edu/id.php?id=129628")]
  )

  ;; -- url->name
  (check-equal? (url->name (id->math-url EULER-ID)) "Leonhard Euler")
  ;; Luca Pacioli has an extra whitespace character in his <title/>
  (check-equal? (url->name (id->math-url PACIOLI-ID)) "Luca Pacioli")

  ;; -- url->advisor-url*
  (check-equal? (url->advisor-id* (id->math-url EULER-ID)) (list BERNOULLI-ID))
  (check-equal? (url->advisor-id* (id->math-url ZASIUS-ID)) '())

)
