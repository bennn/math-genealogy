#lang racket/base

;; Given a URL or mathematician ID,
;; Return the mathematician's ancestors as an unsorted set.

(require
  racket/match
  (only-in "rkt-util/html.rkt" html->xexp)
  (only-in net/url call/input-url string->url get-impure-port url->string)
  (only-in racket/list last empty?)
  (only-in racket/set in-set set-add set-count set set-member?)
  (only-in racket/string string-split)
  (only-in sxml car-sxpath sxpath)
)

;; =============================================================================

(define BASE_URL "http://genealogy.math.ndsu.nodak.edu")

(define-syntax-rule (debug msg arg* ...)
  (displayln (string-append "[INFO] " (format msg arg* ...))))

;; Convert an argument (of unknown type) to 
(define (string->start-url arg)
  (cond
    [(string->number arg)
     => id->math-url]
    [(string->url arg)
     => (lambda (x) x)]
    [else
     (name->math-url arg)]))

(define (id->math-url n)
  (string->url (format "~a/id.php?id=~a" BASE_URL n)))

;; Convert a mathematician's name to a math genealogy URL
(define (name->math-url str)
  (error 'name->math-url "Not implemented"))

;; Collect all ancestors of the mathematician at `url`.
(define (ancestors url #:verbose? [verbose? #f])
  (let loop ([unvisited (url->advisor-id* url)] ;; (Listof Natural)
             [visited   (set)] ;; (Setof Natural)
             [acc       (set)]) ;; (Setof String)
    (match unvisited
     ['() acc]
     [(cons id id*)
      #:when (set-member? visited id)
      (loop id* visited acc)]
     [(cons id id*)
      (when verbose? (debug "Visiting '~a'" id))
      (define a-id* (url->advisor-id* (id->math-url id)))
      (loop (append a-id* id*)
            (set-add visited id)
            (add-name* acc a-id*))])))

;; Convert a list of id's to names, add to the set `acc`.
;; (Very special-purpose)
(define (add-name* acc id*)
  (for/fold ([acc acc]) ([id (in-list id*)])
    (set-add acc (url->name (id->math-url id)))))

(define (url->sxml url)
  (call/input-url url get-impure-port html->xexp))

(define (url->advisor-id* url)
  (sxml->advisor-id* (url->sxml url)))

(define (sxml->advisor-id* sx)
  ;; Find <p> with text "Advisor", scrape all a@href
  (define sx* ((sxpath `(// p ,(contains? '(*text*) "Advisor") a @ href *text*)) sx))
  (map href->id sx*))

(define (href->id str)
  (cond
   [(regexp-match "^id\\.php\\?id=([0-9]+)$" str)
    => (lambda (match*) (string->number (cadr match*)))]
   [else #f]))

;; Scrape a mathematician's name from a URL
(define (url->name url)
  (sxml->name (url->sxml url)))

(define (sxml->name sx)
  (last (string-split ((car-sxpath '(// title *text*)) sx) " - ")))

;; Pretty-print a set of ancestors
(define (display-ancestors name a-set)
  (define title (format "Ancestors of ~a:" name))
  (displayln title)
  (displayln (make-string (string-length title) #\=))
  (for ([a (in-set a-set)])
    (displayln a)))

;; Write a set of ancestors as data
;; Should ideally keep the tree structure
(define (write-ancestors name a-set)
  (format "~a" (cons name a-set)))

(define ((contains? sel part) elem* ???)
  ;; Order of arguments is for my selfish purposes
  (define (contains-part? elem)
    (regexp-match? part ((car-sxpath sel) elem)))
  (filter contains-part? elem*))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  ;; --
  (define output-file (make-parameter #f))
  (define verbose? (make-parameter #f))
  ;; --
  (command-line
    #:program "Math Genealogy scraper"
    #:once-any
    [("-o" "--output") o-param "Location to save results" (output-file o-param)]
    [("-v" "--verbose") "Print debugging information" (verbose? #t)]
    #:args (ID-or-URL)
    (begin
      (define url (string->start-url ID-or-URL))
      (define name (sxml->name (url->sxml url)))
      (define a-set (ancestors url #:verbose? (verbose?)))
      (when verbose? (debug "Finished scraping ~a ancestors." (set-count a-set)))
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

  ;; -- url->advisor-url*
  (check-equal? (url->advisor-id* (id->math-url EULER-ID)) (list BERNOULLI-ID))
  (check-equal? (url->advisor-id* (id->math-url ZASIUS-ID)) '())

)
