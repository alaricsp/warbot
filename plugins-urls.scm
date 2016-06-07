(use srfi-13)
(use srfi-14)
(use http-client)
(use html-parser)
(use sxpath)
(use intarweb)

(define *summary-length* 80)
(define *timeout* 5)
(define *html-length* 65536)

(define (fetch url)
  (receive
   (text url response)
   (with-input-from-request url #f
                            (lambda () (read-string *html-length*)))
   (let ((type (symbol->string
                (header-value 'content-type
                              (response-headers response)
                              'application/octet-stream))))
     (if (string=? "text/" (string-take type 5))
         (cons type text)
         (cons type #f)))))

(define (limit-string text len)
  (if (> (string-length text) len)
      (sprintf "~a..." (string-take text len))
      text))

(define (describe-url* url)
  (let* ((content (fetch url))
         (type (car content))
         (body (cdr content)))
    (cond
     ((string=? type "text/html")
      (let* ((sxml (with-input-from-string (string-append body "\"") html->sxml))
             (titles (map cadr
                          ((sxpath '(// head // title))
                           sxml)))
             (bodies ((sxpath "//body//*[self::div or self::p]/text()") sxml))
             (title (if (null? titles) "no title" (string-trim-both (car titles))))
             (body (if (null? bodies)
                       "no page text"
                       (string-join ; Join all nonempty elements texts with /s
                        (filter
                         (lambda (str)
                           (> (string-length str) 0))
                         (map (lambda (body)
                                ;; Remove tedious whitespace
                                (string-trim-both (irregex-replace/all '(+ whitespace) body " ")))
                              bodies))
                        "/"))))
        (sprintf "[~a]: ~a"
                 (limit-string title *summary-length*)
                 (limit-string body *summary-length*))))
     (body (sprintf "[no title]: ~a"
                    (limit-string
                     (string-trim-both (irregex-replace/all '(+ whitespace) body " "))
                     *summary-length*)))
     (else (sprintf "Non-text content (~a)" type)))))

(define (with-timeout seconds thunk default)
  (let ((thread (make-thread thunk)))
    (thread-start! thread)
    (thread-join! thread seconds default)))

(define (describe-url url)
  (with-timeout
   *timeout*
   (lambda ()
     (handle-exceptions exn
                        (sprintf "Error fetching ~s: ~s in ~s"
                                 url
                                 ((condition-property-accessor 'exn 'message "Unknown error") exn)
                                 (cons ((condition-property-accessor 'exn 'location (void)) exn)
                                       ((condition-property-accessor 'exn 'arguments '()) exn)))
                        (describe-url* url)))
   (sprintf "Timed out fetching ~s" url)))

#;(begin
 (pp (describe-url "/etc/passwd"))
 (pp (describe-url "http://love.warhead.org.uk:22/"))
 (pp (describe-url "http://www.snell-pym.org.uk/wp-content/themes/magicblue/images/headerbg-eyes.png"))
 (pp (describe-url "http://www.snell-pym.org.uk/"))
 (pp (describe-url "http://www.snell-pym.org.uk/alaric/alaric-foaf.rdf"))
 (pp (describe-url "http://love.warhead.org.uk/~alaric/test.txt"))
 (pp (describe-url "https://www.kitten-technologies.org.uk/"))
 (pp (describe-url "http://fortune.com/2015/10/15/theranos-elizabeth-holmes-wsj/"))
 (pp (describe-url "https://science.slashdot.org/story/16/06/01/1656211/forbes-just-cut-its-estimate-of-theranos-ceo-elizabeth-holmess-net-worth-from-45-billion-to-zero"))
 (pp (describe-url* "https://en.wikipedia.org/wiki/Archey's_frog")))

(register-plugin!
 'url-watcher
 (lambda (name logdir)
   (define (log-url! nick channel url description)
     (let ((fd (file-open (make-pathname logdir (string-append channel ".log"))
                          (+ open/wronly open/append open/creat))))
       (when fd
             (file-write fd (sprintf "~s\n" (list (current-seconds) nick url description))))
       (file-close fd))
     (when description
      (irc:say *con* description channel)))

   (let ((url-regex (irregex 'http-url)))
    (make-plugin
     name
     (lambda () (void))                 ; enable
     (lambda () (void))                 ; disable
     (lambda (nick channel) (void))     ; join
     (lambda (nick channel) (void))     ; leave
     (lambda (nick channel text)        ; speak
       ;; Look for a URL
       (for-each
        (lambda (url)
          ;; Trim any trailing punctuation, which is likely to not be
          ;; part of the URL
          (let ((trimmed (string-trim-right url (char-set #\. #\; #\, #\! #\? #\) #\> #\] #\' #\"))))
            (handle-exceptions
             exn
             (log-url! nick channel trimmed #f)
             (let ((description (describe-url trimmed)))
               (log-url! nick channel trimmed description)))))
        (irregex-extract url-regex text))
       (void))
     (list)                             ; channel commands
     (list)                             ; privmsg-only commands
     ))))
