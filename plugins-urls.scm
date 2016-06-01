(use srfi-13)
(use srfi-14)
(use http-client)
(use html-parser)
(use sxpath)
(use intarweb)

(define *summary-length* 80)
(define *timeout* 2)
(define *html-length* 8192)

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
                          ((sxpath '(// html head title))
                           sxml)))
             ;; FIXME: Restrict to p/div/etc. to avoid picking up script tags
             (bodies ((sxpath '(// html body // *text*)) sxml))
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
                        "/")))
             (summary (limit-string body *summary-length*)))
        (sprintf "[~a]: ~a" title summary)))
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
                        "Error fetching page"
                        (describe-url* url)))
   "Timed out"))

#;(begin
 (pp (describe-url "/etc/passwd"))
 (pp (describe-url "http://love.warhead.org.uk:22/"))
 (pp (describe-url "http://www.snell-pym.org.uk/wp-content/themes/magicblue/images/headerbg-eyes.png"))
 (pp (describe-url "http://www.snell-pym.org.uk/"))
 (pp (describe-url "http://www.snell-pym.org.uk/alaric/alaric-foaf.rdf"))
 (pp (describe-url "http://love.warhead.org.uk/~alaric/test.txt"))
 (pp (describe-url "https://www.kitten-technologies.org.uk/")))

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
