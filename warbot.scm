(use irc)
(use regex)
(use regex-case)
(use matchable)

(use posix)
(use srfi-1)
(use srfi-13)
(use postgresql)
(use sql-null)

;; TODOS:
;; * Make WarBot store the users/channels in Postgres, not in a silly
;;   text file.

(define-record user
  name
  password
  powers ;; list of symbols, or * for all
  op-channels) ;; list of channels to op on, or * for all

(define-record nick
  name
  last-sent-help ;; #f if never sent help
  authenticated-user ;; #f if not authenticated
  channels ;; list of channels present on
  op-channels) ;; list of channels with ops on

(define-record channel
  name
  nicks ;; list of nicks in the channel
  ops ;; list of op nicks in the channel
  )

(define-record plugin-command
  regexp ; RE to match commands
  power-needed ; #t for public command, or a symbol
  help ; help text
  handle!) ; lambda (nick send-replies-to channel . re-matches)

(define-record plugin
  name
  enable! ; plugin enabled (configuration...)
  disable! ; plugin disabled ()
  join! ; nick has joined channel (nick channel)
  leave! ; nick has left channel (nick channel)
  speak! ; nick has spoken text in channel (nick channel text)
  channel-commands ; list of plugin-commands
  privmsg-commands) ; list of plugin-commands

;; Config

(define *botnick* "WarBot")
(define *irc-server* "irc.warhead.org.uk")
(define *conf-filename* "warbot.conf")
(define *database-filename* "warbot.db")

;; Global state

(define *sql-connection* #f)

(define *has-oper* #f)

(define *plugin-factories* '())

(define *plugins* '()) ;; Enabled plugins only

(define *bot-user* (make-user *botnick* #f '* '*))

(define *users* '())

(define *nicks* '())

(define *channels* '())

(define *con* #f)

;; Utility accessors

(define (get-nick name)
  (let ((search (assoc name *nicks*)))
    (if search
	(cdr search)
	(let ((new-nick (make-nick name #f #f '() '())))
	  (if (string=? name *botnick*)
	      (nick-authenticated-user-set! new-nick *bot-user*))
	  (set! *nicks* (cons
		       (cons name new-nick)
		       *nicks*))
	  new-nick))))

;; power can be:
;; - a symbol, which must be in the user's power list, or the user's power list is '*
;; - '*, in which case the nick has the power, regardless
;; - #f, in which case the nick does not have the power, regardless
;; - '*authenticated, in which case the nick has the power if it's authenticated
(define (has-power? nick power)
  (and
   power
   (or (eq? power '*)
       (let* ((nick* (get-nick nick))
	      (user (nick-authenticated-user nick*)))
	 (if user
	     (or
	      (eq? power '*authenticated)
	      (eq? (user-powers user) '*)
	      (member power (user-powers user)))
	     #f)))))

(define (is-authenticated? nick)
  (let* ((nick* (get-nick nick))
	 (user (nick-authenticated-user nick*)))
    user))

(define (get-channel name)
  (let ((search (assoc name *channels*)))
    (if search
	(cdr search)
	(let ((new-channel (make-channel name '() '())))
	  (set! *channels* (cons
		       (cons name new-channel)
		       *channels*))
	  new-channel))))

(define (get-user name)
  (let ((search (assoc name *users*)))
    (if search
	(cdr search)
	#f)))

(define (get-authed-user name password)
  (let ((search (assoc name *users*)))
    (if search
	(if (string=? (user-password (cdr search)) password)
	    (cdr search)
	    #f)
	#f)))

(define (for-each-plugin func)
  (for-each func *plugins*))

(define (privmsg-commands)
  (flatten (map privmsg-commands *plugins*)))

(define (channel-commands)
  (flatten (map privmsg-commands *plugins*)))

(define (for-each-privmsg-command nick func)
  (for-each (lambda (plugin)
	      (for-each (lambda (cmd)
			  (if (has-power? nick (plugin-command-power-needed cmd))
			      (func cmd)))
			(plugin-privmsg-commands plugin)))
	    *plugins*))

(define (for-each-channel-command nick func)
  (for-each (lambda (plugin)
	      (for-each (lambda (cmd)
			  (if (has-power? nick (plugin-command-power-needed cmd))
			      (func cmd)))
			(plugin-channel-commands plugin)))
	    *plugins*))

(define (register-plugin! plugin-name plugin-factory)
  (set! *plugin-factories*
	(cons (cons plugin-name plugin-factory) *plugin-factories*)))

(define (force-oper channel nick reason)
  (if *has-oper*
      (let ((command (sprintf "SAMODE ~A +o ~A" channel nick)))
	(printf "~S ~A\n" command reason)
	(irc:command *con* command))))

;; FIXME: Abstract nick-is-in-channel and nick-is-not-in-channel better!

; op? is #t for an op, #f for a non-op, and anything else
; if we don't actually know
(define (nick-is-in-channel! channel nick op?)
  (let ((nick* (get-nick nick))
	(channel* (get-channel channel)))
    (if (not (member nick* (channel-nicks channel*)))
	(channel-nicks-set! channel* (cons nick* (channel-nicks channel*))))
    (if (not (member channel* (nick-channels nick*)))
	(nick-channels-set! nick* (cons channel* (nick-channels nick*))))

    (case op?
      ((#t) (begin
	      (if (not (member nick* (channel-ops channel*)))
		  (channel-ops-set! channel* (cons nick* (channel-ops channel*))))
	      (if (not (member channel* (nick-op-channels nick*)))
		  (nick-op-channels-set! nick* (cons channel* (nick-op-channels nick*))))))
      ((#f) (begin
	      (if (member nick* (channel-ops channel*))
		  (channel-ops-set! channel* (filter (lambda (nick) (not (eq? nick nick*))) (channel-ops channel*))))
	      (if (member channel* (nick-op-channels nick*))
		  (nick-op-channels-set! nick* (filter (lambda (channel) (not (eq? channel channel*))) (nick-op-channels nick*))))
	      (if (string=? nick *botnick*)
		  (force-oper channel *botnick* "Seemed to need them")))))
    (void)))

(define (nick-is-not-in-channel! channel nick)
  (let ((nick* (get-nick nick))
	(channel* (get-channel channel)))

    (if (member nick* (channel-nicks channel*))
	(channel-nicks-set! channel* (filter (lambda (nick) (not (eq? nick nick*))) (channel-nicks channel*))))
    (if (member channel* (nick-channels nick*))
	(nick-channels-set! nick* (filter (lambda (channel) (not (eq? channel channel*))) (nick-channels nick*))))

    (if (member nick* (channel-ops channel*))
	(channel-ops-set! channel* (filter (lambda (nick) (not (eq? nick nick*))) (channel-ops channel*))))
    (if (member channel* (nick-op-channels nick*))
	(nick-op-channels-set! nick* (filter (lambda (channel) (not (eq? channel channel*))) (nick-op-channels nick*))))

    ; If a nick is gone from all channels, de-auth it
    (if (zero? (length (nick-channels nick*)))
	(nick-authenticated-user-set! nick* #f))

    (void)))

(define (is-op? channel nick)
  (let* ((channel* (get-channel channel))
	 (nick* (get-nick nick))
	 (ops (channel-ops channel*)))
    (member nick* ops)))

(define (is-in? channel nick)
  (let* ((channel* (get-channel channel))
	 (nick* (get-nick nick))
	 (nicks (channel-nicks channel*)))
    (member nick* nicks)))

(define (ensure-channel-membership! channel)
  (if (assoc channel *channels*)
      #t ; We're already there
      (begin
	(irc:join *con* channel)
	(irc:action *con* "is ready for action" channel)
	(set! *channels*
	      (cons
	       (cons channel (make-channel channel '() '()))
	       *channels*)))))

(define (load-user! name password powers op-channels)
  (let ((existing-user (get-user name)))
    (if existing-user
	(begin
	  (user-password-set! existing-user password)
	  (user-powers-set! existing-user powers)
	  (user-op-channels-set! existing-user op-channels))
	(begin
	  (let ((user (make-user name password powers op-channels)))
	    (set! *users* (cons
			   (cons name user)
			   *users*)))))))

(define (load-module! plugin)
  (load plugin))

(define (get-plugin-factory plugin factories)
  (cond
   ((null? factories) #f)
   ((eq? plugin (caar factories))
    (cdar factories))
   (else (get-plugin-factory plugin (cdr factories)))))

(define (enable-plugin! plugin config)
  (let ((plugin-factory (get-plugin-factory plugin *plugin-factories*)))
    (if plugin-factory
	(let ((plugin* (apply plugin-factory config)))
	  ((plugin-enable! plugin*))
	  (set! *plugins*
		(cons plugin* *plugins*)))
	(printf "Could not enable unknown plugin ~A\n" plugin))))

(define (disable-plugins!)
  (for-each
   (lambda (plugin)
     ((plugin-disable! plugin)))
   *plugins*)
  (set! *plugins* '()))

(define (load-config!* configuration)
  (if (null? configuration)
      (void)
      (begin
	(match (car configuration)
	       (('load module) (load-module! module))
	       (('plugin plugin . config) (enable-plugin! plugin config))
	       (('database connection) (set! *sql-connection* (connect connection)))
	       (('oper user pass) (irc:command *con* (sprintf "OPER ~A ~A" user pass)))
	       (else (printf "ERROR: Unknown configuration directive: ~S\n" (car configuration))))
	(load-config!* (cdr configuration)))))

(define (load-config! configuration)
  (disable-plugins!)
  (if *sql-connection* (disconnect *sql-connection*))
  (load-config!* configuration))

(define (load-database! db)
  (if (null? db)
      (void)
      (begin
	(match (car db)
	       (('channel chan) (ensure-channel-membership! chan))
	       (('user user pass powers op-channels) (load-user! user pass powers op-channels))
	       (else (printf "ERROR: Unknown database directive: ~S\n" (car db))))
	(load-database! (cdr db)))))

(define (save-database)
  (for-each
   (lambda (channel-pair)
     (write `(channel ,(car channel-pair)))
     (newline)) *channels*)
  (for-each
   (lambda (user-pair)
     (write `(user ,(car user-pair)
		   ,(user-password (cdr user-pair))
		   ,(user-powers (cdr user-pair))
		   ,(user-op-channels (cdr user-pair))))
     (newline)) *users*))

(define (save-database!)
  (with-output-to-file *database-filename* save-database))

;; Utilities for the SQL db

;; A schema is an alist mapping table name strings to SQL strings
;; that create the table if it doesn't exist.
(define (ensure-db-schema schema)
  (for-each (lambda (table)
	      (let ((matches (car (row-values (query
					       *sql-connection*
					       "SELECT COUNT(*) FROM pg_tables WHERE schemaname = 'public' AND tablename = $1"
					       (car table))))))
		(if (zero? matches)
		   (query *sql-connection* (cdr table)))))
	    schema))

;; High-level event handlers

(define (process-status-dump recipient)
  (for-each (lambda (user-pair)
	      (let ((user (cdr user-pair)))
		(irc:say *con*
			 (sprintf "user ~s powers ~s ops ~s" (user-name user) (user-powers user) (user-op-channels user))
			 recipient)))
	    *users*)
  (for-each (lambda (nick-pair)
	      (let ((nick (cdr nick-pair)))
		(irc:say *con*
			 (sprintf "nick ~s (auth: ~s) is in ~s and op in ~s" (nick-name nick) (if (nick-authenticated-user nick) (user-name (nick-authenticated-user nick)) #f) (map channel-name (nick-channels nick)) (map channel-name (nick-op-channels nick)))
			 recipient)))
	    *nicks*)
  (for-each (lambda (channel-pair)
	      (let ((channel (cdr channel-pair)))
		(irc:say *con*
			 (sprintf "chan ~s contains ~s and ~s ops" (channel-name channel) (map nick-name (channel-nicks channel)) (map nick-name (channel-ops channel)))
			 recipient)))
	    *channels*))

(define (check-ops nick noisy?)
  (and-let* ((nick* (get-nick nick))
	     (user* (nick-authenticated-user nick*))
	     (channels (user-op-channels user*))
	     (op-channels (if (eq? '* channels)
			      (map channel-name (nick-channels nick*))
			      channels)))
	    (for-each (lambda (channel)
			(if (is-op? channel *botnick*)
			    (if (is-op? channel nick)
				(if noisy? (irc:action *con* (sprintf "need not give ops in ~A as you already have them" channel) nick))
				(begin
				  (if noisy? (irc:action *con* (sprintf "gives ops in ~A" channel) nick))
				  (irc:command *con* (sprintf "MODE ~A +o ~A" channel nick))
				  (nick-is-in-channel! channel nick #t)))
			    (if noisy? (irc:action *con* (sprintf "cannot give ops in ~A as I don't have them myself!" channel) nick))))
		      op-channels)))

(define (process-attempt-auth nick user pass)
  (let* ((user* (get-authed-user user pass))
	 (nick* (get-nick nick)))
    (if user*
	(begin
	  (nick-authenticated-user-set! nick* user*)
	  (irc:say *con* (sprintf "Welcome, ~A" user) nick)
	  (check-ops nick #t))
	(begin
	  (irc:say *con* "I'm sorry, that username/password is invalid" nick)))))

(define (process-reload nick)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (exn)
	(irc:say *con* "Error reloading: ~S\n" plugin exn)
	(k (void)))
      (lambda ()
	  (load-config! (with-input-from-file *conf-filename* read-file))
	    (irc:say *con* "Reloaded configuration" nick))))))

(define (older-than timestamp age)
  (or
   (not timestamp)
   (< timestamp (- (current-seconds) age))))

(define (send-help nick)
  (if (older-than (nick-last-sent-help (get-nick nick)) 60)
   (let ((suggest-command (lambda (text)
			    (irc:say *con* (sprintf " ~A" text) nick))))
     (nick-last-sent-help-set! (get-nick nick) (current-seconds))

     (irc:say *con* "Here are the commands you can PM me:" nick)
     
     (suggest-command "auth <user> <pass>: Authenticate yourself to me")
     
     (if (has-power? nick '*authenticated)
	 (suggest-command "op me: Ask me to op you in channels you're authorised for"))
     
     (if (has-power? nick 'dump)
	 (suggest-command "dump: Send a status dump"))
     (if (has-power? nick 'reload)
	 (suggest-command "reload: Reload configuration file"))
     
     (for-each-privmsg-command nick
			       (lambda (command)
				 (suggest-command
				  (plugin-command-help command))))
     
     (for-each-channel-command nick
			       (lambda (command)
				 (suggest-command
				  (plugin-command-help command))))
     
     (irc:say *con* "That's all." nick))))

(define (privmsg nick body)
  (regex-case
   body
   ("dump" _
    (if (has-power? nick 'dump)
	(process-status-dump nick)
	(send-help nick)))
   ("auth +([^ ]*) +([^ ]*)" (all user pass)
    (process-attempt-auth nick user pass))
   ("reload" _
    (if (has-power? nick 'reload)
	(process-reload nick)
	(send-help nick)))
   ("op me" _
    (check-ops nick #t))
   (else
    (let ((matches 0))
      (for-each-privmsg-command nick
       (lambda (command)
	 (let ((match (string-match (plugin-command-regexp command) body)))
	   (if match
	       (begin
		 (apply (plugin-command-handle! command) (append (list nick nick #f) match))
		 (set! matches (+ matches 1)))))))

      (for-each-channel-command nick
       (lambda (command)
	 (let ((match (string-match (plugin-command-regexp command) body)))
	   (if match
	       (begin
		 (apply (plugin-command-handle! command) (append (list nick nick #f) match))
		 (set! matches (+ matches 1)))))))

      (if (= matches 0)
	  (send-help nick))))))

(define *mention-re*
  (regexp `(seq bos ,*botnick* ":" (* whitespace) (submatch (* any)) eos) #t))

(define (send-channel-help nick)
  (if (older-than (nick-last-sent-help (get-nick nick)) 60)
   (let ((suggest-command (lambda (help)
			    (irc:say *con* (sprintf " ~A: ~A" *botnick* help) nick))))
     (nick-last-sent-help-set! (get-nick nick) (current-seconds))

     (irc:say *con* "Here are the commands you can use in channels I'm listening to:" nick)
     (for-each-channel-command nick
			       (lambda (command)
				 (suggest-command
				  (plugin-command-help command))))
     (irc:say *con* "That's all." nick))))

(define (channel-mention nick channel body)
  (nick-is-in-channel! channel nick 'unknown)
  (let* ((match (string-match *mention-re* body))
	 (content (cadr match)))
    (let ((matches 0))
      (for-each-channel-command nick
       (lambda (command)
	 (let ((match (string-match (plugin-command-regexp command) content)))
	   (if match
	       (begin
		 (apply (plugin-command-handle! command) (append (list nick channel channel) match))
		 (set! matches (+ matches 1)))))))

      (if (= matches 0)
	  (send-channel-help nick)))))

;; Low-level handlers

(define (join message)
  (let ((channel (irc:message-receiver message))
	(nick (irc:message-sender message)))
    (nick-is-in-channel! channel nick 'unknown)
    (for-each-plugin
     (lambda (plugin)
       ((plugin-join! plugin)
	nick
	channel)))
    (check-ops nick #f)))

(define (part message)
  (let ((channel (irc:message-receiver message))
	(nick (irc:message-sender message)))
    (for-each-plugin
     (lambda (plugin)
       ((plugin-leave! plugin)
	nick
	channel)))
    (nick-is-not-in-channel! channel nick)))

(define (quit message)
  (let* ((nick (irc:message-sender message))
	 (nick* (get-nick nick)))
    (for-each
     (lambda (channel)
       (for-each-plugin
	(lambda (plugin)
	  ((plugin-leave! plugin)
	   nick
	   channel)))
       (nick-is-not-in-channel! channel nick))
     (map channel-name (nick-channels nick*)))))

(define (nick-mode message)
  (let* ((channel (car (irc:message-parameters message)))
	 (modeflags (cadr (irc:message-parameters message)))
	 (nick (caddr (irc:message-parameters message))))
    (nick-is-in-channel! channel nick
			 (cond
			   ((string=? "+o" modeflags) #t)
			   ((string=? "-o" modeflags) #f)
			   (else 'unknown)))))

(define (channel-mode message)
  (let ((channel (car (irc:message-parameters message)))
	(modeflags (cadr (irc:message-parameters message))))
    #f))

(define (mode message)
  (if (= 3 (length (irc:message-parameters message)))
      (nick-mode message)
      (channel-mode message))
  #f)

(define (message-dispatch message)
  (let* ((body* (cadr (irc:message-parameters message)))
	 (body (if (irc:extended-data? body*)
		   (irc:extended-data-content body*)
		   body*)))
    (cond
     ((string=? (irc:message-sender message) *botnick*)
      #f) ; Ignore all messages from self (to avoid feedback loops!)
     ((string=? (irc:message-receiver message) *botnick*)
      (privmsg (irc:message-sender message) body))
     ((string-match *mention-re* body)
      (channel-mention  (irc:message-sender message)  (irc:message-receiver message) body))
     (else
      (nick-is-in-channel! (irc:message-receiver message) (irc:message-sender message) 'unknown)
      (for-each-plugin
       (lambda (plugin)
	 ((plugin-speak! plugin)
	  (irc:message-sender message)
	  (irc:message-receiver message)
	  body)))))
    #f))

(define (invite message)
  (let* ((nick (irc:message-sender message))
	 (channel (cadr (irc:message-parameters message))))
    (if (has-power? nick 'invite)
	(ensure-channel-membership! channel))))

(define (names message)
  (let* ((channel (caddr (irc:message-parameters message)))
	 (names (string-split (cadddr (irc:message-parameters message)))))
    (for-each
     (lambda (name)
       (if (eq? #\@ (string-ref name 0))
	   (nick-is-in-channel! channel (string-drop name 1) #t)
	   (nick-is-in-channel! channel name #f)))
     names)))

(define (irc-error message)
  (printf "IRC ERROR: ~A\n" (irc:message-body message)))

(define (login-complete message)
  (load-config! (with-input-from-file *conf-filename* read-file))
  (load-database! (with-input-from-file *database-filename* read-file)))

(define (oper-complete message)
  (set! *has-oper* #t)
  (for-each
   (lambda (channelpair)
     (if (and (is-in? (car channelpair) *botnick*)
	      (not (is-op? (car channelpair) *botnick*)))
	 (force-oper (car channelpair) *botnick* "/OPER completed")))
   *channels*))

(define (test message)
  (printf
   "prefix:~S command:~S timestamp:~S code:~S body:~S parameters:~S index:~S sender:~S receiver:~S\n"
   (irc:message-prefix message)
   (irc:message-command message)
   (irc:message-timestamp message)
   (irc:message-code message)
   (irc:message-body message)
   (irc:message-parameters message)
   (irc:message-index message)
   (irc:message-sender message)
   (irc:message-receiver message))

  #f)

;; Initialisation

(let reconnect ()
  (set! *con* (irc:connection server: *irc-server* nick: *botnick*))

  (irc:connect *con*)

  (set! *has-oper* #f)

  (irc:add-message-handler!
   *con* test)

  (irc:add-message-handler!
   *con* irc-error command: "ERROR")

  (irc:add-message-handler!
   *con* message-dispatch command: "PRIVMSG")

  (irc:add-message-handler!
   *con* join command: "JOIN")

  (irc:add-message-handler!
   *con* part command: "PART")

  (irc:add-message-handler!
   *con* mode command: "MODE")

  (irc:add-message-handler!
   *con* invite command: "INVITE")

  (irc:add-message-handler!
   *con* quit command: "QUIT")

  (irc:add-message-handler!
   *con* names code: 353)

  (irc:add-message-handler!
   *con* login-complete code: 005)

  (irc:add-message-handler!
   *con* oper-complete code: 381)

  ; FIXME: Send a /oper command on login, and then +o myself in channels
  ; when I join

  (condition-case (irc:run-message-loop *con* debug: #t pong: #t)
		  (ex (i/o net)
		      (irc:disconnect *con*)
		      (sleep 10)
		      (irc:connect *con*)
		      (reconnect))))
