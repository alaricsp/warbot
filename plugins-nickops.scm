(register-plugin!
 'tell
 (lambda (name)
   (let ((schema '(("nickops_tells" . "CREATE TABLE nickops_tells (sender_nick TEXT, recipient_nick TEXT, channel TEXT, content TEXT, when_written TIMESTAMP)")))
         (check-messages
          (lambda (nick channel)
            (unless (string=? nick *botnick*)
                    (with-transaction
                     *sql-connection*
                     (lambda ()
                       (let ((messages (query *sql-connection*
                                              "SELECT sender_nick, channel, content, when_written FROM nickops_tells WHERE LOWER(recipient_nick) = LOWER($1) AND (channel IS NULL OR channel = $2)" nick channel)))
                         (row-for-each*
                          (lambda (sender channel content when)
                            (if (sql-null? channel)
                                (begin ;; privmsg
                                  (irc:say *con* (sprintf "~A told me to tell you: ~A" sender content) nick)
                                  (query *sql-connection*
                                         "DELETE FROM nickops_tells WHERE recipient_nick = $1 AND sender_nick = $2 AND channel IS NULL AND content = $3 and when_written = $4"
                                         nick sender content when))
                                (begin ;; in-channel
                                  (irc:say *con* (sprintf "~A: ~A told me to tell you: ~A" nick sender content) channel)
                                  (query *sql-connection*
                                         "DELETE FROM nickops_tells WHERE LOWER(recipient_nick) = LOWER($1) AND sender_nick = $2 AND channel = $3 AND content = $4 and when_written = $5"
                                         nick sender channel content when))))
                          messages))))))))
     (make-plugin
      name
      (lambda ()
        (ensure-db-schema schema)
        (void))
      (lambda ()
        (void))
      (lambda (nick channel)
        (check-messages nick channel)
        (void))
      (lambda (nick channel)
        (void))
      (lambda (nick channel text)
        (check-messages nick channel)
        (void))
      (list
       (make-plugin-command
        (regexp (sprintf "~A +([^ :,;]+)[:,;]? +(.*)" name) #t)
        '*
        (sprintf "~A <nick> <message>: Leave a message for a nick" name)
        (lambda (from-nick reply-to channel all to-nick message)
          (if (string-ci= to-nick *botnick*)
              (begin
                (irc:say *con* "I can't really send myself a message."
                         reply-to))
              (begin
               (query *sql-connection*
                      "INSERT INTO nickops_tells (sender_nick, recipient_nick, channel, content, when_written) VALUES ($1,$2,$3,$4,NOW())"
                      from-nick to-nick
                      (if channel channel (sql-null))  message))
              (irc:say *con* "I'll pass that on." reply-to))))
       )
      '()))))

;; "su" plugin lets nominated users force certain modes
;; Configuration is an alist of /SAMODE commands, in effect, keyed
;; on user name (NOT nick).
;; Each SAMODE is specified as a list of channel name, then
;; a list of channel modes to set, then a list of channel-user modes
;; to set (with the nick of the user specified)
;; Eg:
;; ("bob" ("#test" ("+u") ("+o")) ("#foo" () ("+o")))

(register-plugin! 'su
                  (lambda (name *user-powers*)
                    (make-plugin
                     name
                     (lambda ()
                       (void))
                     (lambda ()
                       (void))
                     (lambda (nick channel)
                       (void))
                     (lambda (nick channel)
                       (void))
                     (lambda (nick channel text)
                       (void))
                     '()
                     (list
                      (make-plugin-command
                       (regexp name)
                       'su
                       (sprintf "~A: Grant you special powers from my database" name)
                       (lambda (nick reply-to channel all)
                         (and-let* ((nick* (get-nick nick))
                                    (user* (nick-authenticated-user nick*))
                                    (user (user-name user*))
                                    (powers (assoc user *user-powers*)))
                                   (for-each
                                    (lambda (channel-powers)
                                      (printf "CHANNEL POWERS: ~S\n" channel-powers)
                                      (let ((channel (car channel-powers))
                                            (chanmodes (cadr channel-powers))
                                            (usermodes (caddr channel-powers)))
                                        (for-each
                                         (lambda (mode)
                                           (printf "CHANNEL MODE: ~S\n" mode)
                                           (irc:command *con* (sprintf "SAMODE ~A ~A" channel mode)))
                                         chanmodes)

                                        (for-each
                                         (lambda (mode)
                                           (printf "USER MODE: ~S\n" mode)
                                           (irc:command *con* (sprintf "SAMODE ~A ~A ~A" channel mode nick)))
                                         usermodes)))
                                    (cdr powers)))))))))
