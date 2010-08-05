;; The indentation in this file is a travesty. Figure out how to make
;; it look nicer.

(register-plugin! 'tell
		  (lambda (name)
		    (let ((schema '(("nickops_tells" . "CREATE TABLE nickops_tells (sender_nick TEXT, recipient_nick TEXT, channel TEXT, content TEXT, when_written TIMESTAMP)")))
			  (check-messages (lambda (nick channel)
					    (with-transaction *sql-connection*
							      (lambda ()
								(let ((messages (query *sql-connection*
										       "SELECT sender_nick, channel, content, when_written FROM nickops_tells WHERE recipient_nick = $1 AND (channel IS NULL OR channel = $2)" nick channel)))
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
										  "DELETE FROM nickops_tells WHERE recipient_nick = $1 AND sender_nick = $2 AND channel = $3 AND content = $4 and when_written = $5"
										  nick sender channel content when))))
								   messages)
))))))
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
							  (regexp (sprintf "~A +([^ ]+) +(.*)" name) #t)
							  '*
							  (sprintf "~A <nick> <message>: Leave a message for a nick" name)
							  (lambda (from-nick reply-to channel all to-nick message)
							    (query *sql-connection*
								   "INSERT INTO nickops_tells (sender_nick, recipient_nick, channel, content, when_written) VALUES ($1,$2,$3,$4,NOW())"
								   from-nick to-nick
								   (if channel channel (sql-null))  message)
							    (irc:say *con* "I'll pass that on." reply-to)
							    ))
							 )
							'()))))

