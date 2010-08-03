
(register-plugin! 'fortune
		  (lambda (name fortune-command)
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
		     (list
		      (make-plugin-command
		       (regexp `(: ,name))
		       '*
		       (sprintf "~A: Tell us something funny" name)
		       (lambda (nick reply-to channel all)
			 (let ((fortune
				(with-input-from-pipe
				 fortune-command
				 (lambda ()
				   (read-all)))))
			   (irc:say *con* fortune reply-to))))
		      )
		     '())))

