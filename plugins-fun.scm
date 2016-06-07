(use utils)

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
                       (irregex `(: ,name))
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

(register-plugin! 'dance
                  (lambda (name)
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
                       (irregex `(: ,name))
                       '*
                       (sprintf "~A: Dance for us" name)
                       (lambda (nick reply-to channel all)
                         (let loop ((moves '(":D|-<" ":D/-<" ":D\\-<" ":D|-<")))
                           (if (null? moves)
                               (void)
                               (begin
                                 (irc:say *con* (car moves) reply-to)
                                 (if (not (null? (cdr moves)))
                                     (sleep 1))
                                 (loop (cdr moves)))))))
                      )
                     '())))

(define (roll-dice num sides)
  (let* ((rolls (list-tabulate num (lambda (i) (+ 1 (random sides)))))
         (total (fold + 0 rolls)))
    (if (> num 1)
        (sprintf "~Ad~A: ~A (~A)" num sides total (string-join (map number->string rolls) "+"))
        (sprintf "~Ad~A: ~A" num sides total))))

(register-plugin! 'roll
                  (lambda (name)
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
                       (irregex `(: ,name (+ whitespace) (=> num (+ digit)) ("dD") (=> sides (+ digit))))
                       '*
                       (sprintf "~A <num>d<sides>: Roll <num> <sides>-sided dice (eg, 2d6)" name)
                       (lambda (nick reply-to channel all num* sides*)
                         (let ((num (string->number num*))
                               (sides (string->number sides*)))
                          (if (and (<= num 50)
                                   (> num 0)
                                   (<= sides 100000)
                                   (> sides 0))
                              (irc:say *con* (roll-dice num sides) reply-to)
                              (irc:say *con* "I can only roll from 1-50 dice of 1-100000 sides..." reply-to)))
                         ))
                      )
                     '())))
