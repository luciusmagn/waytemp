(in-package #:waytemp)

(defun print-usage ()
  "Print usage information."
  (format t "Usage: waytemp [command] [args]~%~%")
  (format t "Commands:~%")
  (format t "  daemon          Start the daemon~%")
  (format t "  get             Get current temperature~%")
  (format t "  set <temp>      Set temperature (1000-25000K)~%")
  (format t "  inc [amount]    Increase temperature (default: 100K)~%")
  (format t "  dec [amount]    Decrease temperature (default: 100K)~%")
  (format t "  gamma <value>   Set gamma value~%")
  (format t "  quit            Stop the daemon~%")
  (format t "~%")
  (format t "If no command given, runs as daemon.~%"))

(defun main ()
  "Main entry point."
  (let ((args (uiop:command-line-arguments)))
    (handler-case
        (cond
          ;; No args or daemon command - run as daemon
          ((or (null args)
               (string= (first args) "daemon"))
           (format t "~&Starting waytemp daemon...~%")
           (start-daemon)
           ;; Keep main thread alive
           (loop while (and *daemon-thread*
                            (bt:thread-alive-p *daemon-thread*))
                 do (sleep 1))
           (format t "~&Daemon stopped.~%"))

          ;; Help
          ((member (first args) '("help" "-h" "--help") :test #'string=)
           (print-usage))

          ;; Client commands
          (t
           (let* ((command-list (parse-client-args args))
                  (response (apply #'send-command command-list)))
             (print-response response))))

      (error (e)
        (format *error-output* "~&Error: ~A~%" e)
        (uiop:quit 1))))

  (uiop:quit 0))
