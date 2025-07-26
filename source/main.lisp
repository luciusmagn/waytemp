(in-package #:waytemp)


(defun run-daemon (cmd)
  (declare (ignore cmd))
  (format t "Starting waytemp daemon...~%")
  (start-daemon)
  (loop while (and *daemon-thread*
                   (bt:thread-alive-p *daemon-thread*))
        do (sleep 1))
  (format t "Daemon stopped~%"))

(defun waytemp/daemon ()
  (clingon:make-command
   :name "daemon"
   :description "Start the daemon (default)"
   :handler #'run-daemon))

(defun waytemp/get ()
  (clingon:make-command
   :name "get"
   :description "Get current temperature"
   :handler
   (lambda (cmd)
     (declare (ignore cmd))
     (~> (send-command 'get-temp)
         (print-response)))))

(defun waytemp/set ()
  (clingon:make-command
   :name "set"
   :description "Set temperature (1000-25000K)"
   :usage "<amount>"
   :handler
   (lambda (cmd)
     (let ((args (clingon:command-arguments cmd)))
       (~> (send-command 'set-temp
                         (or (and args
                                  (parse-integer (first args) :junk-allowed t))
                             (error "Invalid temperature value")))
           (print-response))))))

(defun waytemp/inc ()
  (clingon:make-command
   :name "inc"
   :description "Increase the temperature (default: 100K)"
   :usage "[amount]"
   :handler
   (lambda (cmd)
     (let ((args (clingon:command-arguments cmd)))
       (~> (send-command 'inc-temp
                         (or (and args
                                  (parse-integer (first args) :junk-allowed t))
                             100))
           (print-response))))))

(defun waytemp/dec ()
  (clingon:make-command
   :name "dec"
   :description "Decrease the temperature (default: 100K)"
   :usage "[amount]"
   :handler
   (lambda (cmd)
     (let ((args (clingon:command-arguments cmd)))
       (~> (send-command 'dec-temp
                         (or (and args
                                  (parse-integer (first args) :junk-allowed t))
                             100))
           (print-response))))))

(defun waytemp/gamma ()
  (clingon:make-command
   :name "gamma"
   :description "Set gamma value... if you dare"
   :usage "<0.0..1.0>"
   :handler
   (lambda (cmd)
     (let ((args (clingon:command-arguments cmd)))
       (~> (send-command 'set-gamma
                         (or (read-from-string (first args))
                             100))
           (print-response))))))

(defun waytemp/quit ()
  (clingon:make-command
   :name "quit"
   :description "Kill the daemon"
   :handler
   (lambda (cmd)
     (declare (ignore cmd))
     (~> (send-command 'quit)
         (print-response)))))

(defun waytemp/save-config ()
  (clingon:make-command
   :name "save-config"
   :description "Save the config to current values"
   :handler
   (lambda (cmd)
     (declare (ignore cmd))
     (~> (send-command 'save-config)
         (print-response)))))

(defun waytemp/command ()
  "The waytemp application"
  (clingon:make-command
   :name "waytemp"
   :description "A server-client adjustable blue-light filter written in Common Lisp"
   :version "1.0.0"
   :license "COLL"
   :usage "[command] [args]"
   :authors '("Lukáš Hozda <me@mag.wiki>")
   :sub-commands (list (waytemp/daemon)
                       (waytemp/get)
                       (waytemp/set)
                       (waytemp/inc)
                       (waytemp/dec)
                       (waytemp/gamma)
                       (waytemp/save-config)
                       (waytemp/quit))
   :handler (lambda (cmd)
              (clingon:print-usage cmd t))))

(defun main()
  "Entrypoint -- not too useful to call this in the REPL"
  (let ((app (waytemp/command)))
    (handler-case
        (clingon:run app)
      (error (e)
        (format *error-output* "Error: ~A~%" e)
        (uiop:quit 1))))
  (uiop:quit 0))
