(in-package #:waytemp)

(defparameter *socket-path* "/tmp/waytemp.sock")
(defparameter *current-temp* 6500)
(defparameter *gamma* 1.0d0)
(defparameter *daemon-thread* nil)
(defparameter *should-quit* nil)

(defun handle-client-message (message)
  "Process a client message and return response."
  (handler-case
      (destructuring-bind (command &rest args) message
        (ecase command
          (set-temp
           (let ((temp (first args)))
             (when (and (numberp temp) (>= temp 1000) (<= temp 25000))
               (setf *current-temp* temp)
               (update-temperature)
               `(ok ,*current-temp*))))
          (inc-temp
           (let ((delta (or (first args) 100)))
             (setf *current-temp* (min 25000 (+ *current-temp* delta)))
             (update-temperature)
             `(ok ,*current-temp*)))
          (dec-temp
           (let ((delta (or (first args) 100)))
             (setf *current-temp* (max 1000 (- *current-temp* delta)))
             (update-temperature)
             `(ok ,*current-temp*)))
          (get-temp
           `(ok ,*current-temp*))
          (set-gamma
           (let ((gamma (first args)))
             (when (and (numberp gamma) (> gamma 0))
               (setf *gamma* (coerce gamma 'double-float))
               (update-temperature)
               `(ok ,*gamma*))))
          (get-gamma
           `(ok ,*gamma*))
          (save-config
           (config-save (make-instance 'config
                                       :temperature *current-temp*
                                       :gamma *gamma*)
                        (config-path))
           `(ok saved))
          (quit
           (setf *should-quit* t)
           '(ok bye))))
    (error (e)
      `(error ,(format nil "~A" e)))))

(defun update-temperature ()
  "Update color temperature using the C core"
  (handler-case
      (progn
        (set-temperature-core *current-temp* *gamma*)
        (format t "~&Set temperature to ~D K (gamma: ~,2F)~%"
                *current-temp* *gamma*))
    (error (e)
      (format *error-output* "~&Failed to set temperature: ~A~%" e))))

(defun wake-socket-server ()
  "Connect to our own socket so the accept loop wakes up and sees
*SHOULD-QUIT*."
  (ignore-errors
   (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
     (unwind-protect (sb-bsd-sockets:socket-connect socket *socket-path*)
       (sb-bsd-sockets:socket-close socket)))))

(defun wayland-event-loop ()
  "Process Wayland events while daemon is running.  Dispatching blocks until
the compositor sends something, so an idle daemon never wakes up."
  (loop while (and (not *should-quit*) *waytemp-ctx*)
        do (handler-case
               (when (and *waytemp-ctx* (minusp (process-waytemp-events)))
                 ;; wl_display_dispatch failed: the connection is gone.
                 (return))
             (error (e)
               (format *error-output* "~&Wayland error: ~A~%" e)
               (return)))
           (sleep 0.01))
  ;; The compositor went away: stop the daemon too, so the next session's
  ;; daemon can bind the socket.
  (unless *should-quit*
    (setf *should-quit* t)
    (wake-socket-server)))

(defun unix-socket-server ()
  "Main daemon loop handling client connections."
  (when (probe-file *socket-path*)
    (delete-file *socket-path*))

  (let ((server (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind server *socket-path*)
    (sb-bsd-sockets:socket-listen server 5)
    (unwind-protect
         (progn
           (format t "~&Daemon listening on ~A~%" *socket-path*)

           (let ((wayland-thread (bt:make-thread #'wayland-event-loop
                                                 :name "waytemp-wayland")))
             (unwind-protect
                  (loop until *should-quit* do
                    ;; Block until a client connects.  Quit requests arrive
                    ;; as clients too, and the Wayland thread connects to
                    ;; wake us when the compositor goes away.
                    (when (sb-sys:wait-until-fd-usable
                           (sb-bsd-sockets:socket-file-descriptor server)
                           :input)
                      (let* ((client (sb-bsd-sockets:socket-accept server))
                             (stream (sb-bsd-sockets:socket-make-stream
                                      client :input t :output t
                                      :buffering :full
                                      :element-type 'character)))
                        (unwind-protect
                             (handler-case
                                 (let* ((message (read stream))
                                        (response (handle-client-message message)))
                                   (prin1 response stream)
                                   (terpri stream)
                                   (finish-output stream))
                               (error (e)
                                 (format *error-output*
                                         "~&Error handling client: ~A~%" e)))
                          (close stream)
                          (sb-bsd-sockets:socket-close client)))))
               (setf *should-quit* t)
               (when (bt:thread-alive-p wayland-thread)
                 (bt:destroy-thread wayland-thread)
                 (sleep 0.1)
                 (when (bt:thread-alive-p wayland-thread)
                   (bt:join-thread wayland-thread :timeout 1))))))
      (sb-bsd-sockets:socket-close server)
      (when (probe-file *socket-path*)
        (delete-file *socket-path*)))))

(defun start-daemon (&key foreground)
  "Start the daemon in a background thread, or with FOREGROUND serve clients
in the calling thread until the daemon quits."
  (when (and *daemon-thread* (bt:thread-alive-p *daemon-thread*))
    (error "Daemon already running"))

  (handler-case
      (let ((config (config-load (config-path))))
        (setf *current-temp* (config-temperature config))
        (setf *gamma*        (config-gamma config)))
    (error (e)
      (format t "Could not load config from ~A: ~A~%" (config-path) e)
      (format t "Using default values of:~%")
      (format t "  Temperature:~A~%" *current-temp*)
      (format t "  Gamma~A~%:" *gamma*)))

  (setf *should-quit* nil)

  (handler-case
      (progn
        (init-waytemp-core)
        (update-temperature))
    (error (e)
      (format *error-output* "~&Failed to initialize waytemp core: ~A~%" e)
      (return-from start-daemon nil)))

  (if foreground
      (unix-socket-server)
      (setf *daemon-thread*
            (bt:make-thread #'unix-socket-server
                            :name "waytemp-daemon")))
  t)

(defun stop-daemon ()
  "Stop the daemon gracefully."
  (setf *should-quit* t)
  (when (and *daemon-thread* (bt:thread-alive-p *daemon-thread*))
    (bt:join-thread *daemon-thread*))
  (cleanup-waytemp-core))
