(in-package #:waytemp)

(defun send-command (command &rest args)
  "Send a command to the daemon and return the response."
  (handler-case
      (let ((socket (iolib:make-socket :address-family :local
                                       :type :stream
                                       :connect :active
                                       :remote-filename *socket-path*)))
        (unwind-protect
             (progn
               (prin1 (cons command args) socket)
               (terpri socket)
               (finish-output socket)
               (read socket))
          (close socket)))
    (error (e)
      `(error ,(format nil "Failed to connect to daemon: ~A" e)))))

(defun parse-client-args (args)
  "Parse command line arguments for client mode."
  (when (null args)
    (return-from parse-client-args '(get-temp)))

  (let ((cmd (first args)))
    (cond
      ((string= cmd "get")
       '(get-temp))
      ((string= cmd "set")
       (let ((temp (parse-integer (second args) :junk-allowed t)))
         (if temp
             `(set-temp ,temp)
             (error "Invalid temperature value"))))
      ((string= cmd "inc")
       (let ((delta (if (second args)
                        (parse-integer (second args) :junk-allowed t)
                        100)))
         `(inc-temp ,delta)))
      ((string= cmd "dec")
       (let ((delta (if (second args)
                        (parse-integer (second args) :junk-allowed t)
                        100)))
         `(dec-temp ,delta)))
      ((string= cmd "gamma")
       (let ((gamma (read-from-string (second args))))
         `(set-gamma ,gamma)))
      ((string= cmd "quit")
       '(quit))
      (t
       (error "Unknown command: ~A" cmd)))))

(defun print-response (response)
  "Pretty print daemon response."
  (destructuring-bind (status &rest data) response
    (case status
      (ok
       (when data
         (format t "~&~A~%" (first data))))
      (error
       (format *error-output* "~&Error: ~A~%" (first data))
       (uiop:quit 1))
      (t
       (format t "~&~S~%" response)))))
