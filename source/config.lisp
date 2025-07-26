(in-package :waytemp)

(defclass config ()
  ((temperature :initarg :temperature :accessor config-temperature
                :type integer
                :documentation "The temperature in Kelvin as integer")
   (gamma       :initarg :gamma       :accessor config-gamma
                :type double-float
                :documentation "The gamma as a double float (e.g. 1.0d0 -- don't forgor the suffix lol)")))


(defmethod print-object ((obj config) stream)
  (if *print-readably*
      (format stream
              "~@<(make-instance '~s :temperature ~s :gamma ~s)~>"
              (class-name (class-of obj))
              (config-temperature obj)
              (config-gamma obj))
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream "temp: ~a gamma: ~a"
                (config-temperature obj)
                (config-gamma obj)))))

(defun config-load (path)
  (with-open-file (stream path)
    (let ((*package* (find-package :waytemp))
          (result nil))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (setf result (eval form)))
      (if (typep result 'config)
          result
          (error "The last form of your config should evaluate to an instance of the class WAYTEMP:CONFIG")))))

(defun config-save (config path)
  (ensure-directories-exist (uiop:pathname-directory-pathname path))
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede)
    (let ((*print-readably* t))
      (print config stream))))

(defun config-path ()
  (let* ((config-home (or (uiop:xdg-config-home)
                          (merge-pathnames (user-homedir-pathname)
                                           (make-pathname :directory '(:relative ".config")))))
         (config-path (~> config-home
                          (merge-pathnames (make-pathname :directory '(:relative "waytemp")) _)
                          (merge-pathnames "config.lisp" _))))
    config-path))
