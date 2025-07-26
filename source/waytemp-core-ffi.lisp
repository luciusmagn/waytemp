(in-package #:waytemp)

(defparameter *waytemp-core-loaded* nil)

(defun try-load-waytemp-core ()
  "Try to load the waytemp core library from various locations"
  (unless *waytemp-core-loaded*
    (let* ((search-paths
             (list (asdf:system-relative-pathname :waytemp "c/libwaytemp_core.so")
                   #P"/usr/lib/libwaytemp_core.so"
                   #P"/usr/local/lib/libwaytemp_core.so"
                   #P"/usr/lib64/libwaytemp_core.so"
                   "libwaytemp_core.so")))

      (loop for path in search-paths
            when path
              do
                 (handler-case
                     (progn
                       (cffi:load-foreign-library path)
                       (setf *waytemp-core-loaded* t)
                       (format *error-output* "~&Loaded waytemp core from: ~A~%" path)
                       (return t))
                   (error () nil)))

      (unless *waytemp-core-loaded*
        (error "Could not find libwaytemp_core.so")))))

(cffi:defcfun ("waytemp_init" %waytemp-init) :pointer)

(cffi:defcfun ("waytemp_destroy" %waytemp-destroy) :void
  (ctx :pointer))

(cffi:defcfun ("waytemp_set_temperature" %waytemp-set-temperature) :int
  (ctx :pointer)
  (temp :int)
  (gamma :double))

(cffi:defcfun ("waytemp_process_events" %waytemp-process-events) :int
  (ctx :pointer))

(defvar *waytemp-ctx* nil)

(defun init-waytemp-core ()
  "Initialize the waytemp core library"
  (try-load-waytemp-core)
  (when *waytemp-ctx*
    (cleanup-waytemp-core))
  (setf *waytemp-ctx* (%waytemp-init))
  (when (cffi:null-pointer-p *waytemp-ctx*)
    (error "Failed to initialize waytemp core"))
  t)

(defun cleanup-waytemp-core ()
  "Clean up waytemp core resources"
  (when (and *waytemp-ctx* (not (cffi:null-pointer-p *waytemp-ctx*)))
    (%waytemp-destroy *waytemp-ctx*)
    (setf *waytemp-ctx* nil)))

(defun set-temperature-core (temp gamma)
  "Set color temperature using the core library"
  (unless *waytemp-ctx*
    (error "Waytemp core not initialized"))
  (let ((result (%waytemp-set-temperature *waytemp-ctx* temp gamma)))
    (when (< result 0)
      (error "Failed to set temperature"))
    result))

(defun process-waytemp-events ()
  "Process pending Wayland events"
  (unless *waytemp-ctx*
    (error "Waytemp core not initialized"))
  (%waytemp-process-events *waytemp-ctx*))
