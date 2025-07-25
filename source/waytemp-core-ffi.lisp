(in-package #:waytemp)

(cffi:define-foreign-library waytemp-core
  (:unix (:or "./c/libwaytemp_core.so"
              "libwaytemp_core.so")))

(cffi:use-foreign-library waytemp-core)

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
