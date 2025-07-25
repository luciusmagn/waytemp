(in-package #:waytemp)

;;; FFI definitions for Wayland
(cffi:define-foreign-library libwayland-client
  (:unix "libwayland-client.so"))

(cffi:use-foreign-library libwayland-client)

;; Basic types
(cffi:defctype wl-display :pointer)
(cffi:defctype wl-registry :pointer)
(cffi:defctype wl-output :pointer)
(cffi:defctype wl-proxy :pointer)
(cffi:defctype wl-interface :pointer)

;; wl_interface structure
(cffi:defcstruct wl-interface
  (name :string)
  (version :int)
  (method-count :int)
  (methods :pointer)
  (event-count :int)
  (events :pointer))

;; Core Wayland functions
(cffi:defcfun "wl_display_connect" wl-display
  (name :pointer))

(cffi:defcfun "wl_display_disconnect" :void
  (display wl-display))

(cffi:defcfun "wl_display_roundtrip" :int
  (display wl-display))

(cffi:defcfun "wl_display_dispatch" :int
  (display wl-display))

(cffi:defcfun "wl_display_get_registry" wl-registry
  (display wl-display))

(cffi:defcfun "wl_registry_bind" wl-proxy
  (registry wl-registry)
  (name :uint32)
  (interface :pointer)
  (version :uint32))

(cffi:defcfun "wl_proxy_add_listener" :int
  (proxy wl-proxy)
  (implementation :pointer)
  (data :pointer))

(cffi:defcfun "wl_proxy_destroy" :void
  (proxy wl-proxy))

;; POSIX functions with prefix to avoid conflicts
(cffi:defcfun ("mmap" c-mmap) :pointer
  (addr :pointer)
  (length :size)
  (prot :int)
  (flags :int)
  (fd :int)
  (offset :long))

(cffi:defcfun ("munmap" c-munmap) :int
  (addr :pointer)
  (length :size))

(cffi:defcfun ("ftruncate" c-ftruncate) :int
  (fd :int)
  (length :long))

(cffi:defcfun ("mkstemp" c-mkstemp) :int
  (template :pointer))

(cffi:defcfun ("unlink" c-unlink) :int
  (pathname :pointer))

(cffi:defcfun ("close" c-close) :int
  (fd :int))

(cffi:defcfun ("lseek" c-lseek) :long
  (fd :int)
  (offset :long)
  (whence :int))

;; External interfaces we need
(cffi:defcvar ("wl_output_interface" wl-output-interface) (:struct wl-interface))
(cffi:defcvar ("wl_registry_interface" wl-registry-interface) (:struct wl-interface))

;; Protocol constants
(defconstant +prot-read+ 1)
(defconstant +prot-write+ 2)
(defconstant +map-shared+ 1)
(defconstant +seek-set+ 0)

;; wlr-gamma-control protocol opcodes
(defconstant +zwlr-gamma-control-v1-set-gamma+ 0)
(defconstant +zwlr-gamma-control-v1-destroy+ 1)

;;; Registry listener
(cffi:defcallback registry-global :void
    ((data :pointer)
     (registry wl-registry)
     (name :uint32)
     (interface :string)
     (version :uint32))
  (declare (ignore data))
  (cond
    ((string= interface "wl_output")
     (format t "~&Registry: adding output ~D~%" name)
     (init-output-listener)  ; Make sure listener is initialized
     (let* ((output-ptr (wl-registry-bind registry name
                                          (cffi:foreign-slot-pointer
                                           (cffi:foreign-symbol-pointer "wl_output_interface")
                                           '(:struct wl-interface) 'name)
                                          4))
            (output (make-wl-output :id name
                                    :wl-output-ptr output-ptr
                                    :enabled t)))
       (setf (gethash name *outputs*) output)
       (wl-proxy-add-listener output-ptr
                              *output-listener-impl*
                              (cffi:null-pointer))))

    ((string= interface "zwlr_gamma_control_manager_v1")
     (format t "~&Found gamma control manager~%")
     (setf *gamma-control-manager*
           (wl-registry-bind registry name
                             (cffi:foreign-slot-pointer
                              (get-gamma-control-manager-interface)
                              '(:struct wl-interface) 'name)
                             1)))))

(cffi:defcallback registry-global-remove :void
    ((data :pointer)
     (registry wl-registry)
     (name :uint32))
  (declare (ignore data registry))
  (let ((output (gethash name *outputs*)))
    (when output
      (format t "~&Registry: removing output ~D~%" name)
      (remhash name *outputs*))))

;; Registry listener vtable
(cffi:defcstruct registry-listener
  (global :pointer)
  (global-remove :pointer))

(defparameter *registry-listener* nil)

(defun init-registry-listener ()
  (unless *registry-listener*
    (setf *registry-listener*
          (cffi:foreign-alloc '(:struct registry-listener)))
    (setf (cffi:foreign-slot-value *registry-listener*
                                   '(:struct registry-listener) 'global)
          (cffi:callback registry-global))
    (setf (cffi:foreign-slot-value *registry-listener*
                                   '(:struct registry-listener) 'global-remove)
          (cffi:callback registry-global-remove))))


;;; Output listener
(cffi:defcallback output-done :void
    ((data :pointer)
     (wl-output wl-output))
  (declare (ignore data wl-output))
  ;; When output is done, set up gamma control
  (maphash (lambda (id output)
             (declare (ignore id))
             (when (and (wl-output-enabled output)
                        (not (wl-output-gamma-control-ptr output))
                        *gamma-control-manager*)
               (setup-gamma-control output)))
           *outputs*))

;; We need stub callbacks for unused output events
(cffi:defcallback output-geometry :void
    ((data :pointer) (output wl-output) (x :int) (y :int)
     (width :int) (height :int) (subpixel :int)
     (make :string) (model :string) (transform :int))
  (declare (ignore data output x y width height subpixel make model transform)))

(cffi:defcallback output-mode :void
    ((data :pointer) (output wl-output) (flags :uint32)
     (width :int) (height :int) (refresh :int))
  (declare (ignore data output flags width height refresh)))

(cffi:defcallback output-scale :void
    ((data :pointer) (output wl-output) (scale :int))
  (declare (ignore data output scale)))

(cffi:defcallback output-name :void
    ((data :pointer) (output wl-output) (name :string))
  (declare (ignore data output name)))

(cffi:defcallback output-description :void
    ((data :pointer) (output wl-output) (description :string))
  (declare (ignore data output description)))

(cffi:defcstruct output-listener
  (geometry :pointer)
  (mode :pointer)
  (done :pointer)
  (scale :pointer)
  (name :pointer)
  (description :pointer))

(defparameter *output-listener-impl* nil)

(defun init-output-listener ()
  (unless *output-listener-impl*
    (setf *output-listener-impl*
          (cffi:foreign-alloc '(:struct output-listener)))
    (setf (cffi:foreign-slot-value *output-listener-impl*
                                   '(:struct output-listener) 'geometry)
          (cffi:callback output-geometry))
    (setf (cffi:foreign-slot-value *output-listener-impl*
                                   '(:struct output-listener) 'mode)
          (cffi:callback output-mode))
    (setf (cffi:foreign-slot-value *output-listener-impl*
                                   '(:struct output-listener) 'done)
          (cffi:callback output-done))
    (setf (cffi:foreign-slot-value *output-listener-impl*
                                   '(:struct output-listener) 'scale)
          (cffi:callback output-scale))
    (setf (cffi:foreign-slot-value *output-listener-impl*
                                   '(:struct output-listener) 'name)
          (cffi:callback output-name))
    (setf (cffi:foreign-slot-value *output-listener-impl*
                                   '(:struct output-listener) 'description)
          (cffi:callback output-description))))

;;; Gamma control protocol
(defun get-gamma-control-manager-interface ()
  "Return pointer to zwlr_gamma_control_manager_v1_interface"
  (let ((interface (cffi:foreign-alloc '(:struct wl-interface))))
    (cffi:with-foreign-string (name "zwlr_gamma_control_manager_v1")
      (setf (cffi:foreign-slot-value interface '(:struct wl-interface) 'name)
            name)
      (setf (cffi:foreign-slot-value interface '(:struct wl-interface) 'version)
            1)
      (setf (cffi:foreign-slot-value interface '(:struct wl-interface) 'method-count)
            2)
      (setf (cffi:foreign-slot-value interface '(:struct wl-interface) 'methods)
            (cffi:null-pointer))
      (setf (cffi:foreign-slot-value interface '(:struct wl-interface) 'event-count)
            0)
      (setf (cffi:foreign-slot-value interface '(:struct wl-interface) 'events)
            (cffi:null-pointer)))
    interface))

(cffi:defcallback gamma-control-gamma-size :void
    ((data :pointer)
     (gamma-control :pointer)
     (ramp-size :uint32))
  (declare (ignore data gamma-control))
  (maphash (lambda (id output)
             (declare (ignore id))
             (when (cffi:pointer-eq (wl-output-gamma-control-ptr output) gamma-control)
               (setf (wl-output-ramp-size output) ramp-size)
               (when (> ramp-size 0)
                 (multiple-value-bind (fd table-ptr)
                     (create-gamma-table ramp-size)
                   (setf (wl-output-table-fd output) fd)
                   (setf (wl-output-table-ptr output) table-ptr)))))
           *outputs*))

(cffi:defcallback gamma-control-failed :void
    ((data :pointer)
     (gamma-control :pointer))
  (declare (ignore data))
  (format *error-output* "~&Gamma control failed~%")
  (wl-proxy-destroy gamma-control))

(cffi:defcstruct gamma-control-listener
  (gamma-size :pointer)
  (failed :pointer))

(defparameter *gamma-control-listener* nil)

(defun init-gamma-control-listener ()
  (unless *gamma-control-listener*
    (setf *gamma-control-listener*
          (cffi:foreign-alloc '(:struct gamma-control-listener)))
    (setf (cffi:foreign-slot-value *gamma-control-listener*
                                   '(:struct gamma-control-listener) 'gamma-size)
          (cffi:callback gamma-control-gamma-size))
    (setf (cffi:foreign-slot-value *gamma-control-listener*
                                   '(:struct gamma-control-listener) 'failed)
          (cffi:callback gamma-control-failed))))


(defun setup-gamma-control (output)
  "Set up gamma control for an output"
  (init-gamma-control-listener)  ; Make sure listener is initialized
  (let ((gamma-control (cffi:foreign-funcall-pointer
                        *gamma-control-manager*
                        ()
                        wl-proxy *gamma-control-manager*
                        wl-proxy (wl-output-wl-output-ptr output)
                        :pointer)))
    (setf (wl-output-gamma-control-ptr output) gamma-control)
    (wl-proxy-add-listener gamma-control
                           *gamma-control-listener*
                           (cffi:null-pointer))))


(defun create-gamma-table (ramp-size)
  "Create anonymous file and mmap for gamma table"
  (let* ((table-size (* ramp-size 3 2)) ; 3 channels, 2 bytes per value
         (template "/tmp/waytemp-XXXXXX"))
    (cffi:with-foreign-string (tpl template)
      (let ((fd (c-mkstemp tpl)))
        (when (< fd 0)
          (error "Failed to create temp file"))

        (c-ftruncate fd table-size)
        (c-unlink tpl)

        (let ((data (c-mmap (cffi:null-pointer) table-size
                            (logior +prot-read+ +prot-write+)
                            +map-shared+ fd 0)))
          (when (cffi:null-pointer-p data)
            (c-close fd)
            (error "Failed to mmap"))
          (values fd data))))))

(defun write-gamma-table (table-ptr ramp-size wp gamma)
  "Fill gamma table with calculated values"
  (let ((r (rgb-r wp))
        (g (rgb-g wp))
        (b (rgb-b wp)))
    (dotimes (i ramp-size)
      (let ((val (/ (coerce i 'double-float) (1- ramp-size))))
        (setf (cffi:mem-aref table-ptr :uint16 i)
              (round (* 65535 (expt (* val r) (/ 1.0d0 gamma))))
              (cffi:mem-aref table-ptr :uint16 (+ i ramp-size))
              (round (* 65535 (expt (* val g) (/ 1.0d0 gamma))))
              (cffi:mem-aref table-ptr :uint16 (+ i (* 2 ramp-size)))
              (round (* 65535 (expt (* val b) (/ 1.0d0 gamma)))))))))

(defun set-gamma-control (output)
  "Apply gamma table to output"
  (c-lseek (wl-output-table-fd output) 0 +seek-set+)

  ;; Call zwlr_gamma_control_v1_set_gamma
  (cffi:foreign-funcall-pointer
   (wl-output-gamma-control-ptr output)
   ()
   :pointer (wl-output-gamma-control-ptr output)
   :int +zwlr-gamma-control-v1-set-gamma+
   :int (wl-output-table-fd output)
   :void))

;;; Main connection handling
(defparameter *display* nil)
(defparameter *registry* nil)
(defparameter *gamma-control-manager* nil)

(defun init-wayland-connection ()
  "Initialize Wayland connection"
  (setf *display* (wl-display-connect (cffi:null-pointer)))
  (when (cffi:null-pointer-p *display*)
    (error "Failed to connect to Wayland display"))

  (init-registry-listener)  ; Initialize listener before use
  (setf *registry* (wl-display-get-registry *display*))
  (wl-proxy-add-listener *registry* *registry-listener* (cffi:null-pointer))

  (wl-display-roundtrip *display*)

  (unless *gamma-control-manager*
    (error "Compositor doesn't support wlr-gamma-control-unstable-v1"))

  (format t "~&Connected to Wayland compositor~%"))

(defun cleanup-wayland-connection ()
  "Clean up Wayland resources"
  (when *display*
    (wl-display-disconnect *display*)
    (setf *display* nil
          *registry* nil
          *gamma-control-manager* nil)))
