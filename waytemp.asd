(asdf:defsystem #:waytemp
  :description "Wayland color temperature control daemon"
  :author "Lukáš Hozda <me@mag.wiki>"
  :license "COLL"
  :version "1.0.1"
  :serial t
  :depends-on (#:cffi
               #:clingon
               #:iolib
               #:alexandria
               #:serapeum
               #:bordeaux-threads)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "waytemp-core-ffi")
                 (:file "daemon")
                 (:file "client")
                 (:file "config")
                 (:file "main"))))
  :build-operation "program-op"
  :build-pathname "waytemp"
  :entry-point "waytemp:main"
  :perform (asdf:compile-op :before (op c)
                            (let ((makefile (asdf:system-relative-pathname "waytemp" "c/Makefile")))
                              (uiop:run-program (list "make" "-C" (namestring (uiop:pathname-directory-pathname makefile)))
                                                :output :interactive
                                                :error-output :interactive))))
