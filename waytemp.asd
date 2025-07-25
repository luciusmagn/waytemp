(asdf:defsystem #:waytemp
  :description "Wayland color temperature control daemon"
  :author "Lukáš Hozda"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:cffi
               #:iolib
               #:alexandria
               #:bordeaux-threads)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "color-math")
                 (:file "wayland-bindings")
                 (:file "daemon")
                 (:file "client")
                 (:file "main"))))
  :build-operation "program-op"
  :build-pathname "waytemp"
  :entry-point "waytemp:main")
