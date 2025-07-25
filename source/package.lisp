(defpackage #:waytemp
  (:use #:cl)
  (:import-from #:clingon)
  (:export #:main
           #:start-daemon
           #:stop-daemon
           #:send-command))
