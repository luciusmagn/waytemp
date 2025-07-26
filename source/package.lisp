(defpackage #:waytemp
  (:use #:cl #:serapeum)
  (:import-from #:clingon)
  (:export #:main
           #:start-daemon
           #:stop-daemon
           #:send-command
           #:config))
