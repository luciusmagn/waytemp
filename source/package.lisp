(defpackage #:waytemp
  (:use #:cl)
  (:import-from #:clingon)
  (:import-from #:serapeum #:~>)
  (:export #:main
           #:start-daemon
           #:stop-daemon
           #:send-command))
