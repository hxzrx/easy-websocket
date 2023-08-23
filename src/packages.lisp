(defpackage :easy-websocket
  (:use :cl)
  (:import-from
   :websocket-driver
   :send
   :send-text
   :send-binary
   :send-ping
   :close-connection
   :version
   :protocol
   :ready-state)
  (:export
   :*app*
   :*handler*
   :start
   :stop
   :with-client
   ;; websocket-driver
   :send
   :send-text
   :send-binary
   :send-ping
   :close-connection
   :version
   :protocol
   :ready-state))
