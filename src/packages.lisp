(defpackage :easy-websocket
  (:use :cl)
  (:export
   :*app*
   :*client-handler*
   ;; websocket
   :start-websocket-server
   :shutdown-websocket-server
   :inspect-connection
   :inspect-connections
   ;; handlers
   :handle-new-connection
   :handle-message
   :handle-close-connection
   ))
