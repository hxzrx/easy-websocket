(in-package :easy-websocket)


;; Websocket handlers

(defun handle-new-connection (conn-obj env)
  "Handle new incoming websocket CONNECTIONS with ID from boot page."
  (declare (websocket-driver.ws.server:server conn-obj)
           (list env))
  (let* ((query   (getf env :query-string))
         (items   (when query (quri:url-decode-params query)))
         (conn-id (when items (cdr (assoc "r" items :test #'equalp)))))
    (handler-case
        (cond ((and conn-id (gethash conn-id *id->connection-data*))
               (log:info "Reconnection id - ~A to ~A~%" conn-id conn-obj)
               (handler-case
                   (websocket-driver:close-connection (gethash conn-id *connection->id*)
                                                      "Aborting this old connection since receiving a reconnection request.")
                 (condition (c)
                   (log:error "Failed to close the old connection when establishing reconnection. This can be normal: The old connection could probably don't work for the client, so the client is requesting to reconnect.~%Condition - ~A.~&"
                              c)))
               (setf (gethash conn-id *connection->id*) conn-obj)
               (setf (gethash conn-obj *id->connection*) conn-id))
              (conn-id
               (log:info "Reconnection id ~A not found. Closing the connection.~%" conn-id)
               (websocket-driver:close-connection conn-obj)) ; Don't send the reason for better security.
              (t
               (let ((new-id (random-hex-string)))
                 (setf (gethash conn-obj *connection->id*) new-id)
                 (setf (gethash new-id *id->connection*) conn-obj)
                 (setf (gethash new-id *id->connection-data*)
                       (make-sync-hash-table :test #'equal))
                 (setf (gethash "conn-id" (get-connection-data new-id)) new-id)
                 (log:info "Send conn-id back for new conn: ~d" new-id)
                 (websocket-driver:send conn-obj
                                        (format nil "conn_id=~A" new-id))
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (handler-case
                        (funcall *on-connect-handler* new-id)
                      (condition (c)
                        (format t "Condition caught connection ~A - ~A.~&" new-id c)
                        (values 0 c))))
                  :name (format nil "Websocket connection ~A"
                                new-id)))))
      (condition (c)
        (format t "Condition caught in handle-new-connection - ~A.~&" c)
        (values 0 c)))))

;; A very simple message handler
(defun echo-message (conn-obj message)
  "Send the message back to the client"
  (websocket-driver:send conn-obj message))

(defun handle-message (conn-obj message)
  "Handle incoming websocket MESSAGE on CONNECTION."
  (declare (websocket-driver.ws.server:server conn-obj))
  (handler-case
      (let ((conn-id (gethash conn-obj *connection->id*))
            (ml (ppcre:split ":" message :limit 2)))
        (log:info "connection id: ~d" conn-id)
        (log:info "type of conn: ~s" (type-of conn-obj))
        (cond ((null conn-id)
               ;; a zombie connection
               (log:info "A zombie connection ~A. Websocket server doesn't remember its connection-id. Closing it.~%"
                         conn-obj)
               (websocket-driver:close-connection conn-obj)) ; don't send the reason for better security
              ((equal (first ml) "0")
               ;; a ping
               (log:info "Connection ~A    Ping~%" conn-id))
              ((equal (first ml) "E")
               ;; an event
               (let* ((em (ppcre:split " " (second ml) :limit 2))
                      (event-id (first em))
                      (data (second em)))
                 (log:info "Connection ~A    Hook = ~A    Data = ~A~%"
                         conn-id event-id data)
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (handler-case
                        (let* ((event-hash (get-connection-data conn-id))
                               (event      (when event-hash
                                             (gethash event-id
                                                      event-hash))))
                          (when event
                            (funcall event data)))
                      (t (c)
                            (log:error "Condition caught in handle-message for event - ~A.~&" c)
                        (values 0 c))))
                  :name (log:error "Websocket event handler ~A"
                                event-id))))
              (t
               ;; a JavaScript execution result
               (let ((server-query-id (first ml))
                     (browser-returned-answer (second ml)))
                 (log:info "Connection ~A    ~A = ~A    ~A = ~A~%"
                           conn-id
                           'server-query-id
                           server-query-id
                           'browser-returned-answer
                           browser-returned-answer)
                 (setf (gethash (parse-integer server-query-id) *queries*) browser-returned-answer)
                 (bordeaux-threads:signal-semaphore
                  (gethash (parse-integer server-query-id) *queries-sems*))))))
    (condition (c)
      (log:error "Condition caught in handle-message - ~A.~&" c)
      (values 0 c))))

(defun handle-close-connection (conn-obj)
  "Close a websocket connection CONN."
  (declare (websocket-driver.ws.server:server conn-obj))
  (handler-case
      (let ((conn-id (gethash conn-obj *connection->id*)))
        (when conn-id
          (log:info "Connection id ~A has closed. ~A~%" conn-id conn-obj)
          (remove-connection conn-id conn-obj)))
    (condition (c)
      (log:error "Condition caught in handle-message - ~A.~&" c)
      (values 0 c))))

#+:ignore
(start-websocket-server (lambda (&rest args) (log:info "ON-CONNECT-HANDLER print, conn-id = ~s" (car args)))
                        #'handle-new-connection
                        ;;#'handle-message
                        #'echo-message
                        #'handle-close-connection)
