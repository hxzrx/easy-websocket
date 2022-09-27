(in-package :easy-websocket)

(defvar *connection->id* (make-sync-hash-table)
  "A map from websocket objects to their id strings")

(defvar *id->connection* (make-sync-hash-table :test #'equal)
  "A map from id strings to their corresponding websocket objects")

(defvar *id->connection-data* (make-sync-hash-table :test #'equal)
  "A map from id strings to their connection based data")

(defvar *app*)
(setf (documentation '*app* 'variable)
      "Clack's app middle-ware")

(defvar *client-handler*)
(setf (documentation '*client-handler* 'variable)
      "Clack's handler for socket traffic")

(defvar *on-connect-handler*)
(setf (documentation '*on-connect-handler* 'variable)
      "New connection event handler.")

(defvar *queries* (make-sync-hash-table)
  "Query ID to Answers")

(defvar *queries-sems* (make-sync-hash-table)
  "Query ID to semiphores")

(defvar *query-time-out* 3
  "Number of seconds to timeout waiting for a query by default")


(defun get-connection-object (conn-id)
  "Get the websocket object associated with CONN-ID."
  (declare (string conn-id))
  (gethash conn-id *id->connection*))

(defun get-connection-id (conn-object)
  "Get the connection id string associated with the websocket object of CONN-OBJECT."
  (declare (websocket-driver.ws.server:server conn-object))
  (gethash conn-object *connection->id*))

(defun get-connection-data (conn-id)
  "Get the connection data associated with CONN-ID"
  (declare (string conn-id))
  (gethash conn-id *id->connection-data*))

(defun remove-connection-object (conn-id)
  "Remove the websocket object associated with CONN-ID."
  (declare (string conn-id))
  (remhash conn-id *id->connection*))

(defun remove-connection-id (conn-object)
  "Remove the connection id string associated with the websocket object of CONN-OBJECT."
  (declare (websocket-driver.ws.server:server conn-object))
  (remhash conn-object *connection->id*))

(defun remove-connection-data (conn-id)
  "Remove the connection data associated with CONN-ID."
  (declare (string conn-id))
  (remhash conn-id *id->connection-data*))

(defun remove-connection (&key conn-id conn-object)
  (declare (string conn-id))
  (declare (websocket-driver.ws.server:server conn-object))
  (clrhash (get-connection-data conn-id))
  (remove-connection-object conn-id)
  (remove-connection-id conn-object)
  (remove-connection-data conn-id)
  (log:info "Connection ~d has been removed." conn-id))


(defun websocket-server (on-open-handler on-message-handler on-close-handler env)
  "Setup websocket server on ENV."
  (declare (function on-open-handler on-message-handler on-close-handler)
           (list env))
  (handler-case
      (let ((ws (websocket-driver:make-server env)))
        (websocket-driver:on :open ws
                             (lambda ()
                               (handler-case
                                   (funcall on-open-handler ws env)
                                 (condition (c)
                                   (format t "Condition caught in websocket-server :open - ~A.~&" c)
                                   (values 0 c)))))
        (websocket-driver:on :message ws
                             (lambda (msg)
                               (handler-case
                                   (funcall on-message-handler ws msg)
                                 (t (c)
                                   (format t "Condition caught in websocket-server :message - ~A.~&" c)
                                   (values 0 c)))))
        (websocket-driver:on :close ws
                             (lambda (&key code reason)
                               (declare (ignore code reason))
                               (handler-case
                                   (funcall on-close-handler ws)
                                 (t (c)
                                   (format t "Condition caught in websocket-server :close - ~A.~&" c)
                                   (values 0 c)))))
        (lambda (responder)
          (declare (ignore responder))
          (websocket-driver:start-connection ws)))
    (t (c)
      (format t "Condition caught in setting up the websocket-server - ~A.~&" c)
      (values 0 c))))

(defun start-websocket-server (on-connect-handler
                               on-open-handler
                               on-message-handler
                               on-close-handler
                               &key
                                 (host    "0.0.0.0")
                                 (port    8080)
                                 (server  :woo)
                                 (workers 1))
  "on-connect-handler: a function that accepts the conn-id as its argument, used to initialize a new websocket connection.
on-open-handler: a function that accepts conn-obj and the env as its arguments, used to listen to the ws msg on open.
on-message-handler: a function that accepts conn-obj and the conn-id as its arguments, used to listen to the ws msg on message.
on-close-handler: a function that accepts conn-obj as its argument, used to listen to the ws msg on close.
"
  (setf *on-connect-handler* on-connect-handler)
  (setf *app*
        (lack:builder
         (lambda (app)
           (lambda (env)
             (log:info "A remote host \"~d:~d\" tries to initialize a connection to this server with request \"~d\"."
                        (getf env :REMOTE-ADDR) (getf env :REMOTE-PORT) (getf env :REQUEST-URI))
             (let ((headers (getf env :headers)))
               (if (equal "websocket" (gethash "upgrade" headers))
                   (funcall app env)
                   (prog1 '(403 (:content-type "text/plain") ("Forbidden!"))
                     (log:warn "The client tried to make a normal http connection to this websocket server."))))))
         (lambda (env)
           (websocket-server on-open-handler on-message-handler on-close-handler env))))
  (setf *client-handler*
        (clack:clackup *app* :server server :address host :port port :worker-num workers))
  (format t "Websocket server started in address \"~A:~A\" by \"~d\" with ~d workers."
          host port server workers))

(defun shutdown-websocket-server ()
  "Shutdown CLOG."
  (loop for conn-data being the hash-values of *id->connection-data*
        do (clrhash conn-data))
  (clack:stop *client-handler*)
  (clrhash *id->connection-data*)
  (clrhash *connection->id*)
  (clrhash *id->connection*)
  (setf *app* nil)
  (setf *client-handler* nil))


;;; Helpers

(defun inspect-connection (conn-id)
  (let ((conn-data (gethash conn-id *id->connection-data*)))
    (if conn-data
        (format t "Connection ~s, result: ~d~%" conn-id conn-data)
        (format t "Connection ~s, result: Not found!" conn-id))))

(defun inspect-connections (&optional show-details-p)
  (format t "The websocket server has ~d connections.~%" (hash-table-count *id->connection-data*))
  (when show-details-p
    (loop for conn-id being the hash-key
            using (hash-value conn-data) of *id->connection-data*
          do (format t "~d: ~d~%" conn-id conn-data))))
