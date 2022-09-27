;;;; A full demo of websocket handlers.
;;;; Most of the code were copied from clog <https://github.com/rabbibotton/clog>

(in-package :easy-websocket)


(defvar *connection->id* (make-sync-hash-table)
  "A map from websocket objects to their id strings")

(defvar *id->connection* (make-sync-hash-table :test #'equal)
  "A map from id strings to their corresponding websocket objects")

(defvar *id->connection-data* (make-sync-hash-table :test #'equal)
  "A map from id strings to their connection based data")

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

(defun remove-connection (conn-id conn-object)
  (declare (string conn-id))
  (declare (websocket-driver.ws.server:server conn-object))
  (clrhash (get-connection-data conn-id))
  (remove-connection-object conn-id)
  (remove-connection-id conn-object)
  (remove-connection-data conn-id)
  (log:info "Connection <~d> has been removed." conn-id))


(defun handle-new-connection (conn-obj env)
  "Handle new incoming websocket CONNECTIONS with ID from boot page."
  (declare (websocket-driver.ws.server:server conn-obj)
           (list env))
  (let* ((query   (getf env :query-string))
         (items   (when query (quri:url-decode-params query)))
         (conn-id (when items (cdr (assoc "r" items :test #'equalp)))))
    (handler-case
        (cond ((and conn-id (gethash conn-id *id->connection-data*))
               (log:info "Reconnection id - ~A to ~A." conn-id conn-obj)
               (handler-case
                   (websocket-driver:close-connection (gethash conn-id *connection->id*)
                                                      "Aborting this old connection since receiving a reconnection request.")
                 (condition (c)
                   (log:error "Failed to close the old connection when establishing reconnection. This can be normal: The old connection could probably don't work for the client, so the client is requesting to reconnect.~%Condition - ~A."
                              c)))
               (setf (gethash conn-id *connection->id*) conn-obj)
               (setf (gethash conn-obj *id->connection*) conn-id))
              (conn-id
               (log:info "Reconnection id ~A not found. Closing the connection." conn-id)
               (websocket-driver:close-connection conn-obj)) ; Don't send the reason for better security.
              (t
               (let ((new-id (random-hex-string)))
                 (setf (gethash conn-obj *connection->id*) new-id)
                 (setf (gethash new-id *id->connection*) conn-obj)
                 (setf (gethash new-id *id->connection-data*)
                       (make-sync-hash-table :test #'equal))
                 (setf (gethash "conn-id" (get-connection-data new-id)) new-id)
                 (log:info "Send conn-id back for new conn: ~d." new-id)
                 (websocket-driver:send conn-obj (format nil "conn_id=~A" new-id))
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (handler-case
                        (funcall *on-connect-handler* new-id)
                      (condition (c)
                        (log:error "Condition caught connection ~A - ~A." new-id c)
                        (values 0 c))))
                  :name (format nil "Websocket connection ~A" new-id)))))
      (condition (c)
        (log:error "Condition caught in handle-new-connection - ~A." c)
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
        (cond ((null conn-id)
               ;; a zombie connection
               (log:info "A zombie connection ~A. Websocket server doesn't remember its connection-id. Closing it."
                         conn-obj)
               (websocket-driver:close-connection conn-obj)) ; don't send the reason for better security
              ((equal (first ml) "0")
               ;; a ping
               (log:info "Connection ~A    Ping." conn-id))
              ((equal (first ml) "E")
               ;; an event
               (let* ((em (ppcre:split " " (second ml) :limit 2))
                      (event-id (first em))
                      (data (second em)))
                 (log:info "Connection ~A    Hook = ~A    Data = ~A."
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
                      (condition (c)
                        (log:error "Condition caught in handle-message for event - ~A." c)
                        (values 0 c))))
                  :name (format nil "Websocket event handler ~A" event-id))))
              (t
               ;; a JavaScript execution result
               (let ((server-query-id (first ml))
                     (browser-returned-answer (second ml)))
                 (log:info "Connection ~A    ~A = ~A    ~A = ~A."
                           conn-id
                           'server-query-id
                           server-query-id
                           'browser-returned-answer
                           browser-returned-answer)
                 (setf (gethash (parse-integer server-query-id) *queries*) browser-returned-answer)
                 (bordeaux-threads:signal-semaphore
                  (gethash (parse-integer server-query-id) *queries-sems*))))))
    (condition (c)
      (log:error "Condition caught in handle-message - ~A." c)
      (values 0 c))))

(defun handle-error (conn-obj err)
  "Close a websocket connection CONN."
  (declare (websocket-driver.ws.server:server conn-obj)
           (condition err))
  (handler-case
      (let ((conn-id (gethash conn-obj *connection->id*)))
        (log:error "Error event processed in handler-error - ~A." err)
        (when conn-id
          (remove-connection conn-id conn-obj))
        (websocket-driver:close-connection conn-obj))
    (condition (c)
      (log:error "Condition caught in handle-error - ~A." c)
      (values 0 c))))

(defun handle-close-connection (conn-obj)
  "Close a websocket connection CONN."
  (declare (websocket-driver.ws.server:server conn-obj))
  (handler-case
      (let ((conn-id (gethash conn-obj *connection->id*)))
        (when conn-id
          (log:info "Connection <~A> has closed. ~A." conn-id conn-obj)
          (remove-connection conn-id conn-obj)))
    (condition (c)
      (log:error "Condition caught in handle-close-connection - ~A." c)
      (values 0 c))))

(defun shutdown-websocket-server ()
  (log:info "Websocket server is shutting down.")
  (shutdown-server (loop for conn-data being the hash-values of *id->connection-data*
                         do (clrhash conn-data))
                   (clrhash *id->connection-data*)
                   (clrhash *connection->id*)
                   (clrhash *id->connection*)))


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


;; start point
#+:ignore
(start-websocket-server (lambda (&rest args) (log:info "ON-CONNECT-HANDLER print, conn-id = ~s" (car args)))
                        #'handle-new-connection
                        ;;#'handle-message
                        #'echo-message
                        #'handle-error
                        #'handle-close-connection)
