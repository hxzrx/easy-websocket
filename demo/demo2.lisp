;;;; A full demo of websocket handlers.
;;;; Most of the code were copied from Clog <https://github.com/rabbibotton/clog>
;;;; This demo defines four handlers:
;;;;   HANDLE-NEW-CONNECTION for open event,
;;;;   HANDLE-MESSAGE for message event, and ECHO-MESSAGE is a simpler handler for message event,
;;;;   HANDLE-ERROR for error event,
;;;;   HANDLE-CLOSE-CONNECTION for close event.
;;;;
;;;; These handlers can be used in products (they've been already used in Clog except HANDLER-ERROR),
;;;; and only HANDLE-MESSAGE should be redefined according to your own message format.
;;;;
;;;; An html src is also put in the #|...|# comment in the end of this file,
;;;; it will display all messages sending from the websocket server.
;;;; This html src is from "The Common Lisp Cookbook" in <https://lispcookbook.github.io/cl-cookbook/websockets.html>.
;;;; Save the src into an html file, change the ws address to your websocket address,
;;;; then open it with a web browser and see what will be displayed.
;;;;
;;;; Hope it will be a good starting point to develop a websocket application.


;;;; The difference between this file and example.lisp is,
;;;; this file receive and send binary messages of type (unsigned-byte 8) arrays,
;;;; where the messages are serialized/deserialized by cl-conspack.

(ql:quickload (list :atomics
                    :cl-isaac
                    :bordeaux-threads
                    :cl-ppcre
                    :cl-conspack
                    :easy-websocket))


(in-package :easy-websocket)

;;; utils

;; Random

#-(or mswindows win32 cormanlisp) ; Not supported yet
(defparameter *isaac-ctx*
  (isaac:init-self-seed :count 5
                        :is64 #+:X86-64 t #-:X86-64 nil)
  "Used to generate random hex strings")

(defun random-hex-string ()
  "Generate cryptographic grade random ids for use in connections."
  #+(or mswindows win32 cormanlisp)
  (ironclad:byte-array-to-hex-string
   (ironclad:random-data 16))
  #-(or mswindows win32 cormanlisp)
  (format nil "~(~32,'0x~)" (#+:X86-64 isaac:rand-bits-64
                             #-:X86-64 isaac:rand-bits
                             *isaac-ctx* 128)))

;; Hash table

(defun make-sync-hash-table (&rest args)
  "Make synchronized hash table"
  #+(or sbcl ecl mezzano)
  (apply #'make-hash-table :synchronized t args)
  #-(or sbcl ecl mezzano) (apply #'make-hash-table args))

;; Atomic

(defun make-atomic (init-value)
  "Return a structure that can be cas'ed"
  #+ccl
  (make-array 1 :initial-element init-value)
  #-ccl
  (cons init-value nil))

(defmacro atomic-place (atomic-structure)
  "Return the place of the value of ATOMIC-STRUCTURE which is made by make-atomic."
  #+ccl
  `(svref ,atomic-structure 0)
  #-ccl
  `(car ,atomic-structure))

(defmacro atomic-incf (place &optional (diff 1))
  "Atomic incf the fixnum in PLACE with DIFF and return the old value."
  #+sbcl
  `(sb-ext:atomic-incf ,place ,diff)
  #+ccl
  `(let ((old ,place))
     (ccl::atomic-incf-decf ,place ,diff)
     old))

(defun make-counter (&optional (init-num 0))
  "Make an thread safe counter which starts from INIT-NUM."
  (declare (fixnum init-num))
  (make-atomic init-num))

(defmacro counter-incf (counter)
  "Increase COUNTER and return its old value"
  `(atomic-incf (atomic-place ,counter)))

(defmacro counter-peek (counter)
  "Atomically get the value of PLACE without change it."
  #+sbcl
  `(progn
     (sb-thread:barrier (:read))
     (atomic-place ,counter))
  #-sbcl
  (alexandria:with-gensyms (val)
    `(loop for ,val = (atomic-place ,counter)
           until (atomics:cas (atomic-place ,counter) ,val ,val)
           finally (return ,val))))


;;; -------

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

(defvar *on-connect-handler* nil
  "A hook which is used to initialize a new websocket connection.")

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

;; handlers

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
                 (websocket-driver:send conn-obj (conspack:encode (format nil "conn_id=~A" new-id)))
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (handler-case
                        (when *on-connect-handler*
                          (funcall *on-connect-handler* new-id))
                      (condition (c)
                        (log:error "Condition caught connection ~A - ~A." new-id c)
                        (values 0 c))))
                  :name (format nil "Websocket connection ~A" new-id)))))
      (condition (c)
        (log:error "Condition caught in handle-new-connection - ~A." c)
        (values 0 c)))))

;; A very simple handler for :messagen event
(defun echo-message (conn-obj message)
  "Send the message back to the client"
  (format t "Message received and send back: <~s>~%" message)
  (websocket-driver:send conn-obj message))

(defun echo-message-serialized (conn-obj message)
  "Send the message back to the client"
  (let* ((decoded (conspack:decode message))
         (encoded (conspack:encode decoded)))
    (format t "Message received <~s> and send back: <~s>~%" decoded encoded)
    (websocket-driver:send conn-obj encoded)))

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

(defun stop-server ()
  (log:info "Websocket server is shutting down.")
  (stop (loop for conn-data being the hash-values of *id->connection-data*
              do (clrhash conn-data))
        (clrhash *id->connection-data*)
        (clrhash *connection->id*)
        (clrhash *id->connection*)))


;; Helpers

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


;; start point, receive an array of type (unsigned-byte 8), and send back
(start 'handle-new-connection
       'echo-message-serialized
       'handle-error
       'handle-close-connection
       :uri "/ws"
       :server :hunchentoot)

#+:ignore
(easy-websocket:with-client client "ws://127.0.0.1:8080/ws" #'(lambda (message)
                                                             (format t "~&Client received: ~A~%" message))
  (easy-websocket:send client "Hello server!")
  (sleep 0.1)) ; sleep so that the msg will be printed

;; (stop-server)


;; https://lispcookbook.github.io/cl-cookbook/websockets.html
#|

<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>LISP-CHAT</title>
  </head>
  <body>
    <ul id="chat-echo-area">
    </ul>
    <div style="position:fixed; bottom:0;">
      <input id="chat-input" placeholder="say something" >
    </div>
    <script>
      window.onload = function () {
          const inputField = document.getElementById("chat-input");
          function receivedMessage(msg) {
              let li = document.createElement("li");
              li.textContent = msg.data;
              console.log("received a msg: ", msg.data)
              document.getElementById("chat-echo-area").appendChild(li);
          }
          const ws = new WebSocket("ws://192.168.182.132:8080");
          ws.addEventListener('message', receivedMessage);

          inputField.addEventListener("keyup", (evt) => {
              if (evt.key === "Enter") {
                  ws.send(evt.target.value);
                  evt.target.value = "";
              }
          });
      };
    </script>
  </body>
</html>

|#
