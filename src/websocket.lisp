(in-package :easy-websocket)


;;; Special vars

(defvar *app*)
(setf (documentation '*app* 'variable)
      "Clack's app middle-ware")

(defvar *handler*)
(setf (documentation '*handler* 'variable)
      "Clack's handler for socket traffic")


(defun websocket-server (on-open-handler on-message-handler on-error-handler on-close-handler env)
  "Setup websocket server on ENV with OPEN, MESSAGE, CLOSE handlers."
  (declare (function on-open-handler on-message-handler on-close-handler)
           (list env))
  (handler-case
      (let ((ws (websocket-driver:make-server env)))
        (websocket-driver:on :open ws
                             (lambda ()
                               (handler-case
                                   (funcall on-open-handler ws env)
                                 (condition (c)
                                   (log:error "Condition caught in websocket-server :open - ~A.~&" c)
                                   (values 0 c)))))
        (websocket-driver:on :message ws
                             (lambda (msg)
                               (handler-case
                                   (funcall on-message-handler ws msg)
                                 (condition (c)
                                   (log:error "Condition caught in websocket-server :message - ~A.~&" c)
                                   (values 0 c)))))
        (websocket-driver:on :error ws
                             (lambda (err)
                               (handler-case
                                   (funcall on-error-handler ws err)
                                 (condition (c)
                                   (log:error "Condition caught in websocket-server :error - ~A.~&" c)
                                   (values 0 c)))))
        (websocket-driver:on :close ws
                             (lambda (&key code reason)
                               (declare (ignore code reason))
                               (handler-case
                                   (funcall on-close-handler ws)
                                 (condition (c)
                                   (log:error "Condition caught in websocket-server :close - ~A.~&" c)
                                   (values 0 c)))))
        (lambda (responder)
          (declare (ignore responder))
          (websocket-driver:start-connection ws)))
    (condition (c)
      (log:error "Condition caught in setting up the websocket-server - ~A.~&" c)
      (values 0 c))))

(defun start-websocket-server (on-open-handler
                               on-message-handler
                               on-error-handler
                               on-close-handler
                               &key
                                 (host    "0.0.0.0")
                                 (port    8080)
                                 (server  :woo)
                                 (workers 2))
  "This function builds a clack app and then starts the webserver.
Only websocket requests are allowed, which have upgrade/websocket record in the headers table,
all other requests will be responded with code 403.

on-open-handler:    function, accepts conn-obj and the env as its arguments, used to listen to the open event.
on-message-handler: function, accepts conn-obj and the conn-id as its arguments, used to listen to the message event.
on-close-handler:   function, accepts conn-obj as its argument, used to listen to the close event.
host: the address this server will listen to.
port: the port opened by this server
server: should be recognized by clack, can be one of :woo, :hunchentoot, :fastcgi, :wookie, :toot, default to :woo.
workers: the count of threads which will be used to fork the workers of the webserver.

Note that the behaviors may not the same between different webservers.
For example, for WOO server, sending messages to the client in on-open-handler will cache util there is a message event,
however, this will not cache for HUNCHENTOOT.
"
  (setf *app* (lack:builder
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
                 (websocket-server on-open-handler on-message-handler on-error-handler on-close-handler env))))
  (setf *handler* (clack:clackup *app*
                                 :server server
                                 :address host
                                 :port port
                                 :debug nil
                                 :use-default-middlewares nil
                                 :worker-num workers))
  (log:info "Websocket server started at address \"~A:~A\" within webserver \"~d\" with ~d workers."
            host port server workers)
  *handler*)

(defmacro shutdown-server (&body cleanup)
  "Stop the websocket server and set *app* and *handler* to nil.
CLEANUP is a list of forms which should be processed along with this shutdown."
  `(if *handler*
       (unwind-protect
            (handler-case (clack:stop *handler*)
              (condition (c)
                (log:error "The websocket server has stopped abnormally with condition <~s>" c)))
         ,@cleanup
         (setf *app* nil)
         (setf *handler* nil))
       (log:warn "The webserver is not running.")))
