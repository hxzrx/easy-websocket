(in-package :easy-websocket)


;;; utils

(defun random-port (&key (host "0.0.0.0")) ; clog/source/clog-connection.lisp
  "Return a random open port on host.
Note that the port number returned by this function might be unavailable when using it."
  (let* ((listen (usocket:socket-listen host 0))
         (port (usocket:get-local-port listen)))
    (usocket:socket-close listen)
    port))


;;; Special vars

(defvar *app* nil
  "App middleware of Clack")

(defvar *handler* nil
  "Http handler of Clack")


;;; Server

(defun setup-server (on-open-handler on-message-handler on-error-handler on-close-handler env)
  "Setup websocket server on ENV with OPEN, MESSAGE, ERROR and CLOSE handlers."
  (handler-case
      (let ((ws (websocket-driver:make-server env)))
        (websocket-driver:on :open ws
                             (lambda ()
                               (handler-case
                                   (funcall on-open-handler ws env)
                                 (condition (c)
                                   (log:error "Setup-server condition on :open with <~A>" c)
                                   (values 0 c)))))
        (websocket-driver:on :message ws
                             (lambda (msg)
                               (handler-case
                                   (funcall on-message-handler ws msg)
                                 (condition (c)
                                   (log:error "Setup-server condition on :message with <~A>" c)
                                   (values 0 c)))))
        (websocket-driver:on :error ws
                             (lambda (err)
                               (handler-case
                                   (funcall on-error-handler ws err)
                                 (condition (c)
                                   (log:error "Setup-server condition on :error with <~A>" c)
                                   (values 0 c)))))
        (websocket-driver:on :close ws
                             (lambda (&key code reason)
                               (declare (ignore code reason))
                               (handler-case
                                   (funcall on-close-handler ws)
                                 (condition (c)
                                   (log:error "Setup-server condition on :close with <~A>" c)
                                   (values 0 c)))))
        (lambda (responder)
          (declare (ignore responder))
          (websocket-driver:start-connection ws)))
    (condition (c)
      (log:error "Setup-server condition when initializing the server with <~A>" c)
      (values 0 c))))

(defun start (on-open-handler
              on-message-handler
              on-error-handler
              on-close-handler
              &rest args
              &key
                (host    "0.0.0.0")
                (port    8080)
                (server  :hunchentoot)
                (workers 2)
                &allow-other-keys)
  "Builds a clack app and then starts the webserver.
Only websocket requests are allowed, which have upgrade/websocket record in the headers table,
all other requests will be responded with code 403.

on-open-handler:    function, accepts conn-obj and the env as its arguments, used to listen to the open event.
on-message-handler: function, accepts conn-obj and the conn-id as its arguments, used to listen to the message event.
on-close-handler:   function, accepts conn-obj as its argument, used to listen to the close event.
host: the address this server will listen to.
port: the port opened by this server
server: can be one of :hunchentoot, :woo, :wookie, default to :hunchentoot, bugs occurred for others.
workers: the count of threads which will be used to fork the workers of the webserver.

Note that the default webserver is hunchentoot, others still have bugs or strange behaviors.
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
                 (setup-server on-open-handler on-message-handler on-error-handler on-close-handler env))))
  (setf *handler* (apply #'clack:clackup
                         *app*
                         :server server
                         :address host
                         :port port
                         :debug nil
                         :use-default-middlewares nil
                         :worker-num workers
                         (alexandria:delete-from-plist args :server :address :port :debug :use-default-middlewares :worker-num)))
  (log:info "Websocket server started at address \"~A:~A\" within webserver \"~d\" with ~d workers."
            host port server workers)
  *handler*)

(defmacro stop (&body cleanup)
  "Stop the websocket server and set *app* and *handler* to nil.
CLEANUP is a list of forms which should be processed along with this shutdown."
  `(if *handler*
       (prog1 t
         (unwind-protect
              (handler-case (clack:stop *handler*)
                (condition (c)
                  (log:error "The websocket server has stopped abnormally with condition <~s>" c)))
           ,@cleanup
           (setf *app* nil)
           (setf *handler* nil)))
       (prog1 nil
         (log:warn "The webserver is not running."))))
