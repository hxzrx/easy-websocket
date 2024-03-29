(in-package :easy-websocket)


(defvar *app* nil
  "App middleware of Clack")

(defvar *handler* nil
  "Http handler of Clack")

;; completely copied from alexandria
(defun delete-from-plist (plist &rest keys)
  "Just like REMOVE-FROM-PLIST, but this version may destructively modify the
provided PLIST."
  (declare (optimize speed))
  (loop with head = plist
        with tail = nil   ; a nil tail means an empty result so far
        for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
           (if (member key keys :test #'eq)
               ;; skip over this pair
               (let ((next (cdr rest)))
                 (if tail
                     (setf (cdr tail) next)
                     (setf head next)))
               ;; keep this pair
               (setf tail rest))
        finally (return head)))

(defun handle-websocket-connection (on-open-handler on-message-handler on-error-handler on-close-handler env)
  "Setup websocket server on ENV with OPEN, MESSAGE, ERROR and CLOSE handlers."
  (handler-case
      (let ((ws (websocket-driver:make-server env)))
        (when on-open-handler
          (websocket-driver:on :open ws
                               (lambda ()
                                 (handler-case
                                     (funcall on-open-handler ws env)
                                   (condition (c)
                                     (log:error "Websocket connection handler on :open with condition <~A>" c)
                                     (values 0 c))))))
        (websocket-driver:on :message ws
                             (lambda (msg)
                               (handler-case
                                   (funcall on-message-handler ws msg)
                                 (condition (c)
                                   (log:error "Websocket connection handler on :message with condition <~A>" c)
                                   (values 0 c)))))
        (when on-error-handler
          (websocket-driver:on :error ws
                               (lambda (err)
                                 (handler-case
                                     (funcall on-error-handler ws err)
                                   (condition (c)
                                     (log:error "Websocket connection handler on :error with condition <~A>" c)
                                     (values 0 c))))))
        (when on-close-handler
          (websocket-driver:on :close ws
                               (lambda (&key code reason)
                                 (declare (ignore code reason))
                                 (handler-case
                                     (funcall on-close-handler ws)
                                   (condition (c)
                                     (log:error "Websocket connection handler on :close with condition <~A>" c)
                                     (values 0 c))))))
        (lambda (responder)
          (declare (ignore responder))
          (websocket-driver:start-connection ws)))
    (condition (c)
      (log:error "Websocket connection handler failed when initializing the server with condition <~A>" c)
      (values 0 c))))

(defun start (on-open-handler
              on-message-handler
              on-error-handler
              on-close-handler
              &rest args
              &key
                (host    "0.0.0.0")
                (port    8080)
                (path    "/ws")
                (server  :hunchentoot)
                (workers 2)
                request-verifier
              &allow-other-keys)
  "Builds a clack app and then starts the webserver.
Only websocket requests are allowed, which have upgrade/websocket record in the headers table,
other requests will be responded with code 403.

on-open-handler:    function, accepts conn-obj and the env as its arguments, used to listen to the open event.
on-message-handler: function, accepts conn-obj and the conn-id as its arguments, used to listen to the message event.
on-error-handler:   function, accepts an error object as its argument, used to handle the error.
on-close-handler:   function, accepts conn-obj as its argument, used to listen to the close event.
host: the address this server will listen to.
port: the port opened by this server
path: if not NULL, the requested path should match this arg, case insensitive.
server: can be one of :hunchentoot, :woo, :wookie, default to :hunchentoot, bugs occurred for others.
workers: the count of threads which will be used to fork the workers of the webserver.
request-verifier: function or NULL, the function accepts the `env` and check the request, a 403 response will be sent if this check failed. This function will be called if provided.

on-open-handler, on-error-handler and on-close-handler can be nil to ignore the respect events.

Note that the default webserver is hunchentoot, others still have bugs or strange behaviors.
For example, for WOO server, sending messages to the client in on-open-handler will cache util there is a message event,
however, this will not cache for HUNCHENTOOT.
"
  (setf *app* (lack:builder
               ;; verify request
               (lambda (app)
                 (lambda (env)
                   (log:info "A remote host \"~d:~d\" tries to make request for uri \"~d\"."
                             (getf env :REMOTE-ADDR) (getf env :REMOTE-PORT) (getf env :REQUEST-URI))
                   ;; verify path, case insensitive
                   (if (and path (not (string-equal path (getf env :PATH-INFO))))
                       (prog1 '(400 (:content-type "text/plain") ("404 Not Found!"))
                         (log:warn "A remote host \"~d\" tried to make a request but with invalid path \"~d\"."
                                   (getf env :REMOTE-ADDR) (getf env :PATH-INFO)))
                       ;; verify websocket request
                       (if (equal "websocket" (gethash "upgrade" (getf env :headers)))
                           ;; the url has the fmt such as "ws://localhost/ws?r=conn_id" where conn_id should be verified
                           (if request-verifier
                               (if (funcall request-verifier env)
                                   (funcall app env)
                                   (prog1 '(403 (:content-type "text/plain") ("403 Forbidden!"))
                                     (log:warn "Failed to verify the websocket connection request.")))
                               (funcall app env))
                           (prog1 '(404 (:content-type "text/plain") ("404 Not Found!"))
                             (log:warn "The remote host \"~d\" tried to make a normal http request."
                                       (getf env :REMOTE-ADDR)))))))
               ;; handle websocket
               (lambda (env)
                 (handle-websocket-connection on-open-handler on-message-handler on-error-handler on-close-handler env))))
  (setf *handler* (apply #'clack:clackup
                         *app*
                         :server server
                         :address host
                         :port port
                         :debug nil
                         :use-default-middlewares nil
                         :worker-num workers
                         (delete-from-plist args :server :address :port :debug :use-default-middlewares :worker-num)))
  (log:info "Websocket server started at address \"ws://~A:~A~@[~d~]\" with ~d workers." host port path workers)
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

(defmacro with-client (client address on-message-handler &body body)
  "Make a webscoket client and do what you want to, the connection will be closed after you've done the jobs.
CLIENT is some symbol that will bind to a websocket client object, and it should be used in BODY.
ADDRESS: a websocket address string such as \"ws://127.0.0.1:8080\".
ON-MESSAGE-HANDLER: a unary function to handler the incoming messages, its sole argument is the message object.
BODY: a list of forms.

It seems that the server could not listen to the close event!
"
  `(let ((,client (websocket-driver:make-client ,address)))
     (websocket-driver:on :message ,client
                          ,on-message-handler)
     (websocket-driver:start-connection ,client)
     (unwind-protect  (progn ,@body)
       ;; close-connection did not send close frame to the server, the close event cannot be sent by emit
       (websocket-driver.ws.base::send-close-frame ,client "" 1000) ; or (websocket-driver:send ,client "" :type :close)
       (websocket-driver:close-connection ,client))))
