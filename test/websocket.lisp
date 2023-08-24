(in-package :easy-websocket-tests)


(easy-websocket:start 'handle-new-connection
                      ;;'handle-message
                      'echo-message
                      'handle-error
                      'handle-close-connection
                      :uri "/demo"
                      :server :hunchentoot)

(define-test demo-text :parent websocket-tests
  (let ((msg "easy-websocket")
        (recv nil))
    (is = 0 (hash-table-count *id->connection*))
    (is = 0 (hash-table-count *connection->id*))
    (is = 0 (hash-table-count *id->connection-data*))
    (finish (easy-websocket:with-client client "ws://127.0.0.1:8080/demo" #'(lambda (message)
                                                                              (push message recv)
                                                                              (format t "Client received msg: ~s~%" message))
              (easy-websocket:send client msg)
              (sleep 0.1) ; make sure the message is returned
              (is equal msg (car recv))
              (is = 1 (hash-table-count *id->connection*))
              (is = 1 (hash-table-count *connection->id*))
              (is = 1 (hash-table-count *id->connection-data*))
              (format t "conn id: ~d~%" (subseq (cadr recv) 8))
              (format t "cooooooooonn: ~d~%" (gethash (subseq (cadr recv) 8) *id->connection*))
              (let* ((conn-id (subseq (cadr recv) 8)) ; "conn_id=6cd9c4109f50965610978558983d5fd6"
                     (conn-obj (gethash conn-id *id->connection*)))
                (of-type websocket-driver.ws.server:server conn-obj)
                (is equal conn-id (gethash conn-obj *connection->id*)))
              ))
    (sleep 0.1)
    (is = 0 (hash-table-count *id->connection*))
    (is = 0 (hash-table-count *connection->id*))
    (is = 0 (hash-table-count *id->connection-data*))

    ;; test invalid uri
    (fail (easy-websocket:with-client client "ws://127.0.0.1:8080/ws" #'(lambda (message)
                                                                          (format t "Client received msg: ~s~%" message))
            (easy-websocket:send client msg)
            (sleep 0.1)))

    ;; error signaled by websocket-driver
    (fail (easy-websocket:with-client client "http://127.0.0.1:8080/ws" #'(lambda (message)
                                                                            (format t "Client received msg: ~s~%" message))
            (easy-websocket:send client msg)
            (sleep 0.1)))))
