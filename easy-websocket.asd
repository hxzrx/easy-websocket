(defsystem "easy-websocket"
  :version "0.5.0"
  :description "A websocket-driver wrapper to make it easy to setup a websocket server."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (:log4cl
               :lack
               :clack
               :websocket-driver)
  :in-order-to ((test-op (test-op "easy-websocket/tests")))
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "websocket")))))

(defsystem "easy-websocket/tests"
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :version "0.5.0"
  :serial t
  :depends-on (:easy-websocket
               :atomics
               :cl-isaac
               :bordeaux-threads
               :cl-ppcre
               :parachute)
  :components ((:module "test"
                :serial t
                :components ((:file "packages")
                             (:file "utils")
                             (:file "server")
                             (:file "websocket")
                             )))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :easy-websocket-tests)))
