(defsystem "easy-websocket"
  :version "0.3.1"
  :description "A websocket-driver wrapper to make it easy to setup a websocket server."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (:log4cl
               :usocket
               :alexandria
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
  :version "0.1.0"
  :serial t
  :depends-on (:easy-websocket
               :parachute
               )
  :components ((:module "test"
                :serial t
                :components ((:file "packages")
                             (:file "utils")
                             (:file "websocket")
                             )))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :easy-websocket-tests)))
