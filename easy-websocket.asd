(defsystem "easy-websocket"
  :version "0.3.1"
  :description "A websocket-driver wrapper to make it easy to setup a websocket server."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (:log4cl
               :lack
               :clack
               :websocket-driver)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "websocket")))))
