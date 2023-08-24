(defpackage #:easy-websocket-tests
  (:use #:cl #:parachute)
  (:export :test
   :easy-websocket-tests))

(in-package :easy-websocket-tests)

(define-test easy-websocket-tests)
(define-test websocket-tests :parent easy-websocket-tests)
