(in-package :easy-websocket-tests)

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
