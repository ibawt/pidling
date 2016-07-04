(in-package #:pidling)

(defun my-key-callback (window key scancode action mod-keys)
  (format nil "key-callback ~a" key))

(defvar *bat-batch* nil)
(defvar *bat-texture* nil)

(defstruct myapp (application)
  bats-batch
  bats-texture
  transform)

(setf *application* (make-myapp))

(defmethod key-hook ((a myapp) window key scancod action mod-keys)
  (format nil "key pushed!"))

(defmethod init-event ((a myapp))
  (setf *bat-batch* (load-sprite-batch "fixtures/bats.json"))
  (setf *bat-texture* (load-image "fixtures/bats.png")))

(defmethod render-event ((a myapp))
  (restartable
    (gl:clear :color-buffer)
    (gl:with-pushed-matrix
      (gl:color 1 1 1 1)
      (gl:rect -25 -25 25 25))))

(defun start ()
  (show (make-myapp) "test" 640 480))

(export 'start)
