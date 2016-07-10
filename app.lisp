(in-package #:pidling)

(defvar *application* nil)

(defstruct myapp (application)
  bats-batch
  transform)

(defmethod init-event ((a myapp))
  (let* ((texture (load-image "fixtures/bats.png"))
         (batch (load-sprite-batch "fixtures/bats.json"))
         (sprite (create-sprite batch
                                :bats--fly-1.png
                                :bats--fly-2.png
                                :bats--fly-3.png)))
    (declare (ignore sprite))
    (setf (slot-value a 'bats-batch) batch)
    (setf (sprite-batch-texture batch) texture)
    (setf (myapp-transform a) (make-matrix4))
    (matrix4-ortho! (myapp-transform a) 0.0 640.0 480.0 0.0 1.0 -1.0)))

(defmethod update-event ((a myapp) dt)
  (update (myapp-bats-batch a) dt))

(defun frame-names (sb)
  (let* ((frames (sprite-sheet-frames (sprite-batch-sprite-sheet sb))))
    (loop for i being the hash-keys of frames
          collect i)))

(defmethod render-event ((a myapp))
  (restartable
    (gl:clear :color-buffer)
    (render-object (myapp-bats-batch a) (myapp-transform a))))

(defun start ()
  (setf *application* (make-myapp))
  (show *application* "test" 640 480))

(export 'start)
