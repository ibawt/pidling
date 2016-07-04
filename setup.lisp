(ql:quickload 'pidling)
(bt:make-thread (lambda () (swank:create-server :port 4005 :dont-close t)))

(defun start-game ()
  (sdl2:make-this-thread-main (lambda () (pidling:start))))
