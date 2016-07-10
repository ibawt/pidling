;;;; pidling.lisp

(in-package #:pidling)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))

(sdl2-image:init '(:png))

(defstruct object)

(defstruct vertex-2d
  x y)

(defstruct matrix4
  (values (make-array 16 :initial-element 0.0 :element-type 'single-float)))

(defun matrix-pos (r c)
  (+ r (* c 4)))

(defmethod matrix4-identity! (m)
  (loop for r from 0 to 3
        do (loop for c from 0 to 3
                 do (setf (aref (matrix4-values m) (matrix-pos r c))
                          (if (= r c) 1.0 0.0)))))

(defmethod matrix4-ortho! ((m matrix4) left right bottom top n f)
  (flet ((set-m (r c v)
           (setf (aref (matrix4-values m) (matrix-pos r c)) (float v))))
    (let* ((x-ortho (/ 2 (- right left)))
           (y-ortho (/ 2 (- top bottom)))
           (z-ortho (/ -2 (- f n)))
           (tx (/ (- (+ right left))
                  (- right left)))
           (ty (/ (- (+ top bottom))
                  (- top bottom)))
           (tz (/ (- (+ f n))
                  (- f n))))
      (set-m 0 0 x-ortho)
      (set-m 1 0 0)
      (set-m 2 0 0)
      (set-m 3 0 0)
      (set-m 0 1 0)
      (set-m 1 1 y-ortho)
      (set-m 2 1 0)
      (set-m 3 1 0)
      (set-m 0 2 0)
      (set-m 1 2 0)
      (set-m 2 2 z-ortho)
      (set-m 3 2 0)
      (set-m 0 3 tx)
      (set-m 1 3 ty)
      (set-m 2 3 tz)
      (set-m 3 3 1))))


(defstruct texture (object)
  id
  width
  height)

(defstruct shader (object)
  id)

(defstruct shader-program (object)
  id
  vertex-shader
  fragment-shader)

(defstruct sprite (object)
  (position #(160.0 240.0))
  (rotation 0.0)
  (scale 1.0)
  (opacity 1.0)
  animation
  body)

(defstruct sprite-batch (object)
  texture
  program
  sprites
  filled-sprite-count
  blend-function
  vertex-buffer
  sprite-sheet)

(defstruct sprite-frame (object)
  key
  source-size
  size
  trimmed-p
  texture-rectangle
  color-rectangle
  offset
  rotated-p
  batch-vertices)

(defstruct sprite-sheet (object)
  frames
  metadata)

(deftype animation-mode () '(member :loop :one-shot :reverse :ping-pong))

(defstruct animation (object)
  (time 0.0 :type float)
  (mode :loop :type animation-mode)
  (index 0)
  (delay 0.3)
  frames)

(defparameter +vertex-format+
  '(:x ; 0
    :y ; 4
    :u ; 8
    :v ; 12
    :rotation ; 16
    :scale ; 20
    :tx ; 24
    :ty ; 28
    :opacity ; 32
    ))

(defmethod sprite-sheet-frame-by-name ((s sprite-sheet) name)
  (gethash name (sprite-sheet-frames s)))

(defparameter *two-element-size-regexp*
  "{(-*\\d+),(-*\\d+)}")

(defparameter *four-element-size-regexp*
  "{{(-*\\d+),(-*\\d+)},{(-*\\d+),(-*\\d+)}}")

(defun strip-whitespace (x)
  "strips whitespace"
  (remove-if (lambda (x) (eq x #\ )) x))

(defun parse-size (x)
  (multiple-value-bind (match digits)
      (cl-ppcre:scan-to-strings *two-element-size-regexp* x)
    (declare (ignore match))
    (make-array 2 :initial-contents
                (loop for x across digits
                      collect (parse-integer x)))))

(defun parse-rectangle (x)
  (multiple-value-bind (match digits)
      (cl-ppcre:scan-to-strings *four-element-size-regexp* x)
    (declare (ignore match))
    (make-array 4 :initial-contents
                (loop for x across digits
                      collect (parse-integer x)))))


(defun parse-sprite-frame (x)
  (macrolet
      ((get-sheet-element (sheet element)
         `(cdr (assoc ,element (cdr ,sheet)))))
      (make-sprite-frame
    :key (car x)
    :source-size (parse-size (strip-whitespace (get-sheet-element x :sprite-source-size)))
    :size (parse-size (strip-whitespace (get-sheet-element x :sprite-size)))
    :texture-rectangle (parse-rectangle (strip-whitespace (get-sheet-element x :texture-rect)))
    :offset (parse-size (strip-whitespace (get-sheet-element x :sprite-offset)))
    :color-rectangle (parse-rectangle (strip-whitespace (get-sheet-element x :sprite-color-rect)))
    :rotated-p (get-sheet-element x :texture-rotated)
    :trimmed-p (get-sheet-element x :sprite-trimmed))))

(defun make-sprite-frame-map (frames)
  (let* ((table (make-hash-table :test 'equal)))
    (loop for i in frames
          do (setf (gethash (sprite-frame-key i) table) i))
    table))

(defun load-sheet (filename)
  (let* ((json (load-json filename))
         (sheet (make-sprite-sheet))
         (metadata (cdr (assoc :metadata json)))
         (frames (cdr (assoc :frames json))))
    (setf (sprite-sheet-frames sheet)
          (make-sprite-frame-map (mapcar #'parse-sprite-frame frames)))
    (setf (sprite-sheet-metadata sheet)
          metadata)
    sheet))

(defstruct vertex-buffer (object)
  id buff size)

(defstruct sprite-batch-vertex
  x y u v rotation scale tx ty opacity)

(defparameter *size-batch-verts* 9)

(defun create-vertex-buffer (&key (size 4096))
  (let* ((vb (make-vertex-buffer :id (gl:gen-buffer))))
    (bind vb)
    (setf (vertex-buffer-buff vb)
          (gl:alloc-gl-array :float (* size *size-batch-verts*)))
    (gl:buffer-data :array-buffer :dynamic-draw (vertex-buffer-buff vb))
    vb))

(defmethod bind ((vb vertex-buffer))
  (gl:bind-buffer :array-buffer (vertex-buffer-id vb)))

(defmethod bind ((tex texture))
  (gl:bind-texture :texture-2d (texture-id tex)))

(defmethod bind ((p shader-program))
  (gl:use-program (shader-program-id p)))

(defmethod free-object ((vb vertex-buffer))
  (gl:delete-buffers (list (vertex-buffer-id vb)))
  (setf (vertex-buffer-id vb) nil))

(defmethod create-animation ((sb sprite-batch) &rest frames)
  (let* ((a (make-animation :delay 0.3))
         (frames (mapcar (lambda (x)
                           (sprite-sheet-frame-by-name (sprite-batch-sprite-sheet sb) x)) frames)))
    (setf (animation-frames a) (make-array (length frames)
                                           :initial-contents frames))
    a))


(defmethod create-sprite ((sb sprite-batch) &rest keys)
  (with-accessors ((sprites sprite-batch-sprites)) sb
    (setf sprites (cons (make-sprite :animation (apply #'create-animation sb keys))
                        sprites))
    (car sprites)))

(defmethod update ((a animation) (dt float))
  (with-accessors ((time animation-time)
                   (delay animation-delay)
                   (index animation-index)
                   (mode animation-mode)
                   (frames animation-frames)) a
    (incf time dt)
    (loop while (>= time delay)
          do (progn
               (decf time delay)
               (ecase mode
                 (:loop (when (> (incf index) (length frames))
                          (setf index 0)))
                 (:one-shot (when (> (incf index) (length frames))
                              (setf index (- (length frames) 1))))
                 (:reverse (when (< 0 (decf index))
                             (setf index 0))))))))

(defmethod animation-current-sprite-frame ((a animation))
  (aref (animation-frames a) (animation-index a)))

(defmethod update ((s sprite) dt)
  (update (sprite-animation s) dt))

(defmethod fill-buffer ((s sprite) buff dst-offset)
  (let* ((batch-vertices (sprite-frame-batch-vertices (animation-current-sprite-frame (sprite-animation s)))))
    (loop for i from 0 to 5
          do (let* ((src-offset (* i 9)))
               (setf (gl:glaref buff (+ dst-offset src-offset 0)) (aref batch-vertices (+ 0 src-offset))) ; x
               (setf (gl:glaref buff (+ dst-offset src-offset 1)) (aref batch-vertices (+ 1 src-offset))) ; y
               (setf (gl:glaref buff (+ dst-offset src-offset 2)) (aref batch-vertices (+ 2 src-offset))) ; u
               (setf (gl:glaref buff (+ dst-offset src-offset 3)) (aref batch-vertices (+ 3 src-offset))) ; v
               (setf (gl:glaref buff (+ dst-offset src-offset 4)) (sprite-rotation s))                    ; rotation
               (setf (gl:glaref buff (+ dst-offset src-offset 5)) (sprite-scale s))                       ; translatioin
               (setf (gl:glaref buff (+ dst-offset src-offset 6)) (aref (sprite-position s) 0))
               (setf (gl:glaref buff (+ dst-offset src-offset 7)) (aref (sprite-position s) 1))
               (setf (gl:glaref buff (+ dst-offset src-offset 8)) (sprite-opacity s))))))

(defmethod update ((sb sprite-batch) dt)
  (bind (sprite-batch-vertex-buffer sb))
  (setf (sprite-batch-filled-sprite-count sb) 0)
  (gl:with-gl-mapped-buffer (buff :array-buffer :write-only :float)
    (loop for x in (sprite-batch-sprites sb)
          :do (update x dt)
          :do (fill-buffer x buff 0)
          :do (incf (sprite-batch-filled-sprite-count sb)))))

(defmacro with-vertex-buffer-mapped (vb binding &body body)
  `(let* ((,binding (vertex-buffer-map ,vb)))
     (unwind-protect (progn ,@body)
       (vertex-buffer-unmap ,vb))))

(defmethod vertex-buffer-map ((vb vertex-buffer))
  (bind vb)
  (gl:map-buffer-to-gl-array :array-buffer :write-only :dynamic-draw))

(defmethod vertex-buffer-unmap (vb vertex-buffer)
  (gl:unmap-buffer :array-buffer))

(defun read-file (filename)
  (with-open-file (in filename)
    (let ((out (make-string (file-length in))))
      (read-sequence out in)
      out)))

(defmethod sprite-sheet-size ((s sprite-sheet))
  (let* ((metadata (sprite-sheet-metadata s))
         (size (strip-whitespace (cdr (assoc :size metadata)))))
    (parse-size size)))

(defun vec-x (v)
  (aref v 0))

(defun vec-y (v)
  (aref v 1))

(defun rect-x (r)
  (aref r 0))

(defun rect-y (r)
  (aref r 1))

(defun rect-w (r)
  (aref r 2))

(defun rect-h (r)
  (aref r 3))

(defun float/ (&rest rest)
  (float (apply #'/ rest)))

(defmethod sprite-sheet-create-batch-vertices ((s sprite-sheet))
  (maphash (lambda (name frame)
             (declare (ignore name))
             (let* ((bv (make-array (* 6 *size-batch-verts*) :initial-element 0.0 :element-type 'single-float))
                    (total-width (aref (sprite-sheet-size s) 0))
                    (total-height (aref (sprite-sheet-size s) 1))
                    (w/2 (float/ (vec-x (sprite-frame-size frame)) 2))
                    (h/2 (float/ (vec-y (sprite-frame-size frame)) 2))
                    (tex-x  (float/ (rect-x (sprite-frame-texture-rectangle frame)) total-width))
                    (tex-y  (float/ (rect-y (sprite-frame-texture-rectangle frame)) total-height))
                    (tex-w  (float/ (rect-w (sprite-frame-texture-rectangle frame)) total-width))
                    (tex-h  (float/ (rect-h (sprite-frame-texture-rectangle frame)) total-height)))

               ;;; 0
               (setf (aref bv (+ 0 0)) (- w/2)) ; x
               (setf (aref bv (+ 0 1)) (- h/2)) ; y
               (setf (aref bv (+ 0 2)) tex-x)   ; u
               (setf (aref bv (+ 0 3)) tex-y)   ; v

               ;;; 1
               (setf (aref bv (+ 9 0)) w/2)
               (setf (aref bv (+ 9 1)) (- h/2))
               (setf (aref bv (+ 9 2)) (+ tex-x tex-w))
               (setf (aref bv (+ 9 3)) tex-y)

               ;;; 2
               (setf (aref bv (+ 18 0)) w/2)
               (setf (aref bv (+ 18 1)) h/2)
               (setf (aref bv (+ 18 2)) (+ tex-x tex-w))
               (setf (aref bv (+ 18 3)) (+ tex-y tex-h))

               ;;; 3 == 2
               (loop for x from 0 to 8
                     do (setf (aref bv (+ (* 9 3) x)) (aref bv (+ (* 9 2) x))))

               ;;; 4
               (setf (aref bv (+ (* 9 4) 0)) (- w/2))
               (setf (aref bv (+ (* 9 4) 1)) h/2)
               (setf (aref bv (+ (* 9 4) 2)) tex-x)
               (setf (aref bv (+ (* 9 4) 3)) (+ tex-y tex-h))

               ;;; 5 == 0
               (loop for x from 0 to 8
                     do (setf (aref bv (+ (* 9 5) x)) (aref bv x)))

               (setf (sprite-frame-batch-vertices frame) bv))
             )
           (sprite-sheet-frames s)))

(defun load-sprite-batch (filename )
  (let* ((sheet (load-sheet filename))
         (sb (make-sprite-batch
              :sprite-sheet sheet
              :program (create-shader-program
                        (read-file "animation.vert")
                        (read-file "animation.frag"))
              :vertex-buffer (create-vertex-buffer)
              :filled-sprite-count 0)))
    (sprite-sheet-create-batch-vertices sheet)
    sb))

(defun load-json (filename)
  (with-open-file (stream filename)
    (cl-json:decode-json stream)))

(define-condition shader-compile-error (error)
  ((info-log :initarg :info-log :reader info-log)))

(defun create-shader (source type)
  (let* ((shader (make-shader)))
    (setf (shader-id shader) (gl:create-shader type))
    (gl:shader-source (shader-id shader) (list source))
    (gl:compile-shader (shader-id shader))
    (let* ((info-log (gl:get-shader-info-log (shader-id shader))))
      (when (not (string= "" info-log))
       (error 'shader-compile-error :info-log info-log)))
    shader))

(defmethod create-shader-program (vertex-shader-source fragment-shader-source)
  (let* ((program (make-shader-program)))
    (with-slots (id vertex-shader fragment-shader) program
      (setf id (gl:create-program))
      (setf vertex-shader (create-shader vertex-shader-source :vertex-shader))
      (setf fragment-shader (create-shader fragment-shader-source :fragment-shader))
      (gl:attach-shader id (shader-id vertex-shader))
      (gl:attach-shader id (shader-id fragment-shader))
      (gl:link-program id)
      (print (gl:get-program-info-log id))
      (gl:validate-program id))
    program))

(defmethod uniform-location ((p shader-program) name)
  (gl:get-uniform-location (shader-program-id p) name))

(defmethod attribute-location ((p shader-program) name)
  (gl:get-attrib-location (shader-program-id p) name))

(defmethod free-object (o))

(defmethod free-object ((shader shader))
  (when (shader-id shader)
    (gl:delete-shader (shader-id shader))
    (setf (shader-id shader) nil)))

(defmethod free-object ((program shader-program))
  (free-object (shader-program-vertex-shader program))
  (free-object (shader-program-fragment-shader program))
  (when (shader-program-id program)
    (gl:delete-program (shader-program-id program))
    (setf (shader-program-id program) nil)))

(defmethod free-object ((tex texture))
  (when (texture-id tex))
   (gl:delete-textures (list (texture-id tex)))
   (setf (texture-id tex) nil))

(defmethod render-object ((sb sprite-batch) (transform matrix4))
  (bind (sprite-batch-vertex-buffer sb))
  (gl:enable :texture-2d)
  (bind (sprite-batch-texture sb))
  (bind (sprite-batch-program sb))
  (gl:uniform-matrix-4fv (uniform-location (sprite-batch-program sb) "u_projTrans") (matrix4-values transform) nil)

  (dolist (i '(("a_position" 0 2) ("a_texCoord0" 8 2) ("transform" 16 2) ("translation" 24 2) ("opacity" 32 1)))
    (let ((location (attribute-location (sprite-batch-program sb) (first i))))
      (gl:enable-vertex-attrib-array location)
      (gl:vertex-attrib-pointer location (nth 2 i) :float nil (* 4 *size-batch-verts*) (second i))))

  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:draw-arrays :triangles 0 (* 6 (sprite-batch-filled-sprite-count sb))))

(defun load-image (filename)
  (let* ((image (sdl2-image:load-image filename))
         (tex-id (gl:gen-texture))
         (tex (make-texture :id tex-id
                            :width (sdl2:surface-width image)
                            :height (sdl2:surface-height image))))
    (gl:bind-texture :texture-2d tex-id)
    (gl:tex-parameter :texture-2d
                      :texture-min-filter
                      :linear)
    (gl:tex-parameter :texture-2d
                      :texture-mag-filter
                      :linear)

    (gl:tex-image-2d :texture-2d
                     0
                     :rgba
                     (sdl2:surface-width image)
                     (sdl2:surface-height image)
                     0
                     :rgba
                     :unsigned-byte
                     (sdl2:surface-pixels image))
    (sdl2:free-surface image)
    tex))

(defstruct application (object))

(defmethod key-event ((a application)))
(defmethod window-size-event ((a application) w h))
(defmethod render-event ((a application)))
(defmethod init-event ((a application)))
(defmethod closing-event (a))
(defmethod update-event (a dt))

(defun update-swank ()
  "Handle REPL requests."
  #+swank
  (restartable
   (let ((connection (or swank::*emacs-connection*
                         (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))

(defmethod show (a title width height)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (init-event a)
        (gl:viewport 0 0 width height)
        (gl:clear-color 0.0 0.0 0.0 1.0)
        (gl:clear :color-buffer)
        (let* ((dt (get-internal-real-time)))
            (sdl2:with-event-loop (:method :poll :timeout 100)
           (:keyup (:keysym keysym)
                   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                     (sdl2:push-event :quit)))
           (:windowevent (:data1 data1 :data2 data2 :type type)
                         (when (eq sdl2-ffi:+sdl-windowevent-resized+ type)
                           (format t "data1: ~a data2: ~a type: ~a" data1 data2 type)))
           (:idle ()
                  (restartable
                    (setf dt (- dt (get-internal-real-time)))
                    (update-event a (float (/ 1 dt)))
                    (setf dt (get-universal-time))
                    ;; (update-swank)
                    (gl:clear :color-buffer)
                    (render-event a)
                    (gl:flush)
                    (sdl2:gl-swap-window win)))
           (:quit () t))))))
  (closing-event a)
  (format t "Exiting..."))
