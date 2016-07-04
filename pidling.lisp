;;;; pidling.lisp

(in-package #:pidling)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))


(defun render ()
  (restartable
    (gl:clear :color-buffer)
    (gl:with-pushed-matrix
      (gl:color 1 1 1 1)
      (gl:rect -25 -25 25 25))))

(sdl2-image:init '(:png))

(defstruct pidling-object)

(defstruct vertex-2d
  x y)

(defstruct matrix4
  (values (make-array 16 :initial-element 0.0 :fill-pointer 0 :element-type 'single-float)))

(defun matrix-pos (r c)
  (+ r (* c 4)))

(defmethod matrix4-identity! (m)
  (loop for r from 0 to 3
        do (loop for c from 0 to 3
                 do (setf (aref (matrix4-values m) (matrix-pos r c))
                          (if (= r c) 1.0 0.0)))))

(defmethod matrix4-ortho! ((m matrix4) left right bottom top n f)
  (flet ((set-m (r c v)
           (setf (aref (matrix4-values m) (matrix-pos r c)) v)))
    (let* ((x-ortho (/ (- right left) 2))
           (y-ortho (/ (- top bottom) 2))
           (z-ortho (/ (- f n) -2))
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


(defstruct texture (pidling-object)
  id
  width
  height)

(defstruct shader (pidling-object)
  id)

(defstruct shader-program (pidling-object)
  id
  vertex-shader
  fragment-shader)

(defstruct sprite (pidling-object)
  position
  rotation
  scale
  opacity
  animation
  body)

(defstruct sprite-batch (pidling-object)
  texture
  program
  sprites
  filled-sprite-count
  blend-function
  vertex-buffer
  sprite-sheet)

(defstruct sprite-frame (pidling-object)
  key
  source-size
  size
  trimmed-p
  texture-rectangle
  color-rectangle
  offset
  rotated-p
  batch-vertices)

(defstruct sprite-sheet (pidling-object)
  frames
  metadata)

(deftype animation-mode () '(member :loop :one-shot :reverse :ping-pong))

(defstruct animation (pidling-object)
  (time 0.0 :type float)
  (mode :loop :type animation-mode)
  delay
  frames)

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
    (loop for x across digits
          collect (parse-integer x))))

(defun parse-rectangle (x)
  (multiple-value-bind (match digits)
      (cl-ppcre:scan-to-strings *four-element-size-regexp* x)
    (declare (ignore match))
    (loop for x across digits
          collect (parse-integer x))))

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

(defstruct vertex-buffer (pidling-object)
  id buff size)

(defstruct sprite-batch-vertex
  x y u v rotation scale tx ty opacity)

(defparameter *size-batch-verts* 9)

(defun create-vertex-buffer (&key (size 4096))
  (let* ((vb (make-vertex-buffer :id (gl:gen-buffer))))
    (bind vb)
    (setf (vertex-buffer-buff vb) ;; (make-array (* size *size-batch-verts*)
                                  ;;               :element-type 'single-float
                                  ;;               :initial-element 0.0)
          (gl:alloc-gl-array :float (* size *size-batch-verts*)))
    (gl:buffer-data :array-buffer :dynamic-draw (vertex-buffer-buff vb))))

(defmethod bind ((vb vertex-buffer))
  (gl:bind-buffer :array-buffer (vertex-buffer-id vb)))

(defmethod free-object ((vb vertex-buffer))
  (gl:delete-buffers (list (vertex-buffer-id vb)))
  (setf (vertex-buffer-id vb) nil))

(defmethod create-animation ((sb sprite-batch) &rest frames)
  (let* ((a (make-animation :delay 0.3))
         (frames (mapcar (lambda (x)
                           (sprite-sheet-frame-by-name (sprite-batch-sprite-sheet sb) x)) frames)))
    (setf (animation-frames a) frames)))

(defmacro with-vertex-buffer-mapped (vb binding &body body)
  `(let* ((,binding (vertex-buffer-map ,vb)))
     (unwind-protect (progn ,@body)
       (vertex-buffer-unmap vb))))

(defmethod vertex-buffer-map ((vb vertex-buffer))
  (bind vb)
  (gl:map-buffer-to-gl-array :array-buffer :write-only :dynamic-draw))

(defmethod vertex-buffer-unmap (vb vertex-buffer)
  (gl:unmap-buffer :array-buffer))

(defun load-sprite-batch (filename)
  (let* ((sheet (load-sheet filename))
         (sb (make-sprite-batch
              :sprite-sheet sheet
              :vertex-buffer (create-vertex-buffer)
              :filled-sprite-count 0)))
    sb))


(defun load-json (filename)
  (with-open-file (stream filename)
    (cl-json:decode-json stream)))

(defun create-shader (source type)
  (let* ((shader (make-shader)))
    (setf (shader-id shader) (gl:create-shader type))
    (gl:shader-source (shader-id shader) (list source))
    (gl:compile-shader (shader-id shader))
    shader))

(defmethod create-shader-program (vertex-shader-source fragment-shader-source)
  (let* ((program (make-shader-program)))
    (with-slots (shader-program-id shader-vertex-shader shader-fragment-shader) program
      (setf shader-program-id (gl:create-program))
      (setf shader-vertex-shader (create-shader vertex-shader-source :vertex-shader))
      (setf shader-fragment-shader (create-shader fragment-shader-source :fragment-shader))
      (gl:attach-shader shader-program-id (shader-id shader-vertex-shader))
      (gl:attach-shader shader-program-id (shader-id shader-fragment-shader))
      (gl:link-program shader-program-id)
      (gl:validate-program))
    program))

(defmethod uniform-location ((p shader-program) name)
  (gl:get-uniform-location (shader-program-id p) name))

(defmethod attribute-location ((p shader-program) name)
  (gl:get-attrib-location (shader-program-id p) name))

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
  (gl:uniform-matrix-4fv (uniform-location (sprite-batch-program sb) "u_projTrans") (matrix4-values transform))

  (dolist (i '(("a_position" 0) ("a_texCoord0" 8) ("transform" 16) ("translation" 24) ("opacity" 28)))
    (let ((location (attribute-location (sprite-batch-program sb) (first i))))
      (gl:enable-vertex-attrib-array location)
      (gl:vertex-attrib-pointer location 2 :float t 32 (second i))))
  (gl:enable :blend)
  (gl:blend-func :one-minus-src-alpha :src-alpha)
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

(defstruct application (pidling-object))

(defmethod key-event ((a application)))
(defmethod window-size-event ((a application) w h))
(defmethod render-event ((a application)))
(defmethod init-event ((a application)))
(defmethod closing-event (a))

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
        (sdl2:with-event-loop (:method :wait :timeout 100)
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                    (sdl2:push-event :quit)))
          (:windowevent (:data1 data1 :data2 data2 :type type)
                        (when (eq sdl2-ffi:+sdl-windowevent-resized+ type)
                          (window-size-event a data1 data2)))
          (:idle ()
                 (restartable
                   ;; (update-swank)
                   (gl:clear :color-buffer)
                   (render-event a)
                   (gl:flush)
                   (sdl2:gl-swap-window win)))
          (:quit () t)))))
  (closing-event a)
  (format t "Exiting..."))
