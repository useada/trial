#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *known-uniforms* (make-hash-table :test 'equal))
(defvar *uniform-counter* 0)

(defun %uniform-slot-id (uniform-name)
  (let ((known (gethash uniform-name *known-uniforms*)))
    (or known
        (setf (gethash uniform-name *known-uniforms*)
              (1- (incf *uniform-counter*))))))

(defclass shader-program (gl-resource)
  ((uniform-map :initform (make-array *uniform-counter* :element-type '(signed-byte 32)) :accessor uniform-map)
   (shaders :initarg :shaders :accessor shaders)
   (buffers :initarg :buffers :accessor buffers))
  (:default-initargs
   :shaders (error "SHADERS required.")
   :buffers ()))

(defun check-shader-compatibility (shaders)
  (loop with table = (make-hash-table :test 'eql)
        for shader in shaders
        do (if (gethash (shader-type shader) table)
               (error "Cannot compile two shaders of the same type into a single program~%  ~a~%  ~a"
                      (gethash (shader-type shader) table) shader)
               (setf (gethash (shader-type shader) table) shader))
        finally (return shaders)))

(defmethod dependencies ((program shader-program))
  (append (shaders program)
          (buffers program)))

(defun link-program (program shaders)
  (let ((prog (gl-name program)))
    (dolist (shader shaders)
      (check-allocated shader)
      (gl:attach-shader prog (gl-name shader)))
    (gl:link-program prog)
    (dolist (shader shaders)
      (gl:detach-shader prog (gl-name shader)))
    (unless (gl:get-program prog :link-status)
      (error "Failed to link ~a: ~%~a"
             program (gl:get-program-info-log prog)))
    (v:debug :trial.asset "Linked ~a with ~a." program shaders)
    (loop for buffer in (buffers program)
          do (bind buffer program))
    (fill (uniform-map program) -1)))

(defmethod (setf shaders) :before (shaders (program shader-program))
  (when (allocated-p program)
    ;; If we're already hot, relink immediately.
    (handler-bind ((resource-not-allocated (constantly-restart 'continue)))
      (link-program program shaders))))

(defmethod (setf buffers) :before (buffers (program shader-program))
  (when (allocated-p program)
    (loop for buffer in buffers
          do (bind buffer program))))

(defmethod allocate ((program shader-program))
  (let ((shaders (shaders program)))
    (check-shader-compatibility shaders)
    (let ((prog (gl:create-program)))
      (with-cleanup-on-failure (progn (gl:delete-program prog)
                                      (setf (data-pointer program) NIL))
        (setf (data-pointer program) prog)
        (link-program program shaders)))))

(defmethod deallocate ((program shader-program))
  (fill (uniform-map program) -1)
  (gl:delete-program (gl-name program)))

(declaim (inline %set-uniform))
(defun %set-uniform (location data)
  (declare (optimize speed))
  (declare (type (signed-byte 32) location))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (etypecase data
    (vec4 (%gl:uniform-4f location (vx data) (vy data) (vz data) (vw data)))
    (vec3 (%gl:uniform-3f location (vx data) (vy data) (vz data)))
    (vec2 (%gl:uniform-2f location (vx data) (vy data)))
    (mat4 #+sbcl
          (let ((data (marr4 data)))
            (sb-sys:with-pinned-objects (data)
              (%gl:uniform-matrix-4fv location 1 T (sb-sys:vector-sap data))))
          #-sbcl
          (gl:uniform-matrix-4fv location (marr4 data)))
    (mat3 #+sbcl
          (let ((data (marr3 data)))
            (sb-sys:with-pinned-objects (data)
              (%gl:uniform-matrix-3fv location 1 T (sb-sys:vector-sap data))))
          #-sbcl
          (gl:uniform-matrix-3fv location (marr3 data)))
    (mat2 #+sbcl
          (let ((data (marr2 data)))
            (sb-sys:with-pinned-objects (data)
              (%gl:uniform-matrix-2fv location 1 T (sb-sys:vector-sap data))))
          #-sbcl
          (gl:uniform-matrix-2fv location (marr2 data)))
    (single-float (%gl:uniform-1f location data))
    (double-float (%gl:uniform-1d location data))
    (fixnum (%gl:uniform-1i location data))
    (matn (ecase (mrows data)
            (2 (ecase (mcols data)
                 (3 (gl:uniform-matrix-2x3-fv location (marrn data)))
                 (4 (gl:uniform-matrix-2x4-fv location (marrn data)))))
            (3 (ecase (mcols data)
                 (2 (gl:uniform-matrix-3x2-fv location (marrn data)))
                 (4 (gl:uniform-matrix-3x4-fv location (marrn data)))))
            (4 (ecase (mcols data)
                 (2 (gl:uniform-matrix-4x2-fv location (marrn data)))
                 (3 (gl:uniform-matrix-4x3-fv location (marrn data)))))))))

(declaim (inline %uniform-location))
(defun %uniform-location (program name id)
  (declare (optimize speed))
  (let ((array (uniform-map program)))
    (declare (type (simple-array (signed-byte 32) (*)) array))
    (declare (type (unsigned-byte 16) id))
    ;; FIXME: We don't yet know all uniforms at compile time. We can't remove this check unless we can ensure
    ;;        this absolutely. Need to do stuff like shader pass port uniforms baking as well.
    #-:elide-uniform-map-resize
    (when (<= (length array) id)
      (setf array (setf (uniform-map program) (adjust-array array (1+ id) :element-type '(signed-byte 32)))))
    (locally (declare (optimize (safety 0)))
      (let ((existing (aref array id)))
        (if (< existing 0)
            (setf (aref array id) (gl:get-uniform-location (gl-name program) name))
            existing)))))

(declaim (inline (setf uniform)))
(defun uniform-location (program name)
  (%uniform-location program name (%uniform-slot-id name)))

(declaim (inline (setf uniform)))
(defun (setf uniform) (data asset name)
  (declare (optimize speed))
  (let* ((name (etypecase name
                 (string name)
                 (symbol (symbol->c-name name))))
         (location (uniform-location asset name)))
    (%set-uniform location data)))

(define-compiler-macro (setf uniform) (&environment env data asset name)
  (let ((nameg (gensym "NAME"))
        (idg (gensym "ID"))
        (locationg (gensym "LOCATION")))
    (cond ((constantp name env)
           `(let ((,nameg (load-time-value
                           (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                             (etypecase ,name
                               (string ,name)
                               (symbol (symbol->c-name ,name))))))
                  (,idg (load-time-value
                         (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                                  (%uniform-slot-id
                                   (etypecase ,name
                                     (string ,name)
                                     (symbol (symbol->c-name ,name))))))))
              (%set-uniform (%uniform-location ,asset ,nameg ,idg) ,data)))
          (T
           `(let* ((,nameg ,name)
                   (,nameg (etypecase ,nameg
                             (string ,nameg)
                             (symbol (symbol->c-name ,nameg))))
                   (,locationg (uniform-location ,asset ,nameg)))
              (%set-uniform ,locationg ,data))))))

(defmethod uniforms ((program shader-program))
  (let ((count (gl:get-program (gl-name program) :active-uniforms)))
    (loop for i from 0 below count
          collect (multiple-value-bind (size type name) (gl:get-active-uniform (gl-name program) i)
                    (list :index i
                          :size size
                          :type type
                          :name name
                          :location (uniform-location program name))))))
