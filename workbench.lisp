(defpackage #:workbench
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:harmony #:org.shirakumo.fraf.harmony))
  (:export #:workbench #:launch))
(in-package #:workbench)

(defun ray->aabb (seg-pos angle aabb-pos aabb-size)
  (let* ((seg-vel (vcartesian (vec 1.0 angle)))
         (scale-x (if (= 0 (vx2 seg-vel) 0.00001) 1000000f0 (/ (vx2 seg-vel))))
         (scale-y (if (= 0 (vy2 seg-vel) 0.00001) 1000000f0 (/ (vy2 seg-vel))))
         (sign-x (if (<= 0. (vx2 seg-vel)) +1. -1.))
         (sign-y (if (<= 0. (vy2 seg-vel)) +1. -1.))
         (near-x (* (- (vx2 aabb-pos) (* sign-x (vx2 aabb-size)) (vx2 seg-pos)) scale-x))
         (near-y (* (- (vy2 aabb-pos) (* sign-y (vy2 aabb-size)) (vy2 seg-pos)) scale-y))
         (far-x (* (- (+ (vx2 aabb-pos) (* sign-x (vx2 aabb-size))) (vx2 seg-pos)) scale-x))
         (far-y (* (- (+ (vy2 aabb-pos) (* sign-y (vy2 aabb-size))) (vy2 seg-pos)) scale-y)))
    (unless (or (< far-y near-x)
                (< far-x near-y))
      (let ((t-far (min far-x far-y)))
        (when (< 0 t-far)
          (max near-x near-y))))))

(defclass workbench (org.shirakumo.fraf.trial.harmony:main main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0) :context `(:vsync T)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(defmethod org.shirakumo.fraf.trial.harmony:server-initargs append ((main workbench))
  (list :effects '((mixed:spatial-reverb :name :spatial-reverb))))

(define-pool workbench)

(define-asset (workbench cat) image
    #p"cat.png")

(define-asset (workbench cube) mesh
    (make-cube 15))

(define-asset (workbench grid) mesh
    (make-line-grid 10 100 100))

(define-asset (workbench step) org.shirakumo.fraf.trial.harmony:sound
    #p "~/Projects/cl/kandria/data/sound/step_ dirt 1.wav"
  :volume 0.2)

(define-shader-entity wall (vertex-entity located-entity)
  ((vertex-array :initform (// 'workbench 'cube))
   (bsize :initform (vec 15 15) :accessor bsize)))

(define-shader-entity player (vertex-entity textured-entity located-entity listener)
  ((name :initform 'player)
   (texture :initform (// 'workbench 'cat))
   (vertex-array :initform (// 'workbench 'cube))
   (timer :initform 0.0 :accessor timer)))

(defmethod stage :after ((player player) (area staging-area))
  (stage (// 'workbench 'step) area))

(define-handler (player tick) (dt fc)
  (when (< (decf (timer player) dt) 0.0)
    (setf (timer player) 0.5)
    (harmony:play (// 'workbench 'step) :reset T))
  (when (= (mod fc 10))
    (let ((segment (harmony:segment :spatial-reverb T))
          (angle (float (random (* 2 PI)) 0f0))
          (ttmin most-positive-single-float))
      (for:for ((entity over (scene +main+)))
        (when (typep entity 'wall)
          (let ((tt (or (ray->aabb (vxy (location player)) angle (vxy (location entity)) (bsize entity))
                        most-positive-single-float)))
            (when (< tt ttmin)
              (setf ttmin tt)))))
      (when (< ttmin most-positive-single-float)
        (mixed:add-spatial-probe segment angle ttmin 0.5))))
  (when (retained :w)
    (incf (vy (location player)) (* dt +50)))
  (when (retained :a)
    (incf (vx (location player)) (* dt -50)))
  (when (retained :s)
    (incf (vy (location player)) (* dt -50)))
  (when (retained :d)
    (incf (vx (location player)) (* dt +50))))

(define-handler (player mouse-press) (pos button)
  (case button
    (:left
     (enter* (make-instance 'wall :location (vxy_ pos)) (scene +main+)))
    (:right
     (for:for ((entity over (scene +main+)))
       (when (typep entity 'wall)
         (leave* entity T))))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'trial::fps-counter) scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'player) scene)
    (enter (make-instance '2d-camera :location (vec 0 0 -100)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
