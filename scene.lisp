#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene (container event-loop)
  ((name-map :initform (make-hash-table :test 'eq) :accessor name-map)))

(defmethod register ((entity entity) (scene scene))
  (setf (gethash (name entity) (name-map scene)) entity))

(defmethod register :after ((listener listener) (scene scene))
  (add-listener listener scene))

(defmethod deregister ((entity entity) (scene scene))
  (remhash (name entity) (name-map scene)))

(defmethod deregister :after ((listener listener) (scene scene))
  (remove-listener thing scene))

(defmethod scene ((scene scene)) scene)
