(in-package #:org.shirakumo.fraf.trial.release)

(defun output ()
  (pathname-utils:to-directory (config :output)))

(defun version (&optional (system (config :system)))
  (asdf:component-version (asdf:find-system system)))

(defun release ()
  (pathname-utils:subdirectory (output) (version)))

(defun update-files (&optional (target (release)))
  (loop for file in (config :build :prune)
        do (prune (merge-pathnames file target)))
  (loop for file in (config :build :copy)
        do (let ((source (merge-pathnames (if (listp file) (first file) file)
                                          (asdf:system-source-directory (config :system))))
                 (target (merge-pathnames (if (listp file) (second file) file)
                                          target)))
             (copy source target))))

(defun deploy (&key (release (release)))
  (let ((bindir (pathname-utils:subdirectory (asdf:system-source-directory (config :system)) "bin")))
    (ensure-directories-exist release)
    (deploy:copy-directory-tree bindir release :copy-root NIL)
    (update-files release)
    (uiop:delete-file-if-exists (merge-pathnames "trial.log" release))
    (dolist (path (list-paths release "*.exe" "*.run" "*.o" "*.dylib"))
      (let ((attrs (attributes:decode-attributes (attributes:attributes path))))
        (setf (getf attrs :other-execute) T)
        (setf (getf attrs :group-execute) T)
        (setf (getf attrs :owner-execute) T)
        (setf (attributes:attributes path) (attributes:encode-attributes attrs))))
    release))

(defun bundle-path (release)
  (make-pathname :name (pathname-utils:directory-name release) :type "zip"
                 :defaults (pathname-utils:parent release)))

(defun bundle (&key (release (release)))
  (let ((bundle (bundle-path release)))
    (zippy:compress-zip release bundle :if-exists :supersede)
    bundle))

(defun release-systems (release)
  (let ((systems ()))
    (when (directory (make-pathname :name :wild :type "run" :defaults release))
      (push "linux" systems))
    (when (directory (make-pathname :name :wild :type "o" :defaults release))
      (push "mac" systems))
    (when (directory (make-pathname :name :wild :type "exe" :defaults release))
      (push "windows" systems))
    systems))

(defmethod upload ((service (eql :ftp)) &key (release (release)) (user (config :ftp :user)) (port (config :ftp :port)) (hostname (config :ftp :hostname)) (password (config :ftp :password)) (path (config :ftp :path)))
  (org.mapcar.ftp.client:with-ftp-connection (connection :hostname hostname
                                                         :port (or port 21)
                                                         :username username
                                                         :password (or password (password user))
                                                         :passive-ftp-p T)
    (when path
      (org.mapcar.ftp.client:send-cwd-command connection (uiop:native-namestring path)))
    (let ((bundle (bundle-path release)))
      (org.mapcar.ftp.client:store-file connection bundle (file-namestring bundle) :type :binary))))

(defmethod upload ((service (eql :http)) &key (release (release)) (url (config :http :url)) (method (config :http :method)) (file-parameter (config :http :file-parameter)) (parameters (config :http :post-parameters)))
  (dexador:request url
                   :method (or method :post)
                   :content (list* (cons (or file-parameter "file") (bundle-path release))
                                   parameters)))

(defmethod upload ((service (eql :itch)) &key (release (release)) (user (config :itch :user)) (project (config :itch :project)) &allow-other-keys)
  (run "butler" "push" (uiop:native-namestring release)
       (format NIL "~a/~a:~{~a~^-~}" user (or project (string (config :system))) (release-systems release))
       "--userversion" (version)))

(defmethod upload ((service (eql :steam)) &key (release (release)) (branch (config :steam :branch)) (preview (config :steam :preview)) (user (config :steam :user)) (password (config :steam :password)) &allow-other-keys)
  (let ((template (make-pathname :name "app-build" :type "vdf" :defaults (output)))
        (build (make-pathname :name "app-build" :type "vdf" :defaults release)))
    (file-replace template build `(("\\$CONTENT" ,(uiop:native-namestring release))
                                   ("\\$BRANCH" ,(or branch ""))
                                   ("\\$PREVIEW" ,(if preview "1" "0"))))
    (run "steamcmd.sh"
         "+login" user (or password (password user))
         "+run_app_build" (uiop:native-namestring build)
         "+quit")))

(defmethod upload ((service (eql :all)) &rest args &key &allow-other-keys)
  (apply #'upload '(:itch :steam) args))

(defmethod upload ((service (eql T)) &rest args &key &allow-other-keys)
  (dolist (service (config :upload :targets))
    (apply #'upload service args)))

(defmethod upload ((services cons) &rest args &key &allow-other-keys)
  (dolist (service services)
    (apply #'upload service args)))

(defun make (&key (build T) (upload T))
  (deploy:status 0 "Building ~a" (version))
  (build build)
  (deploy:status 1 "Deploying to release directory")
  (let ((release (deploy)))
    (deploy:status 1 "Creating bundle zip")
    (bundle :release release)
    (when upload
      (deploy:status 1 "Uploading")
      (upload upload :release release))
    release))
