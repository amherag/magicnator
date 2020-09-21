(in-package :cl-user)
(defpackage mtg.config
  (:use :cl :postmodern)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
	   :*scryfall-url*
           :appenv
           :developmentp
           :productionp
	   :conn
	   ))
(in-package :mtg.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :mtg))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defparameter *scryfall-url* "https://api.scryfall.com")

;; For postgres.
(defparameter *db-name* "mtg")
(defparameter *db-user* "amherag")
(defparameter *db-pass* "asafrade")
(defparameter *db-hostname* "localhost")

(defmacro conn (&rest body)
  `(with-connection (list ,*db-name* ,*db-user* ,*db-pass* ,*db-hostname*)
     ,@body))

(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
