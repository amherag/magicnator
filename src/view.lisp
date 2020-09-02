(in-package :cl-user)
(defpackage mtg.view
  (:use :cl)
  (:import-from :mtg.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json
	   :get-cards))
(in-package :mtg.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

(defun get-cards (fn)
  (let* ((card-names (funcall fn))
	 (small-images (mtg.controller:get-cards-image-pathspec card-names :small))
	 (normal-images (mtg.controller:get-cards-image-pathspec card-names :normal))
	 (large-images (mtg.controller:get-cards-image-pathspec card-names :large))
	 (png-images (mtg.controller:get-cards-image-pathspec card-names :png)))
    (loop for name in card-names
       for small in small-images
       for normal in normal-images
       for large in large-images
       for png in png-images
       collect `((:name . ,name)
		 (:image (:small . ,small)
			 (:normal . ,normal)
			 (:large . ,large)
			 (:png . ,png))))))
;; (get-cards #'mtg.controller:get-hand)


;;
;; Execute package definition

(defpackage mtg.djula
  (:use :cl)
  (:import-from :mtg.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :mtg.djula))
