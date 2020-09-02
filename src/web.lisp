(in-package :cl-user)
(defpackage mtg.web
  (:use :cl
        :caveman2
        :mtg.config
        :mtg.view
        :mtg.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :mtg.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute "/play" (&key |name|)
  (render #P"play.html" `((:life . ,(mtg.controller:get-life))
			  (:hand . ,(get-cards #'mtg.controller:get-hand)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
