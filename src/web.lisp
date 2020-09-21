(in-package :cl-user)
(defpackage mtg.web
  (:use :cl
        :caveman2
        :mtg.config
        :mtg.view
        :mtg.db)
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
			  (:hand . ,(get-cards #'mtg.controller:get-hand))
			  (:lands . ,(get-cards #'mtg.controller:get-lands))
			  (:creatures . ,(get-cards #'mtg.controller:get-creatures))
			  (:artifacts . ,(get-cards #'mtg.controller:get-artifacts))
			  (:planeswalkers . ,(get-cards #'mtg.controller:get-planeswalkers))
			  (:graveyard . ,(get-cards #'mtg.controller:get-graveyard))
			  (:exiled . ,(get-cards #'mtg.controller:get-exiled)))))

(defroute "/view-collection" (&key |keywords| |type| (|all-or-any| :any))
  (render #P"view-collection.html" `((:collection . ,(get-cards (lambda () (mtg.controller:get-collection :keywords (cl-ppcre:split "," |keywords|) :type |type| :all-or-any? |all-or-any|)))))))

(defroute "/view-deck" (&key |name|)
  (render #P"view-deck.html" `((:deck . ,(get-cards (lambda () (mtg.controller:get-deck-cards-names-and-quantities |name|)))))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
