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

(defroute "/view-collection" (&key |name| |colors| |text| |keywords| |type|)
  (render #P"view-collection.html" `((:collection . ,(get-cards (lambda ()
								  (mtg.controller:get-collection
								   :name |name|
								   :colors (cl-ppcre:split "," |colors|)
								   :text |text|
								   ;; :text (cl-ppcre:split "," |text|)
								   :keywords (cl-ppcre:split "," |keywords|)
								   :type |type|)))))))

(defroute "/view-deck" (&key |name|)
  (render #P"view-deck.html" `((:deck . ,(get-cards (lambda () (mtg.controller:get-deck-cards-names-and-quantities |name|)))))))

(defroute "/get-decks" ()
  (render-json (mtg.controller:get-decks)))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
