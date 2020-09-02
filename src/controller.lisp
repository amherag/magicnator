;; (ql:quickload :mtg)
;; (mtg:start :port 2001)
(in-package :cl-user)
(defpackage mtg.controller
  (:use :cl :mtg.config :sxql :datafly :alexandria :access :mtg.utilities :trivial-download)
  (:export
   :get-deck-card-names
   :get-cards-image-pathspec
   
   :get-hand
   :get-graveyard
   :get-exiled
   :get-creatures
   :get-lands
   :get-artifacts
   :get-planeswalkers
   :get-life
   :get-log))
(in-package :mtg.controller)

(defun scryfall-uri (endpoint args)
  (let ((args (format nil "~{~a~^&~}"(loop for arg in args collect (format nil "~a=~a" (car arg) (cdr arg))))))
    (format nil "~a~a?~a" *scryfall-url* endpoint args)))
;; (scryfall-uri "/cards/named" `(("exact" . "Shock")
;; 			       ("exact" . "Shock")))

;; (with-postgres-connection (retrieve-one (select :*
;; 					  (from :cards)
;; 					  (where (:= :name (string-upcase "Shock"))))))

(defun get-db-card-by-name (name)
  (access (with-postgres-connection
	      (retrieve-one (select :json
			      (from :cards)
			      (where (:= :name (string-upcase name))))))
	  :json))
;; (json:decode-json-from-string (get-db-card-by-name "Forest"))

(defun get-db-card-by-id (id)
  (access (with-postgres-connection
	      (retrieve-one (select :json
			      (from :cards)
			      (where (:= :id id)))))
	  :json))

(defun get-scryfall-card-by-name (name)
  ;; Being a good citizen.
  (sleep 0.2)
  (let ((name (do-urlencode:urlencode name)))
    (dex:get (scryfall-uri "/cards/named" `(("exact" . ,name))))))

(defun insert-card-to-db (json)
  (let* ((json-object (json:decode-json-from-string json))
	 (id (access json-object :id))
	 (name (string-upcase (access json-object :name))))
    ;; Maybe the card was not recognized by name,
    ;; but we already have it if we search by id.
    (unless (get-db-card-by-id id)
      (with-postgres-connection
	  (execute (insert-into :cards
		     (set= :id id
			   :name name
			   :json json)))))))

(defun drop-all-cards ()
  (with-postgres-connection (execute (delete-from :cards))))
;; (drop-all-cards)
(defun drop-everything ()
  (with-postgres-connection
      (execute (delete-from :cards))
    (execute (delete-from :collection))
    (execute (delete-from :decks))
    (execute (delete-from :collection-decks))))
;; (drop-everything)

;; (insert-card-to-db (get-scryfall-card-by-name "Shock"))
;; (get-scryfall-card-by-name "Ashiok's Forerunner")
;; (get-scryfall-card-by-name "Ashiok, Sculptor of Fears")
;; (json:decode-json-from-string (get-scryfall-card-by-name "Swamp"))
;; (get-db-card-by-name "Ashiok's Forerunner")
;; (get-db-card-by-id "59fa8e8d-bcb8-47bf-b71a-df11c8d0f2c9")

;; (get-card-loyalty "Ashiok, Sculptor of Fears")

(defun get-card-power-toughness (name)
  (let ((card (json:decode-json-from-string (get-card name))))
    (list (read-from-string (access card :power))
	  (read-from-string (access card :toughness)))))

(defun get-card-loyalty (name)
  (let ((card (json:decode-json-from-string (get-card name))))
    (read-from-string (access card :loyalty))))

;; (defparameter *swamp* (decode-json-from-string (get-scryfall-card-by-name "Swamp")))
;; (defparameter *elf* (decode-json-from-string (get-scryfall-card-by-name "Llanowar Elves")))
;; (defparameter *banner* (decode-json-from-string (get-scryfall-card-by-name "Heraldic Banner")))

;; (access (decode-json-from-string *coco*) :name)

(defun get-card (name)
  ;; Checking if card is in our database.
  (let* ((json (if-let ((db-card (get-db-card-by-name name)))
		 db-card
		 (get-scryfall-card-by-name name))))
    ;; Inserting card to db. If it already exists, INSERT-CARD-TO-DB will know.
    (insert-card-to-db json)
    json))
;; (length "59fa8e8d-bcb8-47bf-b71a-df11c8d0f2c9")

(defun get-card-mana-cost (name)
  (let ((card (json:decode-json-from-string (get-card name))))
    (access card :mana--cost)))

(defun get-card-name-by-id (id)
  (access (json:decode-json-from-string (get-db-card-by-id id)) :name))

(defun check-card-in-collection-by-id (id)
  (access (with-postgres-connection
	      (retrieve-one (select :quantity
			      (from :collection)
			      (where (:= :id id)))))
	  :quantity))

(defun increase-card-in-collection-by-id (id)
  (with-postgres-connection
      (execute (update :collection
		 (set= :quantity (:+ :quantity 1))
		 (where (:= :id id))))))

(defun add-card-to-collection (name)
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id)))
    (if (check-card-in-collection-by-id id)
	(increase-card-in-collection-by-id id)
	(with-postgres-connection
	    (execute (insert-into :collection
		       (set= :id id
			     :quantity 1
			     :used 0)))))))

(defun get-quantity-card-on-collection (name)
  (access (get-card-from-collection name) :quantity))

(defun get-used-card-on-collection (name)
  (access (get-card-from-collection name) :used))

;; (add-card-to-collection "Shock")
;; (get-used-card-on-collection "Shock")
;; (get-quantity-card-on-collection "Shock")
;; (increase-used-card-on-collection "Shock")
;; (decrease-used-card-on-collection "Shock")

;; (add-card-to-collection "Ashiok's Forerunner")
;; (remove-card-from-collection "Ashiok's Forerunner")
;; (get-used-card-on-collection "Ashiok's Forerunner")
;; (get-quantity-card-on-collection "Ashiok's Forerunner")
;; (increase-used-card-on-collection "Ashiok's Forerunner")
;; (decrease-used-card-on-collection "Ashiok's Forerunner")
;; (get-used-card-on-collection "Shock")

(defun increase-used-card-on-collection (name &optional (count 1))
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id))
	 (quantity (check-card-in-collection-by-id id))
	 (used (get-used-card-on-collection name)))
    (unless quantity
      (error "You don't have this card."))
    (when (>= used quantity)
      (error "You don't have more copies of this card."))
    (with-postgres-connection
	(execute (update :collection
		   (set= :used (:+ :used count))
		   (where (:= :id id)))))))

(defun decrease-used-card-on-collection (name &optional (count 1))
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id))
	 (used (get-used-card-on-collection name)))
    (if (check-card-in-collection-by-id id)
	(when (and used (> used 0))
	    (with-postgres-connection
		(execute (update :collection
			   (set= :used (:- :used count))
			   (where (:= :id id))))))
	(error "You don't have this card.")
	)))

(defun remove-card-from-collection (name)
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id))
	 (quantity (check-card-in-collection-by-id id)))
    (with-postgres-connection
	(if (and quantity (> quantity 1))
	    (execute (update :collection
		       (set= :quantity (:- :quantity 1))
		       (where (:= :id id))))
	    (execute (delete-from :collection
		       (where (:= :id id))))))))

(defun get-deck (name)
  (let ((name (string-upcase name)))
    (with-postgres-connection
	(retrieve-one (select :*
			(from :decks)
			(where (:= :name name)))))))

(defun remove-deck (name)
  (let* ((deck-name (string-upcase name))
	 (deck (get-deck deck-name))
	 (deck-id (access deck :id))
	 (cards (when deck-id
		  (with-postgres-connection
		      (retrieve-all (select :*
				      (from :collection-decks)
				      (where (:= :deck-id deck-id)))))))
	 (cards-ids (loop for card in cards collect (access card :card-id)))
	 (cards-names (when cards-ids
			(loop for id in cards-ids collect (get-card-name-by-id id))))
	 (cards-quantities (loop for card in cards collect (access card :quantity)))
	 )
    (when cards-names
      (loop for name in cards-names
    	 for quantity in cards-quantities
    	 do (decrease-used-card-on-collection name quantity)))
    
    (when deck-id
      (with-postgres-connection
	  ;; Removing relationship between deck and collection cards.
	  (execute (delete-from :collection-decks
		     (where (:= :deck-id deck-id))))))
    ;; Removing deck.
    (when deck-id
      (with-postgres-connection
	  (execute (delete-from :decks
		     (where (:= :name deck-name))))))
    ))
;; (remove-deck "Ashiok")

(defun list-decks ()
  (loop for i from 1
     for deck in (with-postgres-connection
		     (retrieve-all (select :name
				     (from :decks))))
     do (format t "~a. ~a~%" i (access deck :name))))

;; (list-decks)
;; (get-deck "Ashiok")
;; (create-deck "Ashiok")
;; (create-deck "Elspeth")
;; (remove-deck "Ashiok")
;; (remove-deck "Elspeth")

(defun create-deck (name)
  (let ((name (string-upcase name)))
    (unless (get-deck name)
      (with-postgres-connection
	  (execute (insert-into :decks
		     (set= :id (format nil "~a" (uuid:make-v4-uuid))
			   :name name)))))))

(defun deck-has-card? (card-name deck-name)
  (let ((card (json:decode-json-from-string (get-card card-name)))
	(deck (get-deck deck-name)))
    (access (with-postgres-connection
		(retrieve-one (select :*
				(from :collection-decks)
				(where (:and (:= :deck-id (access deck :id))
					     (:= :card-id (access card :id)))))))
	    :quantity)))

(defun add-card-to-deck (card-name deck-name)
  (let ((card (json:decode-json-from-string (get-card card-name)))
	(deck (get-deck deck-name)))
    (increase-used-card-on-collection card-name)
    (if (deck-has-card? card-name deck-name)
	(with-postgres-connection
	    (execute (update :collection-decks
		       (set= :quantity (:+ :quantity 1))
		       (where (:and (:= :deck-id (access deck :id))
				    (:= :card-id (access card :id)))))))
	(with-postgres-connection
	    (execute (insert-into :collection-decks
		       (set= :deck-id (access deck :id)
			     :card-id (access card :id)
			     :quantity 1)))))))

(defun remove-card-from-deck (card-name deck-name)
  (let ((card (json:decode-json-from-string (get-card card-name)))
	(deck (get-deck deck-name)))
    (decrease-used-card-on-collection card-name)
    (with-postgres-connection
	(when-let ((quantity (deck-has-card? card-name deck-name)))
	  (if (and quantity (> quantity 1))
	      (execute (update :collection-decks
			 (set= :quantity (:- :quantity 1))
			 (where (:and (:= :deck-id (access deck :id))
				      (:= :card-id (access card :id))))))
	      (execute (delete-from :collection-decks
			 (where (:and (:= :deck-id (access deck :id))
				      (:= :card-id (access card :id)))))))))))

(defun describe-deck (name)
  (let* ((deck (get-deck name))
	 (deck-id (access deck :id))
	 (cards (when deck-id (get-deck-cards name)))
	 (cards-ids (loop for card in cards collect (access card :card-id)))
	 (cards-quantities (loop for card in cards collect (access card :quantity)))
	 (cards-names (when cards-ids
			(loop for id in cards-ids collect (get-card-name-by-id id))))
	 )
    (when (and deck-id cards-names cards-quantities)
      (format t "Deck Name: ~a~%~%" name)
      (loop for i from 1
	 for name in cards-names
	 for quantity in cards-quantities
	 do (format t "~a. ~a (~a)~%" i name quantity)))
    ))

(defun get-deck-card-names (name)
  (let* ((deck (get-deck name))
	 (deck-id (access deck :id))
	 (cards (when deck-id (get-deck-cards name)))
	 (cards-ids (loop for card in cards collect (access card :card-id)))
	 ;; (cards-quantities (loop for card in cards collect (access card :quantity)))
	 (cards-names (when cards-ids
			(loop for id in cards-ids collect (get-card-name-by-id id)))))
    cards-names))
;; (get-deck-card-names "Ashiok")

(defun get-collection-size ()
  (car (access (with-postgres-connection
		   (retrieve-all (select (fields (:sum :quantity))
				   (from :collection))))
	       :sum)))

;; (get-collection-size)
;; (get-collection-uniques-size)

(defun get-collection-uniques-size ()
  (with-postgres-connection
      (length (retrieve-all (select :quantity
			      (from :collection))))))

(defun get-collection-card-names ()
  (loop for card in (with-postgres-connection
			(retrieve-all (select :name (from :cards)
					      (where (:in :id (select :id
								(from :collection)))))))
     collect (access card :name)))
;; (get-collection-card-names)

(defun describe-collection ()
  (let ((cards (with-postgres-connection
		   (retrieve-all (select :*
				   (from :collection))))))
	 
    (loop for card in cards
       do (format t "Name: ~35a ~t Quantity: ~4a ~t Used: ~a~%"
		  (get-card-name-by-id (access card :id))
		  (access card :quantity)
		  (access card :used)))))

(defun describe-unused-collection ()
  (let ((cards (with-postgres-connection
		   (retrieve-all (select :*
				   (from :collection)
				   (where (:= :used 0)))))))
	 
    (loop for card in cards
       do (format t "Name: ~35a ~t Quantity: ~4a ~t Used: ~a~%"
		  (get-card-name-by-id (access card :id))
		  (access card :quantity)
		  (access card :used)))))
;; (describe-unused-collection)

(defun get-quantity-unused-collection ()
  (let ((cards (with-postgres-connection
		   (retrieve-all (select :*
				   (from :collection)
				   (where (:= :used 0)))))))
	 
    (length cards)))
;; (get-quantity-unused-collection)

(defun get-deck-size (name)
  (loop for card in (get-deck-cards name) sum (access card :quantity)))

(defun get-deck-cards (name)
  (let* ((deck (get-deck name)))
    (with-postgres-connection
	(retrieve-all (select :*
			(from :collection-decks)
			(where (:= :deck-id (access deck :id))))))))
;; (get-deck-cards "Ashiok")

(defun store-card-image (name &optional (resolution :normal))
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id))
	 (extension (if (eq resolution :png) ".png" ".jpg"))
	 (url (accesses card :image--uris resolution))
	 (pathspec (merge-pathnames (pathname (string-downcase (format nil "img/cards/~a/~a~a" resolution id extension))) *static-directory*)))
    (unless (probe-file pathspec)
      (download url pathspec :quiet t))
    ))
;; (store-card-image "Forest")

(defun store-collection-images (&optional (resolution :normal))
  (loop for name in (get-collection-card-names)
     do (progn
	  (format t "Storing ~a (~a)~%" name resolution)
	  (sleep 1)
	  (store-card-image name resolution))))
;; (store-collection-images :png)

(defun get-card-image-pathspec (name &optional (resolution :normal))
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id))
	 (extension (if (eq resolution :png) ".png" ".jpg"))
	 ;; (url (accesses card :image--uris resolution))
	 (relative-pathspec (pathname (string-downcase (format nil "img/cards/~a/~a~a" resolution id extension))))
	 (pathspec (merge-pathnames relative-pathspec *static-directory*)))
    (if (probe-file pathspec)
	relative-pathspec
	(progn
	  (sleep 1)
	  (store-card-image name resolution)
	  relative-pathspec))
    ))
;; (get-card-image-pathspec "Forest" :large)
;; (json:decode-json-from-string (get-card "Forest"))

(defun get-cards-image-pathspec (names &optional (resolution :normal))
  (loop for name in names collect (get-card-image-pathspec name resolution)))
;; (get-cards-image-pathspec (get-deck-card-names "Ashiok"))

(defun get-individual-deck-cards (name)
  (let* ((cards (get-deck-cards name))
	 (ids (loop for card in cards collect (access card :card-id)))
	 (quantities (loop for card in cards collect (access card :quantity))))
    (flatten
     (loop
	for quantity in quantities
	for name in (loop for id in ids collect (get-card-name-by-id id))
	collect (loop for i below quantity collect name)))))
;; (get-individual-deck-cards "Ashiok")

(defun print-individual-deck-cards (name)
  (loop for name in (get-individual-deck-cards name)
     do (format t "~a~%" name)))

(defun get-shuffled-deck-cards (name)
  (shuffle (get-individual-deck-cards name)))

(defun print-shuffled-deck-cards (name)
  (loop for name in (get-shuffled-deck-cards name)
       do (format t "~a~%" name)))

;; (get-shuffled-deck-cards "Ashiok")
;; (print-shuffled-deck-cards "Ashiok")

;; (get-individual-deck-cards "Ashiok")
;; (print-individual-deck-cards "Ashiok")

;; (let ((name "Woe Strider"))
;;   (add-card-to-deck name "Ashiok")
;;   (describe-collection))

;; (create-deck "Ashiok")
;; (describe-deck "Ashiok")
;; (get-deck "Ashiok")
;; (list-decks)

;; (create-deck "Adventure")
;; (add-card-to-deck "Indomitable Will" "Elspeth")
;; (add-card-to-collection "Sunlit Hoplite")
;; (describe-deck "Elspeth")

;; (let ((name "Forest"))
;;   ;; (add-card-to-collection name)
;;   (add-card-to-deck name "Adventure")
;;   (describe-deck "Adventure")
;;   ;; (describe-unused-collection)
;;   )

;; (describe-unused-collection)
;; (describe-collection)
;; (get-quantity-card-on-collection "Hypnotic Sprite")
;; (get-used-card-on-collection "Hypnotic Sprite")
;; (get-card "Merchant of the Vale")
;; (get-db-card-by-name "Hypnotic Sprite")

;; (deck-has-card? "Ashiok's Forerunner" "Ashiok")

;; (add-card-to-deck "Devourer of Memory" "Ashiok")
;; (remove-card-from-deck "Ashiok's Forerunner" "Ashiok")
;; (get-used-card-on-collection "Ashiok's Forerunner")
;; (get-quantity-card-on-collection "Ashiok's Forerunner")
;; (add-card-to-collection "Ashiok's Forerunner")

;; (add-card-to-collection "Sleep of the Dead")

(defun init-to-deck (card-name deck-name)
  (let ((name "Omen of the Dead")
	(deck-name "Ashiok"))
    (add-card-to-collection card-name)
    (add-card-to-deck card-name deck-name)
    (get-quantity-card-on-collection card-name)))

;; (get-quantity-card-on-collection "Glimpse of Freedom")

;; (time (get-card "Shock"))
;; (remove-card-from-collection "Shock")
;; (get-card-from-collection "Devourer of Memory")
;; (remove-card-from-deck "Devourer of Memory" "Ashiok")
;; (add-card-to-collection "Devourer of Memory")
;; (get-used-card-on-collection "Devourer of Memory")
;; (get-quantity-card-on-collection "Devourer of Memory")

(defun get-card-from-collection (name)
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id)))
    (with-postgres-connection
	(retrieve-one (select :*
			(from :collection)
			(where (:= :id id))
			)))))

;; (defparameter *coco* (dex:get (scryfall-uri "/cards/named" `(("exact" . "Shock")))))

(defparameter +CELL-FORMATS+ '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

(defun format-table (stream data &key (column-label (loop for i from 1 to (length (car data))
						       collect (format nil "COL~D" i)))
				   (column-align (loop for i from 1 to (length (car data))
						    collect :left)))
  (let* ((col-count (length column-label))
	 (strtable  (cons column-label	; table header
                          (loop for row in data ; table body with all cells as strings
			     collect (loop for cell in row
					collect (if (stringp cell)
						    cell
					;else
						    (format nil "~A" cell))))))
	 (col-widths (loop with widths = (make-array col-count :initial-element 0)
			for row in strtable
			do (loop for cell in row
			      for i from 0
			      do (setf (aref widths i)
				       (max (aref widths i) (length cell))))
			finally (return widths))))
					;------------------------------------------------------------------------------------
					; splice in the header separator
    (setq strtable
	  (nconc (list (car strtable)		       ; table header
		       (loop for align in column-align ; generate separator
			  for width across col-widths
			  collect (case align
				    (:left   (format nil ":~v@{~A~:*~}"
						     (1- width)  "-"))
				    (:right  (format nil "~v@{~A~:*~}:"
						     (1- width)  "-"))
				    (:center (format nil ":~v@{~A~:*~}:"
						     (- width 2) "-")))))
		 (cdr strtable)))	; table body
					;------------------------------------------------------------------------------------
					; Generate the formatted table
    (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%" ; compile the row format
                           (loop for align in column-align
			      collect (getf +CELL-FORMATS+ align))))
	  (widths  (loop for w across col-widths collect w)))
					; write each line to the given stream
      (dolist (row strtable)
	(apply #'format stream row-fmt (mapcan #'list widths row))))))

(let ((library (get-shuffled-deck-cards "Adventure"))
      (hand)
      (graveyard)
      (exiled)
      (creatures)
      (lands)
      (artifacts)
      (planeswalkers)
      (life 20)
      (log)
      (turn 1))

  (defun get-hand ()
    hand)

  (defun get-graveyard ()
    graveyard)

  (defun get-exiled ()
    exiled)
  
  (defun get-creatures ()
    creatures)

  (defun get-lands ()
    lands)

  (defun get-artifacts ()
    artifacts)

  (defun get-planeswalkers ()
    planeswalkers)

  (defun get-life ()
    life)

  (defun get-log ()
    log)

  (defun advance-turn ()
    (incf turn)
    (add-to-log (format nil "Turn ~a." turn))
    (print-board))

  (defun print-log ()
    (format t "Log:~%~%")
    (loop
       for i from 1
       for l in (reverse log)
       do (format t "~a. ~a~%" i l)
       finally (format t "~%")))

  (defun add-to-log (msg)
    (push msg log))

  (add-to-log (format nil "Turn ~a." turn))

  (defun print-life ()
    (format t "Life: ~a~%~%" life))

  (defun add-life (count)
    (if (> count 0)
	(add-to-log (format nil "Gain ~a life." count))
	(add-to-log (format nil "Lose ~a life." count)))
    (incf life count)
    (print-board))

  (defun print-library-size ()
    (format t "Library Size: ~a~%~%" (length library)))
  
  (defun print-graveyard-size ()
    (format t "Graveyard Size: ~a~%~%" (length graveyard)))

  (defun print-exiled-size ()
    (format t "Exiled Size: ~a~%~%" (length exiled)))

  (defun untap-all ()
    (add-to-log (format nil "Untap all."))
    (loop for creature in creatures do (setf (nth 1 creature) "Untapped"))
    (loop for land in lands do (setf (nth 1 land) "Untapped"))
    (loop for artifact in artifacts do (setf (nth 1 artifact) "Untapped"))
    (print-board))

  (defun tap-untap-card (card-position place tap-untap)
    (let ((past-tense (format nil "~aped" tap-untap)))
      (decf card-position)
      (add-to-log (format nil "~a ~a."
			  tap-untap
			  (get-card-name-by-position card-position place)))
      (cond ((eq place :lands)
	     (setf (nth 1 (nth card-position lands)) past-tense))
	    ((eq place :artifacts)
	     (setf (nth 1 (nth card-position artifacts)) past-tense))
	    ((eq place :creatures)
	     (setf (nth 1 (nth card-position creatures)) past-tense)))
      (print-board)))

  (defun tap-card (card-position place)
    (tap-untap-card card-position place "Tap"))

  (defun untap-card (card-position place)
    (tap-untap-card card-position place "Untap"))

  ;; (defun scry (count)
  ;;   (subseq library 0 count))

  (defun drop-token (name &optional (tapped nil) (left-counter 0) (right-counter 0))
    (let ((power-toughness (get-card-power-toughness name)))
      (push `(,name
	      ,(if tapped '"Tapped" "Untapped")
	      ,(+ left-counter (first power-toughness))
	      ,(+ right-counter (second power-toughness))
	      (""))
	    creatures))
    (print-board))

  ;; (defun untap-all ())

  (defun get-card-name-by-position (card-position place)
    (cond ((eq place :lands)
	   (first (nth card-position lands)))
	  ((eq place :artifacts)
	   (first (nth card-position artifacts)))
	  ((eq place :creatures)
	   (first (nth card-position creatures)))
	  ((eq place :graveyard)
	   (nth card-position graveyard))
	  ((eq place :exiled)
	   (nth card-position exiled))
	  ((eq place :planeswalkers)
	   (first (nth card-position planeswalkers)))
	  ((eq place :hand)
	   (nth card-position hand))))

  (defun remove-card-by-position (card-position place)
    (cond ((eq place :lands)
	   (deletef lands (nth card-position lands) :count 1))
	  ((eq place :artifacts)
	   (deletef artifacts (nth card-position artifacts) :count 1))
	  ((eq place :creatures)
	   (deletef creatures (nth card-position creatures) :count 1))
	  ((eq place :graveyard)
	   (deletef graveyard (nth card-position graveyard) :count 1))
	  ((eq place :exiled)
	   (deletef exiled (nth card-position exiled) :count 1))
	  ((eq place :planeswalkers)
	   (deletef planeswalkers (nth card-position planeswalkers) :count 1))
	  ((eq place :hand)
	   (deletef hand (nth card-position hand) :count 1))))

  (defun unsummon-card (card-position place)
    (decf card-position)
    (let ((name (get-card-name-by-position card-position place)))
      (add-to-log (format nil "Unsummon [[~a]]." name))
      (remove-card-by-position card-position place)
      (push name hand)
      (print-board)))

  (defun destroy-card (card-position place)
    (decf card-position)
    (let ((name (get-card-name-by-position card-position place)))
      (add-to-log (format nil "Destroy [[~a]]." name))
      (remove-card-by-position card-position place)
      (push name graveyard)
      (print-board)))

  (defun return-card (card-position place)
    (decf card-position)
    (let ((name (get-card-name-by-position card-position place)))
      (add-to-log (format nil "Return [[~a]]." name))
      (remove-card-by-position card-position place)
      (push name library)
      (print-board)))

  (defun shuffle-library ()
    (setf library (shuffle library)))

  (defun exile-card (card-position place)
    (decf card-position)
    (let ((name (get-card-name-by-position card-position place)))
      (add-to-log (format nil "Exile [[~a]]." name))
      (remove-card-by-position card-position place)
      (push name exiled)
      (print-board)))

  (defun save-game (file-name)
    (write-file
     (format nil "~s"
	     (ms:marshal (list library hand graveyard exiled creatures lands artifacts planeswalkers life log turn)))
     (merge-pathnames (pathname file-name) (merge-pathnames #P"saved-games/" *application-root*))
     :action-if-exists :supersede)
    (print-board))

  (defun load-game (file-name)
    (let ((unmarsh (ms:unmarshal
		    (read-from-string
		     (read-file (merge-pathnames (pathname file-name) (merge-pathnames #P"saved-games/" *application-root*)))))))
      (setf library (nth 0 unmarsh))
      (setf hand (nth 1 unmarsh))
      (setf graveyard (nth 2 unmarsh))
      (setf exiled (nth 3 unmarsh))
      (setf creatures (nth 4 unmarsh))
      (setf lands (nth 5 unmarsh))
      (setf artifacts (nth 6 unmarsh))
      (setf planeswalkers (nth 7 unmarsh))
      (setf life (nth 8 unmarsh))
      (setf log (nth 9 unmarsh))
      (setf turn (nth 10 unmarsh))
      )
    (print-board))

  (defun drop-card-from-hand (name place &key (count 1) (tapped nil) (left-counter 0) (right-counter 0))
    (if-let ((found (find name hand :test #'string=)))
      (progn
	(setf hand (remove found hand :test #'string= :count count))
	(cond
	  ((eq place :lands)
	   (add-to-log (format nil "Drop ~a [[~a]] ~a." count name
			       (if tapped '"Tapped" "Untapped")))
	   (push `(,found ,(if tapped '"Tapped" "Untapped") ("")) lands))
	  ((eq place :creatures)
	   (add-to-log (format nil "Summon ~a [[~a]]." count name))
	   (let ((power-toughness (get-card-power-toughness found)))
	     (push `(,found
		     ,(if tapped '"Tapped" "Untapped")
		     ,(+ left-counter (first power-toughness))
		     ,(+ right-counter (second power-toughness))
		     (""))
		   creatures)))
	  ((eq place :artifacts)
	   (add-to-log (format nil "Drop ~a [[~a]]." count name))
	   (push `(,found
		   ,(if tapped '"Tapped" "Untapped")
		   ,left-counter)
		 artifacts))
	  ((eq place :graveyard)
	   (add-to-log (format nil "Cast [[~a]]." name))
	   (push found graveyard))
	  ((eq place :planeswalkers)
	   (add-to-log (format nil "Summon [[~a]]." name))
	   (push `(,found
		   ,(+ left-counter (get-card-loyalty found)))
		 planeswalkers)))))
    (print-board))

  (defun draw-cards-to-hand (count)
    (add-to-log (format nil "Draw ~a ~a."
			count (if (= count 1) "card" "cards")))
    (let ((drawn-cards (subseq library 0 count))
	  (remaining (subseq library count)))
      (setf library remaining)
      (setf hand (append hand drawn-cards))
      (print-board)))

  (defun mill-cards (count)
    (add-to-log (format nil "Mill ~a ~a."
			count (if (= count 1) "card" "cards")))
    (let ((drawn-cards (subseq library 0 count))
	  (remaining (subseq library count)))
      (setf library remaining)
      (setf graveyard (append graveyard drawn-cards))
      (print-board)))

  (defun add-counters (card-position &key
				       (place :creatures)
				       (left-counter 1)
				       (right-counter 1)
				       )
    (add-to-log (format nil "Add counters ~a/~a to [[~a]]"
			(if (>= left-counter 0)
			    (format nil "+~a" left-counter)
			    (format nil "~a" left-counter))
			(if (>= right-counter 0)
			    (format nil "+~a" right-counter)
			    (format nil "~a" right-counter))
			(cond ((eq place :creatures)
			       (first (nth card-position creatures)))
			      ((eq place :artifacts)
			       (first (nth card-position artifacts)))
			      ((eq place :planeswalkers)
			       (first (nth card-position planeswalkers))))))
    (decf card-position)
    (cond ((eq place :creatures)
	   (incf (nth 2 (nth card-position creatures)) left-counter)
	   (incf (nth 3 (nth card-position creatures)) right-counter))
	  ((eq place :artifacts)
	   (incf (nth 2 (nth card-position artifacts)) left-counter))
	  ((eq place :planeswalkers)
	   (incf (nth 1 (nth card-position planeswalkers)) left-counter))
	  )
    (print-board))

  (defun attach-enchantment (name card-position &key (count 1) (place :creatures))
    (add-to-log (format nil "Attach enchantment [[~a]] to ~a."
			name
			(cond ((eq place :creatures)
			       (first (nth (1- card-position) creatures)))
			      ((eq place :lands)
			       (first (nth (1- card-position) lands))))))
    (when-let ((found (find name hand :test #'string=)))
      (setf hand (remove found hand :test #'string= :count count))
      (decf card-position)
      (cond ((eq place :creatures)
	     (push found (nth 4 (nth card-position creatures))))
	    ((eq place :lands)
	     (push found (nth 2 (nth card-position lands)))))
      (print-board)))

  (defun print-board ()
    (print-life)
    (let ((max-size (max (length hand)
			 (length graveyard)
			 (length exiled)
			 (length creatures)
			 (length lands)
			 (length artifacts)
			 (length planeswalkers))))
      (labels ((fill-size (seq &optional (col-count 1))
		 (append seq
			 (make-list (- max-size (length seq))
				    :initial-element
				    (if (= col-count 1)
					""
					(make-list col-count
						   :initial-element ""))))))
      
	(format-table t
		      (mapcar (lambda (&rest cards) (flatten cards))
			      (iota max-size :start 1)
			      (fill-size hand)
			      (fill-size lands 3)
			      (fill-size creatures 5)
			      (fill-size artifacts 3)
			      (fill-size planeswalkers 2)
			      (fill-size graveyard)
			      (fill-size exiled)
			      )
		      :column-label
		      '("Position" "Hand" "Lands" "Tapped?" "L. Enchant." "Creatures" "Tapped?" "+/" "/+" "C. Enchant." "Artifacts" "Tapped?" "+/" "Planeswalkers" "+/" "Graveyard" "Exiled"))))
    (format t "~%")
    (print-library-size)
    (print-graveyard-size)
    (print-exiled-size)
    (print-log)
    )

  ;; Start.
  (draw-cards-to-hand 7)
  )

;; (save-game "Ashiok.mtg")
;; (load-game "Ashiok.mtg")

;; (drop-token "Goat")
;; (drop-token "Sunder Shaman")

;; (get-card-mana-cost "Sleep of the Dead")
;; (get-card-mana-cost "Mire's Grasp")
;; (get-card-mana-cost "Sweet Oblivion")
;; (get-card-mana-cost "Mindwrack Harpy")
;; (get-card-mana-cost "Underworld Charger")
;; (get-card-mana-cost "Ashiok, Sculptor of Fears")
;; (get-card-mana-cost "Ashiok's Forerunner")
;; (get-card-mana-cost "Swimmer in Nightmares")
;; (get-card-mana-cost "Pharika's Spawn")
;; (get-card-mana-cost "Glimpse of Freedom")
;; (print-board)

;; (destroy-card 1 :creatures)
;; (destroy-card 1 :artifacts)
;; (destroy-card 1 :planeswalkers)
;; (return-card 1 :hand)

;; (advance-turn)
;; (untap-all)
;; (draw-cards-to-hand 1)
;; (add-life -4)
;; (add-life 1)
;; (unsummon-card 1 :graveyard)
;; (unsummon-card 1 :creatures)

;; (tap-card 5 :lands)
;; (tap-card 6 :lands)

;; (drop-card-from-hand "Dismal Backwater" :lands :count 1 :tapped t)
;; (drop-card-from-hand "Swamp" :lands :count 1 :tapped nil)
;; (drop-card-from-hand "Island" :lands :count 1 :tapped nil)
;; (drop-card-from-hand "Mindwrack Harpy" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Swimmer in Nightmares" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Devourer of Memory" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Pharika's Spawn" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Underworld Charger" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Woe Strider" :creatures :count 1 :tapped nil :left-counter 1 :right-counter 1)
;; (drop-card-from-hand "Mire's Grasp" :graveyard :count 1 :tapped nil)
;; (drop-card-from-hand "Sleep of the Dead" :graveyard :count 1 :tapped nil)
;; (drop-card-from-hand "Sweet Oblivion" :graveyard :count 1 :tapped nil)
;; (drop-card-from-hand "Glimpse of Freedom" :graveyard :count 1 :tapped nil)
;; (drop-card-from-hand "Ashiok, Sculptor of Fears" :planeswalkers :count 1 :tapped nil)
;; (drop-card-from-hand "Ashiok's Forerunner" :creatures :count 1 :tapped nil)
;; (add-counters 1 :place :creatures :left-counter 3 :right-counter 0)
;; (add-counters 1 :place :planeswalkers :left-counter -5 :right-counter 0)

;; (exile-card 4 :graveyard)
;; (tap-card 1 :creatures)
;; (tap-card 2 :artifacts)
;; (untap-card 2 :lands)

;; (draw-cards-to-hand 1)
;; (mill-cards 2)
;; (print-lands)
;; (print-hand)
;; (print-board)
;; (print-library-size)
