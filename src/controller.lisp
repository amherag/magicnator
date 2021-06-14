(ql-dist:disable (ql-dist:find-dist "ultralisp"))
;; (ql:quickload :mtg)
;; (mtg:start :port 2001)
(in-package :cl-user)
(defpackage mtg.controller
  (:use :cl :mtg.config :postmodern :alexandria :access :mtg.utilities :trivial-download)
  (:export
   :get-deck-card-names
   :get-collection-card-names
   :get-collection
   :get-deck-cards-names-and-quantities
   :get-cards-image-pathspec

   :get-decks
   
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

(defmacro comment (&rest body))

(defun scryfall-uri (endpoint args)
  (let ((args (format nil "~{~a~^&~}"(loop for arg in args collect (format nil "~a=~a" (car arg) (cdr arg))))))
    (format nil "~a~a?~a" *scryfall-url* endpoint args)))
;; (scryfall-uri "/cards/named" `(("exact" . "Shock")
;; 			       ("exact" . "Shock")))

(defun get-db-card-by-name (name)
  (access (conn (query (:select 'json :from 'cards :where (:= 'name (string-upcase name)))
		       :alist))
	  :json))
;; (json:decode-json-from-string (get-db-card-by-name "Forest"))

(defun get-db-card-by-id (id)
  (access (conn (query (:select 'json :from 'cards :where (:= 'id id))
		       :alist))
	  :json))
;; (get-db-card-by-id "7ef83f4c-d3ff-4905-a16d-f2bae673a5b2")

(defun get-scryfall-card-by-name (name)
  ;; Being a good citizen.
  (sleep 0.2)
  (let ((name (do-urlencode:urlencode name)))
    (dex:get (scryfall-uri "/cards/named" `(("exact" . ,name))))))

(defun insert-card-to-db (json)
  (let* ((json-object (json:decode-json-from-string json))
	 (id (access json-object :id))
	 (name (string-upcase (access json-object :name)))
	 (oracle-text (access json-object :oracle--text))
	 (power (access json-object :power))
	 (toughness (access json-object :toughness))
	 (colors (apply #'vector (access json-object :colors)))
	 (cmc (access json-object :cmc))
	 (type-line (access json-object :type--line))
	 (keywords (apply #'vector (access json-object :keywords)))
	 (set (access json-object :set))
	 (set-name (access json-object :set-name))
	 (rarity (access json-object :rarity))
	 (flavor-text (access json-object :flavor--text))
	 (price (let ((price (accesses json-object :prices :usd))) (if price (read-from-string price) 0.0))))
    ;; Maybe the card was not recognized by name,
    ;; but we already have it if we search by id.
    (unless (get-db-card-by-id id)
      (conn (query (:insert-into 'cards :set
				 'id id
				 'name name
				 'json json
				 ))))))

(defun drop-all-cards ()
  (conn (query (:delete-from 'cards))))
;; (drop-all-cards)

(defun drop-everything ()
  (conn (query (:delete-from 'cards))
	(query (:delete-from 'collection))
	(query (:delete-from 'decks))
	(query (:delete-from 'collection-decks))))
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

(defun get-cards-with-keywords (keywords &optional (all-or-any? :any))
  (let ((keywords (if (listp keywords) (apply #'vector keywords) keywords)))
    (conn (if (eq all-or-any? :any)
	      (query (:select '* :from 'cards :where (:&& 'keywords (:array[] keywords))) :alists)
	      (query (:select '* :from 'cards :where (:@> 'keywords (:array[] keywords))) :alists)))))
;; (get-cards-with-keywords '("Flash"))

(defun get-collection-card-ids ()
  (conn (query (:select 'id :from 'collection) :column)))
;; (get-collection-card-ids)

(defun get-cards-with-keywords-from-collection (keywords &optional (all-or-any? :any))
  (let ((keywords (if (listp keywords) (apply #'vector keywords) keywords)))
    (conn (if (eq all-or-any? :any)
	      (query (:select '* :from 'cards :where
			      (:and (:&& 'keywords (:array[] keywords)) ;; can have any
				    (:in 'id (:set (get-collection-card-ids))))) :alists)
	      (query (:select '* :from 'cards :where
			      (:and (:@> 'keywords (:array[] keywords)) ;; must have all
				    (:in 'id (:set (get-collection-card-ids))))) :alists)))))

(defun get-cards-with-type-from-collection (type)
  (conn (query (:select '* :from 'cards :where
			(:and (:like 'type-line (format nil "%~a%" type)) ;; can have any
			      (:in 'id (:set (get-collection-card-ids))))) :alists)))

;; (get-cards-with-type-from-collection "Knight")


(defun get-collection (&key (name "") text keywords type colors)
  (let* ((keywords (if (listp keywords) (apply #'vector keywords) keywords))
	 ;; (colors (if colors colors '("W" "B" "U" "R" "G")))
	 (colors (if (and colors (listp colors)) (apply #'vector colors) colors)))
    (conn (query (:select '* (:as (:- 'collection.quantity 'collection.used) :free)
			  :from 'cards
			  :inner-join 'collection
			  :on (:= 'cards.id 'collection.id)
			  :where
			  (:or
			   (:= (:cardinality 'colors) (if colors 0 99999))
			   (:&& 'colors (:array[] (:array[] (if colors colors #("ZZZ")))))
			   (:like 'cards.name (if name (format nil "%~a%" (string-upcase name)) "zzzzzz"))
			   (:like 'oracle-text (if text (format nil "%~a%" text) "zzzzzz"))
			   (:like 'type-line (if type (format nil "%~a%" type) "zzzzzzzzz"))
			   ;; (:@> 'keywords (:array[] keywords))
			   (:@> 'keywords (:array[] keywords)))	;; can have any
			  ) :alists))))

;; (get-collection :keywords '("Flash") :type "Knight")
;; (get-collection nil "Knight")

;; (get-collection)
;; (get-collection :keywords '("Flash"))

;; (loop for card in (get-cards-with-keywords-from-collection '("Flying" "Flash") :any)
;;    do (format t "~a~%" (access card :name)))

(defun get-card-mana-cost (name)
  (let ((card (json:decode-json-from-string (get-card name))))
    (access card :mana--cost)))

(defun get-card-name-by-id (id)
  (access (json:decode-json-from-string (get-db-card-by-id id)) :name))

(defun check-card-in-collection-by-id (id)
  (access (conn (query (:select 'quantity :from 'collection :where (:= 'id id)) :alist)) :quantity))

(defun increase-card-in-collection-by-id (id)
  (conn (query (:update 'collection :set 'quantity (:+ 'quantity 1) :where (:= 'id id)))))

(defun add-card-to-collection (name)
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id)))
    (if (check-card-in-collection-by-id id)
	(increase-card-in-collection-by-id id)
	(conn (query (:insert-into 'collection :set 'id id 'quantity 1 'used 0))))))

(defun get-quantity-card-on-collection (name)
  (access (get-card-from-collection name) :quantity))

(defun get-used-card-on-collection (name)
  (access (get-card-from-collection name) :used))

;; (add-card-to-collection "Shock")

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
    (conn (query (:update 'collection :set 'used (:+ 'used count) :where (:= 'id id))))))

(defun decrease-used-card-on-collection (name &optional (count 1))
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id))
	 (used (get-used-card-on-collection name)))
    (if (check-card-in-collection-by-id id)
	(when (and used (> used 0))
	  (conn (query (:update 'collection :set 'used (:- 'used count) :where (:= 'id id)))))
	(error "You don't have this card."))))

(defun remove-card-from-collection (name)
  (let* ((card (json:decode-json-from-string (get-card name)))
	 (id (access card :id))
	 (quantity (check-card-in-collection-by-id id)))
    (conn (if (and quantity (> quantity 1))
	      (query (:update 'collection :set 'quantity (:- 'quantity 1) :where (:= 'id id)))
	      (query (:delete-from 'collection :where (:= 'id id)))))))

(defun get-deck (name)
  (let ((name (string-upcase name)))
    (conn (query (:select '* :from 'decks :where (:= 'name name)) :alist))))

(defun remove-deck (name)
  (let* ((deck-name (string-upcase name))
	 (deck (get-deck deck-name))
	 (deck-id (access deck :id))
	 (cards (when deck-id
		  (conn (query (:select '* :from 'collection-decks :where (:= 'deck-id deck-id)) :alists))))
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
      ;; Removing relationship between deck and collection cards.
      (conn (query (:delete-from 'collection-decks :where (:= 'deck-id deck-id)))))
    ;; Removing deck.
    (when deck-id
      (conn (query (:delete-from 'decks :where (:= 'name deck-name)))))))
;; (remove-deck "Ashiok")

(defun list-decks ()
  (loop for i from 1
     for deck in (conn (query (:select 'name :from 'decks) :alists))
	do (format t "~a. ~a~%" i (access deck :name))))

(defun get-decks ()
  (conn (query (:select '* :from 'decks) :alists)))
;; (get-decks)

(comment
 (progn
   (list-decks)
   (format t "~%")
   (describe-deck "ashiok")
   (format t "~%")
   (describe-deck "elspeth")
   (format t "~%")
   (describe-deck "adventure")
   (format t "~%")
   (describe-deck "knight")
   (format t "~%")
   (describe-deck "amass")
   (format t "~%")
   (describe-deck "draw")
   (format t "~%")
   (describe-deck "simicflash")
   (format t "~%")
   (describe-deck "cycling")))

;; (list-decks)
;; (get-deck "Ashiok")
;; (create-deck "SimicFlash")
;; (create-deck "Elspeth")
;; (remove-deck "Ashiok")
;; (remove-deck "Flash")

(defun create-deck (name)
  (let ((name (string-upcase name)))
    (unless (get-deck name)
      (conn (query (:insert-into 'decks :set 'id (format nil "~a" (uuid:make-v4-uuid)) 'name name))))))

(defun deck-has-card? (card-name deck-name)
  (let ((card (json:decode-json-from-string (get-card card-name)))
	(deck (get-deck deck-name)))
    (access (conn (query (:select '* :from 'collection-decks :where (:and (:= 'deck-id (access deck :id))
									  (:= 'card-id (access card :id))))
			 :alist))
	    :quantity)))

(defun add-card-to-deck (card-name deck-name)
  (let ((card (json:decode-json-from-string (get-card card-name)))
	(deck (get-deck deck-name)))
    (increase-used-card-on-collection card-name)
    (if (deck-has-card? card-name deck-name)
	(conn (query (:update 'collection-decks :set 'quantity (:+ 'quantity 1)
			      :where (:and (:= 'deck-id (access deck :id))
					   (:= 'card-id (access card :id))))))
	(conn (query (:insert-into 'collection-decks :set
				   'deck-id (access deck :id)
				   'card-id (access card :id)
				   'quantity 1))))))

(defun remove-card-from-deck (card-name deck-name)
  (let ((card (json:decode-json-from-string (get-card card-name)))
	(deck (get-deck deck-name)))
    (decrease-used-card-on-collection card-name)
    (when-let ((quantity (deck-has-card? card-name deck-name)))
      (if (and quantity (> quantity 1))
	  (conn (query (:update 'collection-decks :set 'quantity (:- 'quantity 1) :where
				(:and (:= 'deck-id (access deck :id))
				      (:= 'card-id (access card :id))))))
	  (conn (query (:delete-from 'collection-decks :where
				(:and (:= 'deck-id (access deck :id))
				      (:= 'card-id (access card :id))))))))))

(defun describe-deck (name)
  (let* ((deck (get-deck name))
	 (deck-id (access deck :id))
	 (cards (when deck-id (get-deck-cards name)))
	 (cards-ids (loop for card in cards collect (access card :card-id)))
	 (cards-quantities (loop for card in cards collect (access card :quantity)))
	 (cards-names (when cards-ids
			(loop for id in cards-ids collect (get-card-name-by-id id)))))
    (when (and deck-id cards-names cards-quantities)
      (format t "//Deck Name: ~a (~a)~%~%" name (reduce #'+ cards-quantities))
      (loop for i from 1
	 for name in cards-names
	 for quantity in cards-quantities
	 do (format t "~a ~a~%" quantity name)))))

(defun get-deck-cards-names-and-quantities (name)
  (let* ((deck (get-deck name))
	 (deck-id (access deck :id))
	 (cards (when deck-id (get-deck-cards name)))
	 (cards-ids (loop for card in cards collect (access card :card-id)))
	 (cards-quantities (loop for card in cards collect (access card :quantity)))
	 (cards-names (when cards-ids
			(loop for id in cards-ids collect (get-card-name-by-id id)))))
    (when (and deck-id cards-names cards-quantities)
      (loop
	 for name in cards-names
	 for quantity in cards-quantities
	 collect `((:name . ,name)
		   (:quantity . ,quantity))))))
;; (get-deck-cards-names-and-quantities "Ashiok")

;; (get-deck-cards "Ashiok")

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
  (access (conn (query (:select (:sum 'quantity) :from 'collection) :alist)) :sum))

;; (get-collection-size)
;; (get-collection-uniques-size)

(defun get-collection-uniques-size ()
  (access (conn (query (:select (:count 'quantity) :from 'collection) :alist)) :count))

(defun get-collection-card-names ()
  (loop for card in (conn (query (:select 'name :from 'cards :where (:in 'id (:select 'id :from 'collection)))))
     collect (car card)))
;; (get-collection-card-names)

(defun describe-collection ()
  (let ((cards (conn (query (:select '* :from 'collection) :alists))))
    (loop for card in (reverse cards)
       do (format t "Name: ~35a ~t Quantity: ~4a ~t Used: ~a~%"
		  (get-card-name-by-id (access card :id))
		  (access card :quantity)
		  (access card :used)))))
;; (describe-collection)

(defun describe-unused-collection ()
  (let ((cards (conn (query (:select '* :from 'collection :where (:= 'used 0)) :alists))))
    (loop for card in (reverse cards)
       do (format t "Name: ~35a ~t Quantity: ~4a ~t Used: ~a~%"
		  (get-card-name-by-id (access card :id))
		  (access card :quantity)
		  (access card :used)))))
;; (describe-unused-collection)

(defun get-quantity-unused-collection ()  
  (let ((cards (conn (query (:select '* :from 'collection :where (:= 'used 0)) :alists))))
    (length cards)))
;; (get-quantity-unused-collection)

(defun get-deck-size (name)
  (loop for card in (get-deck-cards name) sum (access card :quantity)))
;; (get-deck-size "Ashiok")

(defun get-deck-cards (name)
  (let* ((deck (get-deck name)))
    (conn (query (:select '* :from 'collection-decks :where (:= 'deck-id (access deck :id))) :alists))))
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

;; (add-card-to-deck "Rimrock Knight // Boulder Rush" "Knight")
;; (add-card-to-deck "Raging Redcap" "Knight")
;; (add-card-to-deck "Ardenvale Tactician // Dizzying Swoop" "Knight")
;; (add-card-to-deck "Ardenvale Paladin" "Knight")
;; (add-card-to-deck "Venerable Knight" "Knight")
;; (add-card-to-deck "Syr Alin, the Lion's Claw" "Knight")
;; (add-card-to-deck "Jousting Dummy" "Knight")
;; (add-card-to-deck "Burning-Yard Trainer" "Knight")
;; (add-card-to-deck "Leonin of the Lost Pride" "Knight")
;; (add-card-to-deck "Terror of Mount Velus" "Knight")
;; (add-card-to-deck "Hero of the Winds" "Knight")
;; (add-card-to-deck "Archon of Falling Stars" "Knight")
;; (add-card-to-deck "Mysterious Pathlighter" "Knight")
;; (add-card-to-deck "Lonesome Unicorn // Rider in Need" "Knight")

;; (add-card-to-deck "Wind-Scarred Crag" "Knight")
;; (add-card-to-deck "Mountain" "Knight")
;; (add-card-to-deck "Plains" "Knight")
;; (add-card-to-deck "Castle Ardenvale" "Knight")

;; (remove-card-from-deck "Wings of Hubris" "Knight")
;; (remove-card-from-deck "Final Flare" "Knight")
;; (remove-card-from-deck "Leonin of the Lost Pride" "Elspeth")
;; (add-card-to-deck "Nylea, Keen-Eyed" "Adventure")
;; (add-card-to-deck "Trapped in the Tower" "Knight")
;; (describe-deck "Ashiok")
;; (list-decks)
;; (describe-collection)


;; (add-card-to-deck "Thaumaturge's Familiar" "Knight")

;; (create-deck "Draw")
;; (create-deck "Ashiok")
;; (describe-deck "Draw")
;; (get-deck "Ashiok")
;; (list-decks)

;; (create-deck "Cycling")
;; (add-card-to-deck "Hypnotic Sprite // Mesmeric Glare" "Draw")
;; (add-card-to-collection "Sunlit Hoplite")

(comment
 ;; (remove-card-from-deck "Thunderous Snapper" "Draw")
 ;; (remove-card-from-collection "Startling Development")
 ;; (list-decks)
 
 (let ((deck-name "Cycling")
       (name "Forest")
       (n 7))
   (loop repeat n do
     (add-card-to-collection name)
     ;; (add-card-to-deck name deck-name)
     ;; (remove-card-from-deck name deck-name)
     ;; (remove-card-from-collection name)
	 )
   
   ;; (describe-deck deck-name)
   ;; (describe-collection)
   (format t "~%~%Added ~a ~a~%~%" n name)
   )

 ;; (describe-deck "Cycling")
 )
;; (remove-card-from-deck "Relentless Advance" "Amass")
;; (describe-deck "Amass")

;; (describe-unused-collection)
;; (describe-collection)
;; (get-quantity-card-on-collection "Hypnotic Sprite")
;; (get-used-card-on-collection "Hypnotic Sprite")
;; (json:decode-json-from-string (get-card "Arcanist's Owl"))
;; (json:decode-json-from-string (get-card "Shock"))
;; (json:decode-json-from-string (get-card "Bastion of Remembrance"))
;; (json:decode-json-from-string (get-card "Gingerbrute"))
;; (json:decode-json-from-string (get-card "Spire Mangler"))
;; (get-db-card-by-name "Hypnotic Sprite")

(defun extract-metadata-from-json ()
  (loop for card in (conn (query (:select '* :from 'cards) :alists))
     do (let* ((id (access card :id))
	       (c (json:decode-json-from-string (access card :json)))
	       (oracle-text (access c :oracle--text))
	       (power (access c :power))
	       (toughness (access c :toughness))
	       (colors (apply #'vector (access c :colors)))
	       (cmc (access c :cmc))
	       (type-line (access c :type--line))
	       (keywords (apply #'vector (access c :keywords)))
	       (set (access c :set))
	       (set-name (access c :set-name))
	       (rarity (access c :rarity))
	       (flavor-text (access c :flavor--text))
	       (price (let ((price (accesses c :prices :usd))) (if price (read-from-string price) 0.0))))
	  (conn (query (:update 'cards :set
				'oracle-text oracle-text
				'power power
				'toughness toughness
				'colors colors
				'cmc cmc
				'type-line type-line
				'keywords keywords
				'set set
				'set-name set-name
				'rarity rarity
				'flavor-text flavor-text
				'price price
				:where (:= 'id id)))
		;; (query (:update 'cards :set 'power power :where (:= 'id id)))
		)
	  )))
;; (conn (query (:alter-table 'cards :add-column 'flavor-text :type (or db-null string))))
;; (conn (query (:alter-table 'cards :add-column 'price :type (or db-null float))))
;; (conn (query (:alter-table 'cards :add-column 'keywords :type (or db-null text[]))))

;; (deck-has-card? "Ashiok's Forerunner" "Ashiok")

;; (add-card-to-deck "Devourer of Memory" "Ashiok")
;; (get-used-card-on-collection "Ashiok's Forerunner")
;; (get-quantity-card-on-collection "Ashiok's Forerunner")
;; (add-card-to-collection "Ashiok's Forerunner")

;; (add-card-to-collection "Sleep of the Dead")

(defun init-to-deck (card-name deck-name)
  (add-card-to-collection card-name)
  (add-card-to-deck card-name deck-name)
  (get-quantity-card-on-collection card-name))

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
    (conn (query (:select '* :from 'collection :where (:= 'id id)) :alist))))

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

(let ((library (get-shuffled-deck-cards "Knight"))
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
    (untap-all)
    (draw-cards-to-hand 1)
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

  (defun drop-token (name &key (tapped nil) (left-counter 0) (right-counter 0) (is-artifact? nil))
    (add-to-log (format nil "Summon [[~a]]." name))
    (if is-artifact?
	(push `(,name
		,(if tapped '"Tapped" "Untapped")
		,left-counter)
	      artifacts)
	(let ((power-toughness (get-card-power-toughness name)))
	  (push `(,name
		  ,(if tapped '"Tapped" "Untapped")
		  ,(+ left-counter (first power-toughness))
		  ,(+ right-counter (second power-toughness))
		  (""))
		creatures)))
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
		     ,(if (numberp (first power-toughness)) (+ left-counter (first power-toughness)) "*")
		     ,(if (numberp (second power-toughness)) (+ right-counter (second power-toughness)) "*")
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

;; (save-game "Knight.mtg")
;; (load-game "Knight.mtg")

;; (drop-token "Goat")
;; (drop-token "Sunder Shaman")
;; (drop-token "Spider")
;; (drop-token "Food" :is-artifact? t)

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

;; (destroy-card 1 :lands)
;; (destroy-card 2 :creatures)
;; (destroy-card 1 :artifacts)
;; (destroy-card 1 :planeswalkers)
;; (return-card 1 :hand)

;; (advance-turn)
;; (print-board)
;; (add-life -2)
;; (add-life 2)
;; (unsummon-card 1 :graveyard)
;; (unsummon-card 1 :creatures)

;; (tap-card 1 :lands)
;; (tap-card 2 :lands)
;; (tap-card 3 :lands)
;; (tap-card 4 :lands)
;; (tap-card 5 :lands)
;; (tap-card 6 :lands)

;; (drop-card-from-hand "Plains" :lands :count 1 :tapped nil)
;; (drop-card-from-hand "Mountain" :lands :count 1 :tapped nil)
;; (drop-card-from-hand "Venerable Knight" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Ardenvale Paladin" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Inspiring Veteran" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Bronze Sword" :artifacts :count 1 :tapped nil)
;; (drop-card-from-hand "Fateful End" :graveyard :count 1 :tapped nil)
;; (drop-card-from-hand "Scorching Dragonfire" :graveyard :count 1 :tapped nil)

;; (drop-card-from-hand "Plains" :lands :count 1 :tapped nil)
;; (drop-card-from-hand "Hero of the Pride" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Sunlit Hoplite" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Eidolon of Inspiration" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Daxos, Blessed by the Sun" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Pious Wayfarer" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Banishing Light" :artifacts :count 1 :tapped nil)
;; (drop-card-from-hand "Karametra's Blessing" :graveyard :count 1 :tapped nil)

;; (drop-card-from-hand "Forest" :lands :count 1 :tapped nil)
;; (drop-card-from-hand "Wolfwillow Haven" :lands :count 1 :tapped nil)
;; (drop-card-from-hand "Flaxen Intruder // Welcome Home" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Nylea's Huntmaster" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Arasta of the Endless Web" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Ilysian Caryatid" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Moss Viper" :creatures :count 1 :tapped nil)
;; (drop-card-from-hand "Altar of the Pantheon" :artifacts :count 1 :tapped nil)
;; (drop-card-from-hand "Icon of Ancestry" :artifacts :count 1 :tapped nil)
;; (drop-card-from-hand "Return to Nature" :graveyard :count 1 :tapped nil)
;; (drop-card-from-hand "Nylea's Forerunner" :exiled :count 1 :tapped nil)
;; (drop-card-from-hand "Vivien, Champion of the Wilds" :planeswalkers :count 1 :tapped nil)

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
;; (add-counters 1 :place :creatures :left-counter 0 :right-counter -1)
;; (add-counters 1 :place :planeswalkers :left-counter -5 :right-counter 0)

;; (exile-card 2 :hand)
;; (tap-card 1 :lands)
;; (tap-card 1 :creatures)
;; (tap-card 2 :artifacts)
;; (untap-card 2 :lands)

;; (draw-cards-to-hand 1)
;; (mill-cards 1)
;; (print-lands)
;; (print-hand)
;; (print-board)
;; (print-library-size)
