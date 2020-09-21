(in-package :cl-user)
(defpackage mtg.utilities
  (:use :cl :mtg.config :postmodern)
  (:export :init-database :read-file :write-file))
(in-package :mtg.utilities)

(defun init-database ()
  "Creates all the necessary tables for MTG."
  (conn
   (unless (table-exists-p 'cards)
     (query (:create-table 'cards
			   ((id :type string)
			    (name :type string)
			    (json :type string)
			    (oracle-text :type string)
			    (power :type string)
			    (toughness :type string)
			    (colors :type string[])
			    (cmc :type float)
			    ;; (mana-cost :type string[])
			    (type-line :type string)
			    (keywords :type string[])
			    (set :type string)
			    (set-name :type string)
			    (rarity :type string)
			    (flavor-text :type string)
			    (price :type float))
			   (:primary-key id)))
     (query (:create-table 'collection
			   ((id :type string)
			    (user-id :type string)
			    (quantity :type integer)
			    (used :type integer))
			   (:primary-key id)))
     (query (:create-table 'decks
			   ((id :type string)
			    (name :type string))
			   (:primary-key id)))
     (query (:create-table 'collection-decks
			   ((deck-id :type string)
			    (card-id :type string)
			    (quantity :type integer))
			   (:primary-key id))))))
;; (init-database)

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun write-file (string outfile &key (action-if-exists :error))
  (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete 
				       :overwrite :append :supersede))
  (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
    (write-sequence string outstream)))
