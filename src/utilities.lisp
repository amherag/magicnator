(in-package :cl-user)
(defpackage mtg.utilities
  (:use :cl :sxql :datafly :mtg.config)
  (:export :init-database :read-file :write-file))
(in-package :mtg.utilities)

(defun init-database ()
  "Creates all the necessary tables for MTG."
  (with-postgres-connection
      (execute (create-table (:cards :if-not-exists t)
		   ((id :type '(:varchar 36)
			:primary-key t)
		    (name :type '(:varchar 64))
		    (json :type :text))))

    (execute (create-table (:users :if-not-exists t)
		 ((id :type '(:varchar 36)
		      :primary-key t)
		  (name :type '(:varchar 64))
		  (password :type '(:varchar 64)))))
    
    (execute (create-table (:collection :if-not-exists t)
		 ((id :type '(:varchar 36)
		      :primary-key t)
		  (user-id :type '(:varchar 36))
		  (quantity :type 'integer
			    :not-null t)
		  (used :type 'integer
			:not-null t))))
    (execute (create-table (:decks :if-not-exists t)
		 ((id :type '(:varchar 36)
		      :primary-key t)
		  (name :type '(:varchar 64)))))
    (execute (create-table (:collection-decks :if-not-exists t)
		 ((deck-id :type '(:varchar 36)
			   :not-null t)
		  (card-id :type '(:varchar 36)
			   :not-null t)
		  (quantity :type 'integer
			    :not-null t))))))
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
