(defsystem "mtg"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

	       ;; for API connections
	       "dexador"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql"

	       ;; utilities
	       "alexandria"
	       "cl-json"
	       "uuid"
	       "marshal"
	       "do-urlencode"
	       "access"
	       "trivial-download")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "controller"))
                 (:file "view" :depends-on ("config" "controller"))
                 (:file "db" :depends-on ("config"))
		 (:file "controller" :depends-on ("config" "utilities"))
		 (:file "utilities" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "mtg-test"))))
