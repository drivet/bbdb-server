#!/usr/bin/emacs --script

(package-initialize)
(require 'bbdb)
(require 'bbdb-com)
(require 'json)
(require 'cl)

(defun json-to-plist (value)
  (if (equal value nil)
      '()
    (let ((json-object-type 'plist))
      (json-read-from-string value))))

(defun delete-record-if-match (firstname lastname hash wraprec)
  (let ((record (car wraprec)))
    (if (and (equal firstname (bbdb-record-firstname record))
             (equal lastname (bbdb-record-lastname record))
             (equal hash (woe-record-hash record)))
        (progn
          (bbdb-delete-record-internal record 't)
          't)
      'nil)))

(defun delete-record-from-list (firstname lastname hash records)
  (cond ((eq records 'nil) 'nil)
        ((delete-record-if-match firstname lastname hash (car records)) 't)
        ('t (delete-record-from-list firstname lastname hash (cdr records)))))

(defun delete-record (firstname lastname hash)
  "Delete record from BBDB, using the first and last names, plus the hash
that was given when you fetched the record."
  (bbdb (concat firstname " " lastname))
  (save-current-buffer
    (set-buffer "*BBDB*")
    (if (delete-record-from-list firstname lastname hash bbdb-records)
        (save-some-buffers 't)
      'nil)))

(defun fetch-contacts-json-batch (text)
  (princ (fetch-contacts-json text)))
  
(defun fetch-contacts-json (text)
  (if (equal (bbdb text) 1)
      (save-current-buffer
        (set-buffer "*BBDB*")
        (json-encode
         (apply 'vector
                (mapcar 'bbdb-record-to-contact
                        (mapcar 'car bbdb-records)))))))

(defun bbdb-record-to-contact (record)
  (let ((fields '((:hash woe-record-hash)
                  (:firstName woe-record-firstname)
                  (:lastName woe-record-lastname)
                  (:email woe-record-mail)
                  (:organization woe-record-organization)
                  (:phone woe-record-phone)
                  (:address woe-record-address))))
    (contact-internal fields record)))

(defun contact-internal (field-list record)
  "convert a bbdb record into the list of fields given"
  (if (eq field-list 'nil)
      'nil
    (let ((field-value (funcall (cadar field-list) record)))
      (if (eq field-value 'nil)
          (contact-internal (cdr field-list) record)
        (append (list (caar field-list) field-value)
                (contact-internal (cdr field-list) record))))))
  
(defun woe-record-firstname(record)
  (bbdb-record-firstname record))

(defun woe-record-lastname(record)
  (bbdb-record-lastname record))

(defun woe-record-mail(record)
  (apply 'vector (bbdb-record-mail record)))

(defun woe-record-organization(record)
  (apply 'vector (bbdb-record-organization record)))

(defun woe-record-phone(record)
  (apply 'vector (mapcar 'bbdb-phone-to-plist (bbdb-record-phone record))))

(defun woe-record-address(record)
  (apply 'vector (mapcar 'bbdb-address-to-plist (bbdb-record-address record))))

(defun bbdb-phone-to-plist (bbdb-phone)
  "Converts a bbdb phone vector into a JSON friendly plist"
  (list ':label (bbdb-phone-label bbdb-phone)
        ':areaCode (bbdb-phone-area bbdb-phone)
        ':prefix (bbdb-phone-exchange bbdb-phone)
        ':suffix (bbdb-phone-suffix bbdb-phone)
        ':extension (bbdb-phone-extension bbdb-phone)))

(defun bbdb-address-to-plist (bbdb-address)
  "Converts a bbdb address vector into a JSON friendly plist"
  (list ':label (bbdb-address-label bbdb-address)
        ':streetLines (apply 'vector (bbdb-address-streets bbdb-address))
        ':city (bbdb-address-city bbdb-address)
        ':state (bbdb-address-state bbdb-address)
        ':postalCode (bbdb-address-postcode bbdb-address)
        ':country (bbdb-address-country bbdb-address)))

(defun woe-record-hash (record)
  (secure-hash 'md5 (prin1-to-string (list (bbdb-record-firstname record)
                                           (bbdb-record-lastname record)
                                           (bbdb-record-mail record)
                                           (bbdb-record-organization record)
                                           (bbdb-record-phone record)
                                           (bbdb-record-address record)))))

;; update related functions
(defun to-list(vec)
  "convert a vector to a list"
  (append vec 'nil))

(defun val-to-num (val)
  (if (equal val 'nil)
      0
    (string-to-number val)))
             
(defun phone-plist-to-bbdb (phone-plist)
  "Converts a plist (JSON-friendly, i.e. derived from JSON) phone
into a bbdb phone vector"
  (if (equal phone-plist nil)
      '()
    (vector (plist-get phone-plist ':label) 
            (val-to-num (plist-get phone-plist ':areaCode))
            (val-to-num (plist-get phone-plist ':prefix))
            (val-to-num (plist-get phone-plist ':suffix))
            (val-to-num (plist-get phone-plist ':extension)))))

(defun short-phone-plist-to-bbdb (phone-plist)
  "Converts a plist (JSON-friendly, i.e. derived from JSON) phone
into a bbdb phone vector, but the phone number is just a string"
  (if (equal phone-plist nil)
      '()
    (vector (plist-get phone-plist ':label) 
            (plist-get phone-plist ':number))))

(defun address-plist-to-bbdb (address-plist)
  "Converts a plist (JSON-friendly, i.e. derived from JSON)
address into a bbdb address vector"
  (if (equal address-plist nil) 
      '()
    (vector (plist-get address-plist ':label)
            (to-list (plist-get address-plist ':streetLines))
            (plist-get address-plist ':city)
            (plist-get address-plist ':state)
            (plist-get address-plist ':postalCode)
            (plist-get address-plist ':country))))

(defun update-bbdb-firstname (record firstname)
  (bbdb-record-set-field record 'firstname firstname))

(defun update-bbdb-lastname (record lastname)
  (bbdb-record-set-field record 'lastname lastname))

(defun update-bbdb-emails (record emails)
  (bbdb-record-set-field record 'mail emails))

(defun update-bbdb-organizations (record organizations)
  (bbdb-record-set-field record 'organization organizations))

(defun update-bbdb-phones (record phones)
  (bbdb-record-set-field record 'phone 
                         (mapcar 'phone-plist-to-bbdb phones) 'nil 't))

(defun update-bbdb-addresses (record addresses)
  (bbdb-record-set-field
   record 'address (mapcar 'address-plist-to-bbdb addresses) 'nil 't))
  
(defun update-record-fields (contact-fields record)
  "contact-fields is a property list of fields and values, which
will be used to update the BBDB record passed in"
  (if (equal contact-fields 'nil) 'nil
    (let ((field (car contact-fields))
          (value (cadr contact-fields)))
      (cond ((equal field ':firstName) (update-bbdb-firstname record value))
            ((equal field ':lastName) (update-bbdb-lastname record value))
            ((equal field ':email) (update-bbdb-emails record value))
            ((equal field ':organization) (update-bbdb-organizations record value))
            ((equal field ':phone) (update-bbdb-phones record (mapcar 'json-to-plist value)))
            ((equal field ':address) (update-bbdb-addresses record (mapcar 'json-to-plist value))))
      (update-record-fields (cddr contact-fields) record))))
                      
(defun update-record-if-match (firstname lastname hash contact wraprec)
  "return non-nil if there is a match"
  (let ((record (car wraprec)))
    (if (and (equal firstname (bbdb-record-firstname record))
             (equal lastname (bbdb-record-lastname record))
             (equal hash (woe-record-hash record)))
        (progn
          (update-record-fields contact record)
          (bbdb-change-record record)
          't)
      'nil)))

(defun update-record-from-list (firstname lastname hash contact records)
  (cond ((eq records 'nil) 'nil)
        ((update-record-if-match firstname lastname hash contact (car records)) 't)
        ('t (update-record-from-list firstname lastname hash contact (cdr records)))))
  
(defun update-record (firstname lastname hash contact)
  "Update a BBDB record using the first name last names, plus the
hash that was given when we fetched the record. The contact
argument is in plist format"
  (bbdb (concat firstname " " lastname))
  (save-current-buffer
    (set-buffer "*BBDB*")
    (if (update-record-from-list firstname lastname hash contact bbdb-records)
        (save-some-buffers 't)
      'nil)))

(defun bbdbcli-create (args)
  (let ((pargs (cdr args)))
    (let ((firstName (plist-get pargs ':firstName))
          (lastName (plist-get pargs ':lastName))
          (organization (plist-get pargs ':organization))
          (email (plist-get pargs ':email))
          (phone (short-phone-plist-to-bbdb 
                  (json-to-plist (plist-get pargs ':phone))))
          (address (address-plist-to-bbdb
                    (json-to-plist (plist-get pargs ':address)))))
      (progn
        (message "creating BBDB record with lastName: %s, firstName: %s" 
                 lastName firstName)
        (bbdb-create-internal (cons firstName lastName) nil nil
                              organization email phone address)
        (save-some-buffers 't)))))

(defun bbdbcli-update (args) 
  (let ((pargs (cdr args)))
    (let ((firstName (plist-get pargs ':firstName))
          (lastName (plist-get pargs ':lastName))
          (hash (plist-get pargs ':hash)))
      (progn
        (message "updating BBDB record with lastName: %s, firstName: %s, hash: %s" 
                 lastName firstName hash)
        (update-record firstName lastName hash pargs)))))

(defun bbdbcli-delete (args)  
  (let ((posargs (car args)))
    (let ((firstname (elt posargs 0)) 
          (lastname (elt posargs 1))
          (hash (elt posargs 2))) 
      (progn 
        (message "deleting BBDB record with lastname: %s, firstname: %s, hash: %s" 
                 lastname firstname hash)
        (delete-record firstname lastname hash)))))

(defun bbdbcli-fetch (args)
  (let ((posargs (car args)))
    (let ((text (elt posargs 0)))
      (progn 
        (message "fetching BBDB record with text: %s" text)
        (fetch-contacts-json-batch text)))))

(defun add-opt (name value args)
  (cons (car args) (plist-put (cdr args) name value)))

(defun add-opt-list (name value args)
  (let ((options (cdr args)))
    (cons (car args) 
          (plist-put options
                     name (cons value (plist-get options name))))))

(defun add-pos (a args)
  (cons (cons a (car args)) (cdr args)))

(defun parse-args (args)
  "Convert a list of command line arguments into a pair.  The
  first element is the list of positional arguments.  The next
  argument is a plist of the optional parameters"
  (if (eq args 'nil)
      '(nil)
    (let ((a (car args)))
      (cond 
       ((equal "--firstName" a) (add-opt (intern ":firstName") 
                                         (cadr args) (parse-args (cddr args))))
       ((equal "--lastName" a) (add-opt (intern ":lastName") 
                                        (cadr args) (parse-args (cddr args))))
       ((equal "--phone" a) (add-opt-list (intern ":phone") 
                                          (cadr args) (parse-args (cddr args))))
       ((equal "--email" a) (add-opt-list (intern ":email")
                                          (cadr args) (parse-args (cddr args))))
       ((equal "--organization" a) (add-opt-list (intern ":organization") 
                                                 (cadr args) (parse-args (cddr args))))
       ((equal "--address" a) (add-opt-list (intern ":address") 
                                            (cadr args) (parse-args (cddr args))))
       ((equal "--hash" a) (add-opt (intern ":hash") 
                                    (cadr args) (parse-args (cddr args))))

       ('t (add-pos a (parse-args (cdr args))))))))
  
(defun main ()
  "run the main program"
  (let ((action (car argv)) (args (cdr argv)))
    (progn
      (cond
       ((equal action "create") (bbdbcli-create (parse-args args)))
       ((equal action "delete") (bbdbcli-delete (parse-args args)))
       ((equal action "update") (bbdbcli-update (parse-args args)))
       ((equal action "fetch") (bbdbcli-fetch (parse-args args)))
       ('t (message "unknown action %s" action))))))

(main)
